-module(exometer_report_influxdb).

-behaviour(exometer_report).

%% gen_server callbacks
-export([exometer_init/1,
         exometer_info/2,
         exometer_cast/2,
         exometer_call/3,
         exometer_report/5,
         exometer_subscribe/5,
         exometer_unsubscribe/4,
         exometer_newentry/2,
         exometer_setopts/4,
         exometer_terminate/2]).


-define(DEFAULT_HOST, <<"127.0.0.1">>).
-define(DEFAULT_DB, <<"exometer">>).
-define(DEFAULT_PROTOCOL, http).
-define(DEFAULT_PORT, 8086).
-define(DEFAULT_USERNAME, undefined).
-define(DEFAULT_PASSWORD, undefined).
-define(DEFAULT_PRECISION, u).
-define(DEFAULT_TIMESTAMP_OPT, false).

-define(VALID_PRECISIONS, [n, u, ms, s, m, h]).

-type options() :: [{atom(), any()}].
-type value() :: any().
-type callback_result() :: {ok, state()} | any().
-type precision() :: n | u | ms | s | m | h.
-type protocol() :: http | udp.

-record(state, {protocol :: protocol(),
                db :: binary(),
                username :: undefined | binary(), % for http
                password :: undefined | binary(), % for http
                timestamping :: boolean(),
                precision :: precision(),
                tags :: map(),
                metrics :: map(),
                connection :: gen_udp:socket() | reference()}).
-type state() :: #state{}.


%% ===================================================================
%% Public API
%% ===================================================================
-spec exometer_init(options()) -> callback_result().
exometer_init(Opts) ->
    Host = get_opt(host, Opts, ?DEFAULT_HOST),
    Protocol = get_opt(protocol, Opts, ?DEFAULT_PROTOCOL),
    Port = get_opt(port, Opts, ?DEFAULT_PORT),
    DB = get_opt(db, Opts, ?DEFAULT_DB),
    Username = get_opt(username, Opts, ?DEFAULT_USERNAME),
    Password = get_opt(password, Opts, ?DEFAULT_PASSWORD),
    TimestampOpt = get_opt(timestamping, Opts, ?DEFAULT_TIMESTAMP_OPT),
    {Timestamping, Precision} = evaluate_timestamp_opt(TimestampOpt),
    Tags = [{key(Key), Value} || {Key, Value} <- get_opt(tags, Opts, [])],
    {ok, Connection} = connect(Protocol, Host, Port),
    {ok, #state{protocol = Protocol, 
                db = DB, 
                username = Username,
                password = Password,
                timestamping = Timestamping,
                precision = Precision,
                tags = merge_tags([{<<"host">>, net_adm:localhost()}], Tags), 
                metrics = maps:new(),
                connection = Connection}}.

-spec exometer_report(exometer_report:metric(),
                      exometer_report:datapoint(),
                      exometer_report:extra(),
                      value(),
                      state()) -> callback_result().
exometer_report(Metric, DataPoint, _Extra, Value, 
                #state{metrics = Metrics} = State) ->
    {MetricName, Tags} = maps:get(Metric, Metrics),
    Packet = make_packet(MetricName, Tags, maps:from_list([{DataPoint, Value}]),
                         State#state.timestamping, State#state.precision),
    send(Packet, State).

-spec exometer_subscribe(exometer_report:metric(), 
                         exometer_report:datapoint(),
                         exometer_report:interval(), 
                         exometer_report:extra(), 
                         state()) -> callback_result().
exometer_subscribe(Metric, _DataPoint, _Interval, TagOpts,
                   #state{metrics=Metrics, tags=DefaultTags} = State) ->
    {MetricName, SubscriberTags} = evaluate_subscription_tags(Metric, TagOpts),
    Tags = merge_tags(DefaultTags, SubscriberTags),
    case MetricName of
        [] -> exit({invalid_metric_name, MetricName});
        _  ->
            NewMetrics = maps:put(Metric, {MetricName, Tags}, Metrics),
            {ok, State#state{metrics = NewMetrics}}
    end.

-spec exometer_unsubscribe(exometer_report:metric(), 
                           exometer_report:datapoint(),
                           exometer_report:extra(), 
                           state()) -> callback_result().
exometer_unsubscribe(Metric, _DataPoint, _Extra,
                     #state{metrics = Metrics} = State) ->
    {ok, State#state{metrics = maps:remove(Metric, Metrics)}}.

-spec exometer_call(any(), pid(), state()) ->
    {reply, any(), state()} | {noreply, state()} | any().
exometer_call(_Unknown, _From, State) ->
    {ok, State}.

-spec exometer_cast(any(), state()) -> {noreply, state()} | any().
exometer_cast(_Unknown, State) ->
    {ok, State}.

-spec exometer_info(any(), state()) -> callback_result().
exometer_info(_Unknown, State) ->
    {ok, State}.

-spec exometer_newentry(exometer:entry(), state()) -> callback_result().
exometer_newentry(_Entry, State) ->
    {ok, State}.

-spec exometer_setopts(exometer:entry(), options(),
                       exometer:status(), state()) -> callback_result().
exometer_setopts(_Metric, _Options, _Status, State) ->
    {ok, State}.

-spec exometer_terminate(any(), state()) -> any().
exometer_terminate(_, _) ->
    ignore.


%% ===================================================================
%% Internal functions
%% ===================================================================
-spec connect(protocol(), binary(), integer()) -> 
    {ok, pid() | reference()} | {error, term()}.
connect(http, Host, Port) ->
    {ok, _} = application:ensure_all_started(hackney),
    hackney:connect(hackney_tcp_transport, Host, Port, []);
connect(udp, _, _) -> {error, {udp, not_implemented}};
connect(Protocol, _, _) -> {error, {Protocol, not_supported}}.

-spec send(binary() | list(), state()) -> 
    {ok, state()} | {error, term()}.
send(Packet, #state{protocol = http, connection= Connection,
                    precision = Precision, db = DB,
                    timestamping = Timestamping} = State) ->
    QsVals = case Timestamping of
                 false -> [{<<"db">>, DB}];
                 true  -> [{<<"db">>, DB}, {<<"precision">>, Precision}]
             end,
    Url = hackney_url:make_url(<<"/">>, <<"write">>, QsVals),
    Req = {post, Url, [], Packet},
    case hackney:send_request(Connection, Req) of
        {ok, 204, _, Ref} -> 
            hackney:body(Ref),
            {ok, State};
        {ok, _, _Headers, Ref} ->
            {ok, Body} = hackney:body(Ref),
            {error, Body};
        {error, _} = Error -> Error
    end;
send(_, #state{protocol = udp}) -> {error, {udp, not_implemented}};
send(_, #state{protocol = Protocol}) -> {error, {Protocol, not_supported}}.

-spec merge_tags(list() | map(), list() | map()) -> map().
merge_tags(Tags, AdditionalTags) when is_list(Tags) ->
    merge_tags(maps:from_list(Tags), AdditionalTags); 
merge_tags(Tags, AdditionalTags) when is_list(AdditionalTags) ->
    merge_tags(Tags, maps:from_list(AdditionalTags)); 
merge_tags(Tags, AdditionalTags) when not is_map(AdditionalTags) -> Tags;
merge_tags(Tags, AdditionalTags) -> maps:merge(Tags, AdditionalTags).

-spec get_opt(atom(), list(), any()) -> any().
get_opt(K, Opts, Default) ->
    exometer_util:get_opt(K, Opts, Default).

%% LINE PROTOCOL 
-define(SEP(V), case V of <<>> -> <<>>; [] -> <<>>; _ -> <<$,>> end).

-spec microsecs() -> integer().
microsecs() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 * 1000000 + Secs * 1000000 + MicroSecs.

-spec convert_time_unit(integer(), erlang:time_unit() | minutes | hours) -> 
    integer().
convert_time_unit(MicroSecs, hours) -> 
    round(convert_time_unit(MicroSecs, minutes) / 60);
convert_time_unit(MicroSecs, minutes) -> 
    round(convert_time_unit(MicroSecs, seconds) / 60);
convert_time_unit(MicroSecs, seconds) -> 
    round(convert_time_unit(MicroSecs, milli_seconds) / 1000);
convert_time_unit(MicroSecs, milli_seconds) -> 
    round(MicroSecs / 1000);
convert_time_unit(MicroSecs, nano_seconds) -> 
    MicroSecs * 1000.

-spec unix_time(precision() | undefined) -> integer() | undefined.
unix_time(n)  -> convert_time_unit(microsecs(), nano_seconds);
unix_time(u)  -> microsecs();
unix_time(ms) -> convert_time_unit(microsecs(), milli_seconds);
unix_time(s)  -> convert_time_unit(microsecs(), seconds);
unix_time(m)  -> convert_time_unit(microsecs(), minutes);
unix_time(h)  -> convert_time_unit(microsecs(), hours);
unix_time(_)  -> undefined.

-spec metric_to_string(list()) -> string().
metric_to_string([Final]) -> metric_elem_to_list(Final);
metric_to_string([H | T]) ->
    metric_elem_to_list(H) ++ "_" ++ metric_to_string(T).

-spec metric_elem_to_list(atom() | string() | integer()) -> string().
metric_elem_to_list(E) when is_atom(E) -> atom_to_list(E);
metric_elem_to_list(E) when is_list(E) -> E;
metric_elem_to_list(E) when is_integer(E) -> integer_to_list(E).

-spec name(exometer_report:metric()) -> binary().
name(Metric) -> iolist_to_binary(metric_to_string(Metric)).

-spec key(integer() | atom() | list() | binary()) -> binary().
key(K) when is_integer(K) -> key(integer_to_binary(K));
key(K) when is_list(K) -> key(list_to_binary(K));
key(K) when is_atom(K) -> key(atom_to_binary(K, utf8));
key(K) -> 
    binary:replace(K, [<<" ">>, <<$,>>, <<$=>>], <<$\\>>,
                   [global, {insert_replaced, 1}]).

-spec value(any()) -> binary() | list().
value(V) when is_integer(V) -> [integer_to_binary(V), $i];
value(V) when is_float(V) -> float_to_binary(V);
value(V) when is_atom(V) -> value(atom_to_binary(V, utf8));
value(V) when is_list(V) -> value(list_to_binary(V));
value(V) when is_binary(V) ->
    [$", binary:replace(V, <<$">>, <<$\\, $">>, [global]), $"].

-spec flatten_fields(list()) -> list().
flatten_fields(Fields) ->
    maps:fold(fun(K, V, Acc) ->
                  [Acc, ?SEP(Acc), key(K), $=, value(V)]
              end, <<>>, Fields).

-spec flatten_tags(map() | list()) -> list().
flatten_tags(Tags) when is_map(Tags) -> flatten_tags(maps:to_list(Tags));
flatten_tags(Tags) ->
    lists:foldl(fun({K, V}, Acc) ->
                    [Acc, ?SEP(Acc), key(K), $=, key(V)]
                end, [], lists:keysort(1, Tags)).

-spec make_packet(exometer_report:metric(), map() | list(),
                  list(), boolean(), precision()) -> list().
make_packet(Measurement, Tags, Fields, Timestamping, Precision) ->
    BinaryTags = flatten_tags(Tags),
    BinaryFields = flatten_fields(Fields),
    case Timestamping of
        false ->
            [name(Measurement), ?SEP(BinaryTags), BinaryTags, " ",
            BinaryFields, " "];
        true ->
            [name(Measurement), ?SEP(BinaryTags), BinaryTags, " ",
            BinaryFields, " ", integer_to_binary(unix_time(Precision))]
    end.

-spec evaluate_timestamp_opt({boolean(), precision()} | boolean())
                            -> {boolean(), precision()}.
evaluate_timestamp_opt({Term, Precision}) when is_boolean(Term) ->
    case lists:member(Precision, ?VALID_PRECISIONS) of
        true -> {Term, Precision};
        false -> exit(invalid_precision)
    end;
evaluate_timestamp_opt(Term) when is_boolean(Term) ->
    {Term, ?DEFAULT_PRECISION};
evaluate_timestamp_opt(_) ->
    exit(invalid_timestamp_option).

-spec del_indices(list(), [integer()]) -> list().
del_indices(List, Indices) ->
    SortedIndices = lists:reverse(lists:usort(Indices)),
    case length(SortedIndices) == length(Indices) of
        true -> del_indices1(List, SortedIndices);
        false -> exit({invalid_indices, Indices})
    end.

-spec del_indices1(list(), [integer()]) -> list().
del_indices1(List, []) -> List;
del_indices1([], Indices = [ _Index | _Indices1 ]) -> exit({too_many_indices, Indices});
del_indices1(List, [Index | Indices]) when length(List) >= Index, Index > 0 ->
    {L1, [_|L2]} = lists:split(Index-1, List),
    del_indices1(L1 ++ L2, Indices);
del_indices1(_List, Indices) ->
    exit({invalid_indices, Indices}).

-spec evaluate_subscription_tags(list(), undefined | [{atom(), value()}]) -> 
    {list(), map()}.
evaluate_subscription_tags(Metric, undefined) -> 
    evaluate_subscription_tags(Metric, []);
evaluate_subscription_tags(Metric, Tags) ->
    evaluate_subscription_tags(Metric, Tags, [], []).

-spec evaluate_subscription_tags(list(), [{atom(), value()}], [{atom(), value()}],
                                 [integer()]) -> {list(), map()}.
evaluate_subscription_tags(Metric, [], TagAcc, PosAcc) ->
    MetricName = del_indices(Metric, PosAcc),
    {MetricName, maps:from_list(TagAcc)};
evaluate_subscription_tags(Metric, [{Key, {from_name, Pos}} | TagOpts], TagAcc, PosAcc)
    when is_number(Pos), length(Metric) >= Pos, Pos > 0 ->
    NewTagAcc = TagAcc ++ [{Key, lists:nth(Pos, Metric)}],
    NewPosAcc = PosAcc ++ [Pos],
    evaluate_subscription_tags(Metric, TagOpts, NewTagAcc, NewPosAcc);
evaluate_subscription_tags(Metric, [TagOpt = {Key, {from_name, Name}} | TagOpts],
    TagAcc, PosAcc) ->
    case string:str(Metric, [Name]) of
        0     -> exit({invalid_tag_option, TagOpt});
        Index ->
            NewTagAcc = TagAcc ++ [{Key, Name}],
            NewPosAcc = PosAcc ++ [Index],
            evaluate_subscription_tags(Metric, TagOpts, NewTagAcc, NewPosAcc)
    end;
evaluate_subscription_tags(Metric, [Tag = {_Key, _Value} | Tags], TagAcc, PosAcc) ->
    evaluate_subscription_tags(Metric, Tags, TagAcc ++ [Tag], PosAcc);
evaluate_subscription_tags(_Metric, [Tag | _] , _TagAcc, _PosAcc) ->
    exit({invalid_tag_option, Tag}).
