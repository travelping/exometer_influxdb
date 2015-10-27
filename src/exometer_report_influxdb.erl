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

-type options() :: [{atom(), any()}].
-type value() :: any().
-type callback_result() :: {ok, state()} | any().
-type precision() :: n | u | ms | s | n | h.
-type protocol() :: http | udp.

-record(state, {protocol :: protocol(),
                db :: binary(),
                username :: undefined | binary(), % for http
                password :: undefined | binary(), % for http
                precision :: precision(),
                tags :: map(),
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
    Precision = get_opt(precision, Opts, ?DEFAULT_PRECISION),
    Tags = [{key(Key), Value} || {Key, Value} <- get_opt(tags, Opts, [])],
    {ok, Connection} = connect(Protocol, Host, Port),
    {ok, #state{protocol = Protocol, 
                db = DB, 
                username = Username,
                password = Password,
                precision = Precision,
                tags = merge_tags([{<<"host">>, net_adm:localhost()}], Tags), 
                connection = Connection}}.

-spec exometer_report(exometer_report:metric(),
                      exometer_report:datapoint(),
                      exometer_report:extra(),
                      value(),
                      state()) -> callback_result().
exometer_report(Metric, DataPoint, Extra, Value, #state{tags = Tags} = State) ->
    ExtraTags = case Extra of undefined -> []; _ -> Extra end,
    Packet = make_packet(Metric, merge_tags(Tags, ExtraTags), 
                         maps:from_list([{DataPoint, Value}]), State#state.precision),
    send(Packet, State).

-spec exometer_subscribe(exometer_report:metric(), 
                         exometer_report:datapoint(),
                         exometer_report:interval(), 
                         exometer_report:extra(), 
                         state()) -> callback_result().
exometer_subscribe(_Metric, _DataPoint, _Interval, _Extra, State) ->
    {ok, State}.

-spec exometer_unsubscribe(exometer_report:metric(), 
                           exometer_report:datapoint(),
                           exometer_report:extra(), 
                           state()) -> callback_result().
exometer_unsubscribe(_Metric, _DataPoint, _Extra, State) ->
    {ok, State}.

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
    hackney:connect(hackney_tcp_transport, Host, Port, []);
connect(udp, _, _) -> {error, {udp, not_implemented}};
connect(Protocol, _, _) -> {error, {Protocol, not_supported}}.

-spec send(binary() | list(), state()) -> 
    {ok, state()} | {error, term()}.
send(Packet, #state{protocol = http, connection= Connection,
                    precision = Precision, db = DB} = State) ->
    Url = hackney_url:make_url(<<"/">>, <<"write">>, 
                               [{<<"db">>, DB}, {<<"precision">>, Precision}]),
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
    round(convert_time_unit(MicroSecs, milliseconds) / 1000);
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
                  list(), precision()) -> list().
make_packet(Measurement, Tags, Fields, Precision) ->
    BinaryTags = flatten_tags(Tags),
    BinaryFields = flatten_fields(Fields),
    [name(Measurement), ?SEP(BinaryTags), BinaryTags, " ", BinaryFields, 
     " ", integer_to_binary(unix_time(Precision))].
