-module(exometer_influxdb_subscribe_mod).
-compile([export_all]).

subscribe([metric, test], histogram) ->
   Tags = [{tags, [{type, {from_name, 2}}]}],
   [{[metric, test], min, 1000, Tags},
    {[metric, test], max, 1000, Tags},
    {[metric, test], median, 1000, Tags}];
subscribe([metric, test1], histogram) ->
   Tags = [{tags, [{type, {from_name, 2}}]}],
   [{[metric, test1], max, 1000, Tags},
    {[metric, test1], median, 1000, Tags}];
subscribe(_, _) -> [].
