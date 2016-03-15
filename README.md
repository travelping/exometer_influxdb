# Exometer InfluxDB reporter [![Build Status](https://travis-ci.org/travelping/exometer_influxdb.svg)](https://travis-ci.org/travelping/exometer_influxdb)

This reporter pushes data to [InfluxDB](https://influxdb.com/index.html).

## Usage

1. Add exometer_influxdb to your list of dependencies in rebar.config:

    ```erlang
    {deps, [
        {exometer_influxdb, ".*", {git, "https://github.com/travelping/exometer_influxdb.git", "master"}}
    ]}.
    ```

2. Ensure exometer_influxdb is started before your application:

    ```erlang
    {applications, [exometer_influxdb]}.
    ```

3. Configure it:

    ```erlang
    {exometer, 
        {reporters, [
            {exometer_report_influxdb, [{protocol, http}, 
                                        {host, <<"localhost">>},
                                        {port, 8086},
                                        {db, <<"exometer">>},
                                        {tags, [{region, ru}]}]}
        ]}
    }.
    ```

Available options:

* __host__ - InfluxDB host. `127.0.0.1` by default.
* __protocol__ - `http` or `udp` for operating with InfluxDB. `http` by default.
* __port__ - InfluxDB port. `8086` by default.
* __db__ - Database on InfluxDB for writing data. `exometer` by default.
* __username__ - Username for authorization on InfluxDB.
* __password__ - Password for authorization on InfluxDB.
* __timestamping__ - Enable timestamping, `false` by default. To enable `timestamping` with the reporter you can use `true` or `{true, Precision}` where `Precision` is a unit taken from `[n,u,ms,s,m,h]`. The default unit is `u`.
* __batch_window_size__ - set window size in ms for batch sending. This means the reporter will collect measurements within this interval and send all measurements in one packet. `0` by default.

The following options can be set globally in the reporter config or locally in a specific subscription. The latter case overwrites the first.

* __tags__ - List of tags for each time series. The `host` is automatically included here.
* __series_name__ - The name of a time series visible within the `FROM` field. By default this is set to the concatenated elements of the exometer id. Caution: If set in the global reporter config then every time series will have this name.
* __formatting__ - Formatting options to alter the appearance of a series name or tags.

### Subscription examples:

```erlang
{exometer, 
    {subscriptions, [
         {exometer_report_influxdb, [erlang, memory], total, 5000, true, [{tags, {tag, value}}]},
    ]}
}.
```

By default the in InfluxDB visible name of the metric is derived from the exometer id: Here `[erlang, memory]` is translated to `erlang_memory`. 
It is possible to remove an item from this list by naming itself or its position with the `from_name` keyword. A removed element is then used as tag value:

```erlang
exometer_report:subscribe(exometer_report_influxdb, [erlang, memory], total, 5000, true, [{tags, [{tag, {from_name, 2}}]}]).
```

This will result in a name `erlang` with the tag pair `{tag, memory}` (plus the default pair `{host, Host}`). To disable the removal of elements in the series name you can set:

```erlang
{formatting, [{purge, [{all_from_name, false}]}]}
```

Further it might be handy to remove e.g. `undefined` tag keys or values. This can be done via:

```erlang
{formatting, [{purge, [{tag_keys, undefined}, {tag_values, undefined}]}]}
```


# TODO

* Reconfiguration on runtime
* Enhance the formatting options (e.g. concatenation chars, format strings, etc.)
