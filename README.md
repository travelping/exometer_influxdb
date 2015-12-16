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
* __db__ - database on InfluxDB for writing data. `exometer` by default
* __username__ - username for authorization on InfluxDB.
* __password__ - password for authorization on InfluxDB.
* __timestamping__ - enable timestamping, `false` by default.
* __tags__ - list of default tags for each data point. The `host` is automatically included here. 

Timestamping is by default done by influxdb itself. To enable `timestamping` with the reporter you can use `true` or `{true, Precision}` where `Precision` is a unit taken from `[n,u,ms,s,m,h]`. The default unit is `u`.

Besides the `tags` for the reporter initialization it is possible to add other tags via the `Extra` parameter of `exometer_report:subscribe/5` as done here with environment variables:

```erlang
{exometer, 
    {subscriptions, [
         {exometer_report_influxdb, [erlang, memory], total, 5000, true, [{tag, <<"value">>}]},
    ]}
}.
```

By default the in influxdb visible name of the metric is derived from the exometer id: here `[erlang, memory]` is translated to `erlang_memory`. It is possible to remove an item from this list by naming itself or its position with the `from_name` keyword. A removed element is then used as tag:

```erlang
exometer_report:subscribe(exometer_report_influxdb, [erlang, memory], total, 5000, true, [{tag, {from_name, 2}}]).
```

This will result in a name `erlang` with the tag pair `{tag, memory}` (plus the default pair `{host, Host}`).

# TODO

* Tests 
* Reconfiguration on runtime
