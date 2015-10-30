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
                                        {port, 9090},
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
* __username__ - username for authorization on InfluxDB. __Not implemented yet__.
* __password__ - password for authorization on InfluxDB. __Not implemented yet__.
* __timestamping__ - enable timestamping, `false` by default.
* __tags__ - list of default tags for each data point. Here always is `host` which local host name by default. 

Timestamping is by default done by influxdb itself. To enable `timestamping` with the reporter you can use `true` or `{true, Precision}` where `Precision` is a unit taken from `[n,u,ms,s,m,h]`. The default unit is `u`.

There is possibility to extend the default tags list which only has `host` by default. 
When you describe subscriptions list you can add tags to `Extra`. For example:

```erlang
{exometer, 
    {subscriptions, [
         {exometer_report_influxdb, [erlang, memory], total, 5000, true, [{tag, <<"value">>}]},
    ]}
}.

```

# TODO

* UDP support
* Auth support
* Tests 
* Reconfiguration on runtime
