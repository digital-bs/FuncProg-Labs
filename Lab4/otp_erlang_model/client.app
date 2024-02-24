{application, client,
    [{description, "Client proc"},
        {vsn, "1.0"},
        {modules, [client_app, client_lib, client_server, client_event, client_supervisor]},
        {registered, [ client_server, client_event, client_supervisor]},
        {applications, [kernel, stdlib]},
        {mod, {client_app,[]}},
        {start_phases, []}]}.