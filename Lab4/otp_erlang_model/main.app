{application, main,
    [{description, "Main proc"},
        {vsn, "1.0"},
        {modules, [main_app, main_lib, main_server, main_event, main_supervisor]},
        {registered, [main_server, main_event, main_supervisor]},
        {applications, [kernel, stdlib]},
        {mod, {main_app,[]}},
        {start_phases, []}]}.