{application, terminal,
    [{description, "Terminal proc"},
        {vsn, "1.0"},
        {modules, [terminal_app, terminal_lib, terminal_server, terminal_event, terminal_supervisor]},
        {registered, [terminal_server, terminal_event, terminal_supervisor]},
        {applications, [kernel, stdlib]},
        {mod, {terminal_app,[]}},
        {start_phases, []}]}.