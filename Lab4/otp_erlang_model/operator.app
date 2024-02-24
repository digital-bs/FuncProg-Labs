{application, operator,
    [{description, "Operator proc"},
        {vsn, "1.0"},
        {modules, [operator_app, operator_lib, operator_server, operator_event, operator_supervisor]},
        {registered, [operator_server, operator_event, operator_supervisor]},
        {applications, [kernel, stdlib]},
        {mod, {operator_app,[]}},
        {start_phases, []}]}.