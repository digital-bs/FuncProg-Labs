{application, bankomat,
    [{description, "bankomat proc"},
        {vsn, "1.0"},
        {modules, [bankomat_app, bankomat_lib, bankomat_server, bankomat_event, bankomat_supervisor]},
        {registered, [bankomat_server, bankomat_event, bankomat_supervisor]},
        {applications, [kernel, stdlib]},
        {mod, {bankomat_app,[]}},
        {start_phases, []}]}.