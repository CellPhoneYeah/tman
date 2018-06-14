{
    application, 
    tman,
    [
        {description, "yxf tcp manager"},
        {mod, {tman_app, []}},
        {applications, []},
        {modules, [tman_sup, tman_server]}
    ]
}.
