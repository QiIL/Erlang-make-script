{application, test_app, [
    {description, ""},
    {vsn, "1.0.0"},
    {registered, [test_app]},
    {applications, [
        kernel,
        stdlib
    ]},
    {modules, [test_app, test_sup]},
    {mod, {test_app, []}},
    {env, []}
]}.