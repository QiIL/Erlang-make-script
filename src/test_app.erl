-module(test_app).
-behaviour(application).
-export([
    start/2,
    stop/1]).

start(_StartType, _StartArgs) ->
    {ok, PID} = test_sup:start_link(),
    %% start mysql_pool
    start_mysql_pool(),
    {ok, PID}.

stop(_State) ->
    ok.

start_mysql_pool() -> 
    PoolOptions  = [{size, 10}, {max_overflow, 20}],
    MySqlOptions = [{host, "127.0.0.1"}, {port, 3306}, {user, "root"}, {password, "123456"}, {database, "test"}],
    ChildSpecs = mysql_poolboy:child_spec(mysql_pool, PoolOptions, MySqlOptions),
    supervisor:start_child(test_sup, ChildSpecs).