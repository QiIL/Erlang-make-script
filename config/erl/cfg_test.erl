-module(cfg_test).

%% API
-export([
    find/1
]).

find(1)->2;
find(3)->3;
find(aaa)->3;
find(_)->undefined.


