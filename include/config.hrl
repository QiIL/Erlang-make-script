-ifndef(COFNFIG_HRL).
-define(CONFIG_HRL, config_hrl).

-define(CFG_H, find(K) -> case K of )
-define(C(K, V), find(K) -> V; ).
-define(CFG_END find(_) -> undefined
end )
-endif.