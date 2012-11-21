-module(hcr_config).

-export([incr1/1]).

incr1(test) -> 1;
incr1({model, Type, _}) -> incr1(Type).
