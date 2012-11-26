-module(hcr_config).
-include("hcr.hrl").

-export([incr1/1]).

-export_type([type/0]).
-type type() :: 'test'.

-spec incr1(type() | model()) -> non_neg_integer() | no_return().
incr1(test) -> 1;
incr1({hcr_model, Type, _}) -> incr1(Type);
incr1(A) -> erlang:throw({hcr, no_config, [?MODULE, incr1, A]}).
