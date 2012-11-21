-module(hcr_model).

-export([new/0, perform_action/1]).
-export([v1/1]).

%% represent model as tagged proplist, to able to have
%% somewhat useful -specs and make sure we are dealing
%% with the right model in the accessors
new() -> {model, [{v1, 1}]}.

perform_action(M) -> v1(M, v1(M) + hcr_config:incr1(M)).

%% ===================================================================
%% Accessors
%% ===================================================================

v1({model, P}) -> proplists:get_value(v1, P, 1).

v1({model, P}, N) -> {model, lists:keystore(v1, 1, P, {v1, N})}.
