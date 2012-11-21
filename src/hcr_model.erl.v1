-module(hcr_model).

-export([new/0, perform_action/1]).
-export([v1/1]).

%% Represent model as tagged proplist, to able to have
%% somewhat useful -specs and make sure we are dealing
%% with the right model in the accessors.
%%
%% We could use the type aka primary key as an additional
%% element in the tuple, e.g. {g8_tree, pine, [Property]}
new() ->
    Templ = {model, test, []},
    %% set defaults
    Fs = [ fun(M) -> v1(M, v1(M)) end
         ],
    lists:foldl(fun (F, M) -> F(M) end, Templ, Fs).

perform_action(M) -> v1(M, v1(M) + hcr_config:incr1(M)).

%% ===================================================================
%% Accessors
%% ===================================================================

v1(M)    -> (hcr_accessors:getter(model, v1))(M).
v1(M, V) -> (hcr_accessors:setter(model, v1))(M, V).
