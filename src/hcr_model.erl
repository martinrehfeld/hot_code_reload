-compile({parse_transform, hcr_accessors}).
-module(hcr_model).
-include("hcr.hrl").

-export([new/0, perform_action/1]).
-export([v1/1]).

-export_type([property/0]).
-type property() :: {v1, non_neg_integer()}.


%% Represent model as tagged proplist, to able to have
%% somewhat useful -specs and make sure we are dealing
%% with the right model in the accessors.
%%
%% We could use the type aka primary key as an additional
%% element in the tuple, e.g. {g8_tree, pine, [Property]}
-spec new() -> model().
new() ->
    Templ = {model, test, []},
    %% set defaults
    Fs = [ fun(M) -> v1(M, v1(M)) end
         ],
    lists:foldl(fun (F, M) -> F(M) end, Templ, Fs).


-spec perform_action(model()) -> model().
perform_action(M) -> v1(M, v1(M) + hcr_config:incr1(M)).


%% ===================================================================
%% Accessors
%% ===================================================================

v1(M)    -> (hcr_accessors:getter(model, v1, 1))(M).
v1(M, V) -> (hcr_accessors:setter(model, v1))(M, V).
