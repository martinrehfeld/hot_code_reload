-module(hcr_accessors).

-export([getter/3, setter/2]).
-export([get/3, set/3]).


%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Generate a getter for a given model, propery and default value
getter(Tag, Name, Default) ->
    fun({T, _, P}) when T =:= Tag ->
            ?MODULE:get(Name, P, Default)
    end.

%% @doc Generate a setter for a given model and propery
setter(Tag, Name) ->
    fun({T, Type, P}, V) when T =:= Tag ->
            {T, Type, ?MODULE:set(Name, P, V)}
    end.


get(Name, P, Default) -> proplists:get_value(Name, P, Default).
set(Name, P, V)       -> lists:keystore(Name, 1, P, {Name, V}).
