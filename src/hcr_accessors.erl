-module(hcr_accessors).

-export([getter/2, setter/2]).


getter(Tag, Name) ->
    fun({T, _, P}) when T =:= Tag ->
            proplists:get_value(Name, P, 1)
    end.

setter(Tag, Name) ->
    fun({T, Type, P}, V) when T =:= Tag ->
            {T, Type, lists:keystore(Name, 1, P, {Name, V})}
    end.
