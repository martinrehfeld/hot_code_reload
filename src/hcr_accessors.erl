-module(hcr_accessors).

%% Activate optimized accessors in your model with
%%     -compile({parse_transform, hcr_accessors}).
%%
%% The parse transformation will look for getters/setters like this
%% and will perform the function generation at compile time:
%%     v1(M)    -> (hcr_accessors:getter(model, v1, 1))(M).
%%     v1(M, V) -> (hcr_accessors:setter(model, v1))(M, V).
-export([parse_transform/2]).

-export([getter/3, setter/2]).


%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Generate a getter for a given model, propery and default value
getter(Tag, Name, Default) ->
    fun({T, _, P}) when T =:= Tag ->
            proplists:get_value(Name, P, Default)
    end.

%% @doc Generate a setter for a given model and propery
setter(Tag, Name) ->
    fun({T, Type, P}, V) when T =:= Tag ->
            {T, Type, lists:keystore(Name, 1, P, {Name,V})}
    end.


parse_transform(Ast, _Options) ->
    walk_ast([], Ast).

%% ===================================================================
%% Internal functions
%% ===================================================================

walk_ast(Acc, []) ->
    lists:reverse(Acc);
walk_ast(Acc, [{function,_,_,1,Clauses}=H|T]) ->
    Form = setelement(5, H, [maybe_transform_getter(C) || C <- Clauses]),
    walk_ast([Form|Acc], T);
walk_ast(Acc, [{function,_,_,2,Clauses}=H|T]) ->
    Form = setelement(5, H, [maybe_transform_setter(C) || C <- Clauses]),
    walk_ast([Form|Acc], T);
walk_ast(Acc, [H|T]) ->
    walk_ast([H|Acc], T).

maybe_transform_getter(
    {clause,Line,
     [{var,_,_VarM}],
     [],
     [{call,_,
       {call,_,
        {remote,_,{atom,_,hcr_accessors},{atom,_,getter}},
        [{atom,_,Tag},{atom,_,Name},{DefaultType,_,DefaultValue}]
       },
       [{var,_,_VarM}]}]}) ->
    %% io:format("Optimizing getter ~p~n", [{Tag, Name, DefaultValue}]),
    getter_clause(Tag, Name, DefaultType, DefaultValue, Line);
maybe_transform_getter(Cs) ->
    Cs.

maybe_transform_setter(
    {clause,Line,
     [{var,_,_VarM},{var,_,_VarV}],
     [],
     [{call,_,
       {call,_,
        {remote,_,{atom,_,hcr_accessors},{atom,_,setter}},
        [{atom,_,Tag},{atom,_,Name}]
       },
       [{var,_,_VarM},{var,_,_VarV}]}]}) ->
    %% io:format("Optimizing setter ~p~n", [{Tag, Name}]),
    setter_clause(Tag, Name, Line);
maybe_transform_setter(Cs) ->
    Cs.

%% @doc This is AST equivalent of getter/3's returned Fun
getter_clause(Tag, Name, DefaultType, DefaultValue, Line)
  when is_atom(Tag) andalso is_atom(Name) andalso is_integer(Line) ->
    {clause,Line,
     [{tuple,Line,[{atom,Line,Tag},{var,Line,'_'},{var,Line,'P'}]}],
     [],
     [{call,Line,{remote,Line,{atom,Line,proplists},{atom,Line,get_value}},
       [{atom,Line,Name},
        {var,Line,'P'},
        {DefaultType,Line,DefaultValue}
       ]}]}.

%% @doc This is AST equivalent of setter/2's returned Fun
setter_clause(Tag, Name, Line)
  when is_atom(Tag) andalso is_atom(Name) andalso is_integer(Line) ->
    {clause,Line,
     [{tuple,Line,[{atom,Line,Tag},{var,Line,'T'},{var,Line,'P'}]}, {var,Line,'V'}],
     [],
     [{tuple,Line,
       [{atom,Line,Tag},
        {var,Line,'T'},
        {call,Line,{remote,Line,{atom,Line,lists},{atom,Line,keystore}},
         [{atom,Line,Name},
          {integer,Line,1},
          {var,Line,'P'},
          {tuple,Line,[{atom,Line,Name},{var,Line,'V'}]}
         ]}]}]}.
