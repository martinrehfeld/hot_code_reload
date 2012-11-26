-module(hcr_accessors).
-include("hcr.hrl").

%% Activate accessor generation in your model with
%%     -compile({parse_transform, hcr_accessors}).
%%
%% Use
%%     ?ATTR_READER([{prop1, defaultvalue1}, ...]).
%%     ?ATTR_ACCESSOR([{prop1, defaultvalue1}, ...]).
%%     ?ATTR_WRITER([prop1, ...]).
%% to generate getters and/or setters.

-export([parse_transform/2]).

-export([init_defaults/3]).

-record(state, { expected = any
               , exports  = []
               , readers  = []
               , writers  = []
               , eof_line = 9999
               , module
               }).


%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Initialize given properties with its defaults
%% when they are not already set
-spec init_defaults(module(), M, [atom()]) -> M when M :: any_model().
init_defaults(Mod, State, Properties) ->
    Accessors = [ {fun Mod:P/1, fun Mod:P/2} || P <- Properties],
    Fs = [ fun(S) -> W(S, R(S)) end || {R, W} <- Accessors ],
    lists:foldl(fun (F, S) -> F(S) end, State, Fs).


parse_transform(Forms, Options) ->
    {Forms1, S} =
        parse_trans:transform(fun xform_fun/4, #state{}, Forms, Options),

    Forms2 = lists:foldl(fun ({M,A}, Acc) ->
                                 parse_trans:export_function(M,A,Acc)
                         end, Forms1, S#state.exports),

    Forms3 = parse_trans:do_insert_forms(
        below,
        [ reader_form(S#state.module, Name, Default, S#state.eof_line)
            || {Name, Default} <- S#state.readers ],
        Forms2,
        parse_trans:initial_context(Forms2, [])
    ),

    Forms4 = parse_trans:do_insert_forms(
        below,
        [ writer_form(S#state.module, Name, S#state.eof_line)
            || Name <- S#state.writers ],
        Forms3,
        parse_trans:initial_context(Forms3, [])
    ),
    Result = parse_trans:revert(Forms4),
    %% io:format("~n~n~nREWRITTEN~n~p~n", [Result]),
    Result.


%% ===================================================================
%% Internal functions
%% ===================================================================

xform_fun(eof_marker, {eof, Line}=Form, Ctx, S) ->
    %% io:format("~p~n~p~n~n", [eof_marker, {Form, Ctx, S}]),
    {Form, true, S#state{eof_line = Line,
                         module   = parse_trans:context(module, Ctx)}};
xform_fun(Type, Form, Ctx, S) ->
    %% io:format("~p~n~p~n~n", [Type, {Form, Ctx, S}]),
    case {parse_trans:context(function, Ctx), parse_trans:context(arity, Ctx)} of
        {attr_reader, 0}   -> parse_reader(Type, Form, Ctx, S);
        {attr_writer, 0}   -> parse_writer(Type, Form, Ctx, S);
        {attr_accessor, 0} -> parse_accessor(Type, Form, Ctx, S);
        {_, _}             -> {Form, true, S#state{expected=any}}
    end.


parse_reader(atom, Form, _Ctx, #state{expected=any, exports=Exports}=S) ->
    {Form, true, S#state{expected=clause,
                         exports = [{attr_reader, 0} | Exports]}};
parse_reader(clause, Form, _Ctx, #state{expected=clause}=S) ->
    {Form, true, S#state{expected=list}};
parse_reader(list, Form, _Ctx, #state{expected=list}=S) ->
    {Form, true, S#state{expected=tuple}};
parse_reader(tuple, {tuple, _, [{atom, _, Name}, Default]}=Form, _Ctx,
             #state{expected=tuple, exports=Exports, readers=Readers}=S) ->
    {Form, false, S#state{readers  = [{Name, Default} | Readers],
                          exports  = [{Name, 1} | Exports],
                          expected = list}};
parse_reader(Type, Form, _Ctx, #state{expected=Expected}) ->
    parse_trans:error("Invalid attr_reader list", Form,
        [{form, Form}, {got, Type}, {expected, Expected}]).


parse_writer(atom, Form, _Ctx, #state{expected=any, exports=Exports}=S) ->
    {Form, true, S#state{expected = clause,
                         exports  = [{attr_writer, 0} | Exports]}};
parse_writer(clause, Form, _Ctx, #state{expected=clause}=S) ->
    {Form, true, S#state{expected=list}};
parse_writer(list, Form, _Ctx, #state{expected=list}=S) ->
    {Form, true, S#state{expected=atom}};
parse_writer(atom, {atom, _, Name}=Form, _Ctx,
             #state{expected=atom, exports=Exports, writers=Writers}=S) ->
    {Form, false, S#state{writers  = [Name | Writers],
                          exports  = [{Name, 2} | Exports],
                          expected = atom}};
parse_writer(Type, Form, _Ctx, #state{expected=Expected}) ->
    parse_trans:error("Invalid attr_writer list", Form,
        [{form, Form}, {got, Type}, {expected, Expected}]).


parse_accessor(atom, Form, _Ctx, #state{expected=any, exports=Exports}=S) ->
    {Form, true, S#state{expected = clause,
                         exports  = [{attr_accessor, 0} | Exports]}};
parse_accessor(clause, Form, _Ctx, #state{expected=clause}=S) ->
    {Form, true, S#state{expected=list}};
parse_accessor(list, Form, _Ctx, #state{expected=list}=S) ->
    {Form, true, S#state{expected=tuple}};
parse_accessor(tuple, {tuple, _, [{atom, _, Name}, Default]}=Form, _Ctx,
             #state{expected=tuple, exports=Exports,
                    readers=Readers, writers=Writers}=S) ->
    {Form, false, S#state{readers  = [{Name, Default} | Readers],
                          writers  = [Name | Writers],
                          exports  = [{Name, 1} | [{Name, 2} | Exports]],
                          expected = list}};
parse_accessor(Type, Form, _Ctx, #state{expected=Expected}) ->
    parse_trans:error("Invalid attr_accessor list", Form,
        [{form, Form}, {got, Type}, {expected, Expected}]).


%% @doc This is AST equivalent of getter/3's returned Fun
reader_form(Tag, Name, Default, Line)
  when is_atom(Tag) andalso is_atom(Name) andalso is_integer(Line) ->
    {function, Line, Name, 1,
       [{clause,Line,
         [{tuple,Line,[{atom,Line,Tag},{var,Line,'_'},{var,Line,'P'}]}],
         [],
         [{call,Line,{remote,Line,{atom,Line,proplists},{atom,Line,get_value}},
           [{atom,Line,Name},
            {var,Line,'P'},
            Default
           ]}]}]
    }.

%% @doc This is AST equivalent of setter/2's returned Fun
writer_form(Tag, Name, Line)
  when is_atom(Tag) andalso is_atom(Name) andalso is_integer(Line) ->
    {function, Line, Name, 2,
       [{clause,Line,
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
             ]}]}]}]
    }.
