-module(hcr_server).
-behaviour(gen_server).
-include("hcr.hrl").

-export([start_link/0, perform_action/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    timer:apply_interval(2000, ?MODULE, perform_action, []),
    {ok, Pid}.

perform_action() ->
    gen_server:cast(?MODULE, perform_action).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) -> {ok, [{model, hcr_model:new(test)}]}.

handle_call(_Req, _From, S) -> {noreply, S}.

-spec handle_cast(perform_action, state()) -> {noreply, state()}.
handle_cast(perform_action, [{model, M}]) ->
    M1 = hcr_model:perform_action(M),
    error_logger:info_msg("Updated model from ~p~n"
                          "              to   ~p~n", [M, M1]),
    {noreply, [{model, M1}]}.

handle_info(_Info, S) -> {noreply, S}.

terminate(_Reason, _S) -> ok.

code_change(_OldVsn, S, _Extra) -> {ok, S}.
