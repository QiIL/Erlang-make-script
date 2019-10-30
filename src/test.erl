-module(test).
-behaviour(gen_server).
-record(state,{}).
%% API
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(Call, _From, State) ->
    {reply, Call, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', PID, shutdown}, State) ->
    List = erlang:pid_to_list(PID),
    case string:str(List, "<0.") of
        0 ->
            {noreply, State};
        _ ->
            {stop, normal, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.