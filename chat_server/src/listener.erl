-module(listener).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

-record(state, {listen_socket}).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {active, false}, {reuseaddr, true}]),
    io:format("Слушаем на порту ~p~n", [Port]),
    self() ! accept,
    {ok, #state{listen_socket=LSock}}.

handle_info(accept, State = #state{listen_socket=LSock}) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    io:format("Новый клиент: ~p~n", [Socket]),
    {ok, Pid} = supervisor:start_child(client_sup, [Socket]),
    ok = gen_tcp:controlling_process(Socket, Pid),
    gen_server:cast(Pid, activate),
    self() ! accept,
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.