-module(client_proc).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_info/2, handle_call/3, terminate/2, code_change/3]).

-record(state, {socket, room, nick=undefined}).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    io:format("Клиент сокет PID: ~p~n", [self()]),
    room_manager:join_room(self(), "main"),
    {ok, #state{socket=Socket, room="main"}}.

handle_info({message, Room, Msg}, State = #state{socket=Socket, room=CurRoom}) when Room =:= CurRoom ->
    send_line(Socket, io_lib:format("[~s] ~ts", [Room, Msg])),
    {noreply, State};
handle_info({message, _Room, _Msg}, State) ->
    {noreply, State};

handle_info({message, Ts, Room, Nick, Msg}, State = #state{socket=Socket, room=CurRoom}) when Room =:= CurRoom ->
    send_line(Socket, io_lib:format("[~ts] (~s) ~ts: ~ts", [Ts, Room, Nick, Msg])),
    {noreply, State};
handle_info({message, _Ts, _Room, _Nick, _Msg}, State) ->
    {noreply, State};

handle_info({tcp, Socket, Data}, State = #state{room=Room}) ->
    Str = string:trim(unicode:characters_to_list(Data, utf8)),
    case parse_command(Str) of
        {nick, Nick} ->
            {Reply, NewState} = handle_set_nick(Nick, State),
            send_line(Socket, Reply),
            inet:setopts(Socket, [{active, once}]),
            {noreply, NewState};
        {join, R} ->
            room_manager:join_room(self(), R),
            send_line(Socket, io_lib:format("Вы присоединились к комнате: ~ts", [R])),
            inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{room=R}};
        {leave, R} ->
            room_manager:leave_room(self(), R),
            send_line(Socket, io_lib:format("Вы вышли из комнаты: ~ts", [R])),
            inet:setopts(Socket, [{active, once}]),
            {noreply, case Room =:= R of true -> State#state{room="main"}; false -> State end};
        help ->
            send_line(Socket, help_text()),
            inet:setopts(Socket, [{active, once}]),
            {noreply, State};
        none ->
            io:format("Получено от клиента (~s): ~ts~n", [Room, Str]),
            room_manager:broadcast(Room, Str, self()),
            send_line(Socket, io_lib:format("Вы: ~ts", [Str])),
            inet:setopts(Socket, [{active, once}]),
            {noreply, State}
    end;

handle_info({tcp_closed, _Socket}, State = #state{room=Room}) ->
    room_manager:leave_room(self(), Room),
    io:format("Клиент отключился от: ~s~n", [Room]),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(activate, State = #state{socket=Socket}) ->
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

parse_command(Str) when is_list(Str) ->
    case Str of
        "/help" -> help;
        _ ->
            Toks = string:tokens(Str, " \t"),
            case Toks of
                ["/nick"] -> none;
                ["/nick"|Rest] -> {nick, string:join(Rest, " ")};
                ["/join", Room] -> {join, Room};
                ["/leave", Room] -> {leave, Room};
                _ -> none
            end
    end.

handle_set_nick(Nick, State) ->
    case room_manager:set_nick(self(), Nick) of
        {ok, Room} ->
            {io_lib:format("\n\n================================\nУстановленное имя: ~ts\nВы находитесь в комнате: ~ts\n/help — справка о других командах\n================================\n\n", [Nick, Room]), State#state{nick=Nick}};
        Other -> {io_lib:format("Ошибка установки имени: ~p", [Other]), State}
    end.

send_line(Socket, IoData) ->
    Bin = unicode:characters_to_binary([IoData, "\n"], unicode, utf8),
    gen_tcp:send(Socket, Bin).

help_text() ->
    "\n\n================================\nКоманды:\n/nick <имя> — установить имя\n/join <название_комнаты> — войти в комнату\n/leave <название_комнаты> — выйти из комнаты\n================================\n\n".
