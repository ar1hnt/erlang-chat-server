-module(room_manager).
-behaviour(gen_server).

-export([start_link/0, set_nick/2, join_room/2, leave_room/2, broadcast/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {rooms = #{}, p2r = #{}, pid2nick = #{}, users = #{}}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

set_nick(UserPid, Nick) ->
    gen_server:call(?MODULE, {set_nick, UserPid, Nick}).

join_room(UserPid, Room) ->
    gen_server:call(?MODULE, {join, UserPid, Room}).

leave_room(UserPid, Room) ->
    gen_server:call(?MODULE, {leave, UserPid, Room}).

broadcast(Room, Msg, From) ->
    gen_server:abcast([node()|nodes()], ?MODULE, {broadcast, Room, Msg, From}).

 
handle_call({set_nick, UserPid, Nick}, _From, State = #state{rooms=Rooms0, p2r=P2R0, pid2nick=P2N0, users=Users0}) ->
    Users1 = case maps:find(UserPid, P2N0) of
                 {ok, OldNick} ->
                     case maps:get(OldNick, Users0, undefined) of
                         undefined -> Users0;
                         U -> maps:put(OldNick, maps:put(pid, undefined, U), Users0)
                     end;
                 error -> Users0
             end,
    U0 = maps:get(Nick, Users1, #{rooms => ordsets:add_element("main", ordsets:new()), pid => undefined}),
    RoomsList = maps:get(rooms, U0),
    Rooms1 = lists:foldl(
              fun(R, Acc) -> maps:update_with(R, fun(L) -> ordsets:to_list(ordsets:add_element(UserPid, ordsets:from_list(L))) end, [UserPid], Acc) end,
              Rooms0,
              ordsets:to_list(RoomsList)),
    P2R1 = case maps:is_key(UserPid, P2R0) of
               true -> P2R0;
               false -> Ref = erlang:monitor(process, UserPid), maps:put(UserPid, Ref, P2R0)
           end,
    Users2 = maps:put(Nick, maps:put(pid, UserPid, maps:put(rooms, RoomsList, U0)), Users1),
    P2N1 = maps:put(UserPid, Nick, P2N0),
    {reply, {ok, ordsets:to_list(RoomsList)}, State#state{rooms=Rooms1, p2r=P2R1, pid2nick=P2N1, users=Users2}};

handle_call({join, UserPid, Room}, _From, State = #state{rooms=Rooms0, p2r=P2R0, pid2nick=P2N0, users=Users0}) ->
    Rooms1 = maps:update_with(Room, fun(L) -> [UserPid|L] end, [UserPid], Rooms0),
    P2R1 = case maps:is_key(UserPid, P2R0) of
               true -> P2R0;
               false -> Ref = erlang:monitor(process, UserPid), maps:put(UserPid, Ref, P2R0)
           end,
    Users1 = case maps:find(UserPid, P2N0) of
                 {ok, Nick} ->
                     U = maps:get(Nick, Users0, #{rooms => ordsets:new(), pid => undefined}),
                     Rs = ordsets:add_element(Room, maps:get(rooms, U)),
                     maps:put(Nick, maps:put(rooms, Rs, maps:put(pid, UserPid, U)), Users0);
                 error -> Users0
             end,
    {reply, ok, State#state{rooms=Rooms1, p2r=P2R1, users=Users1}};

handle_call({leave, UserPid, Room}, _From, State = #state{rooms=Rooms0, p2r=P2R0, pid2nick=P2N0, users=Users0}) ->
    List0 = maps:get(Room, Rooms0, []),
    List1 = lists:delete(UserPid, List0),
    Rooms1 = case List1 of
                 [] -> maps:remove(Room, Rooms0);
                 _  -> maps:put(Room, List1, Rooms0)
             end,
    StillPresent = lists:any(fun({_R, L}) -> lists:member(UserPid, L) end,
                             maps:to_list(Rooms1)),
    P2R1 = case {StillPresent, maps:take(UserPid, P2R0)} of
               {false, {Ref, P2RRest}} -> erlang:demonitor(Ref, [flush]), P2RRest;
               _ -> P2R0
           end,
    Users1 = case maps:find(UserPid, P2N0) of
                 {ok, Nick} ->
                     U = maps:get(Nick, Users0, #{rooms => ordsets:new(), pid => undefined}),
                     Rs = ordsets:del_element(Room, maps:get(rooms, U)),
                     maps:put(Nick, maps:put(rooms, Rs, maps:put(pid, UserPid, U)), Users0);
                 error -> Users0
             end,
    {reply, ok, State#state{rooms=Rooms1, p2r=P2R1, users=Users1}}.

handle_cast({broadcast, Room, Msg, From}, State = #state{rooms=Rooms, pid2nick=P2N}) ->
    {{Y,Mo,D},{H,Mi,S}} = calendar:local_time(),
    Ts = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y,Mo,D,H,Mi,S]),
    Nick = case maps:find(From, P2N) of
               {ok, N} -> N;
               error -> "unknown"
           end,
    lists:foreach(
      fun(Pid) ->
          if Pid =/= From -> Pid ! {message, Ts, Room, Nick, Msg}; true -> ok end
      end,
      maps:get(Room, Rooms, [])),
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, State = #state{rooms=Rooms0, p2r=P2R0, pid2nick=P2N0, users=Users0}) ->
    Rooms1 = maps:map(fun(_Room, L) -> lists:delete(Pid, L) end, Rooms0),
    P2R1 = case maps:find(Pid, P2R0) of
               {ok, Ref} -> maps:remove(Pid, P2R0);
               error -> P2R0
           end,
    {Users1, P2N1} = case maps:find(Pid, P2N0) of
                         {ok, Nick} ->
                             U = maps:get(Nick, Users0, #{rooms => ordsets:new(), pid => undefined}),
                             {maps:put(Nick, maps:put(pid, undefined, U), Users0), maps:remove(Pid, P2N0)};
                         error -> {Users0, P2N0}
                     end,
    {noreply, State#state{rooms=Rooms1, p2r=P2R1, users=Users1, pid2nick=P2N1}};

handle_info(_Info, State) -> {noreply, State}.
terminate(_R, _S) -> ok.
code_change(_OldV, State, _Extra) -> {ok, State}.
