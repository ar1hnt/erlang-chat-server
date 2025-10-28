-module(chat_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        {room_manager, {room_manager, start_link, []}, permanent, 5000, worker, [room_manager]},
        {client_sup, {client_sup, start_link, []}, permanent, 5000, supervisor, [client_sup]},
        {listener, {listener, start_link, [5555]}, permanent, 5000, worker, [listener]}
    ],
    {ok, {{one_for_one, 10, 10}, Children}}.
