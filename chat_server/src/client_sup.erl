-module(client_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ClientChild = {client_proc, {client_proc, start_link, []},
                   temporary, 5000, worker, [client_proc]},
    {ok, {{simple_one_for_one, 5, 10}, [ClientChild]}}.
