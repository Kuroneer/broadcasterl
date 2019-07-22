-module(service_discovery).

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).

-define(APP, ?MODULE).
-define(DEFAULT_SOCKET_ACCEPTORS, 100).

%%====================================================================
%% callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    supervisor:start_link(?MODULE, []).

stop(_State) ->
    ok.

init(_) ->
    {ok, {#{strategy => one_for_one}, [child_spec(service_discovery_listener)]}}.

%%====================================================================
%% Internal functions
%%====================================================================

child_spec(Module) ->
    #{id => Module,
      start => {Module, start_link, []},
      % restart => permanent,
      shutdown => infinity,
      type => worker
      % modules => [Module]
     }.

