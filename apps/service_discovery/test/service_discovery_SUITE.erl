-module(service_discovery_SUITE).
-compile(export_all).

suite() ->
    [{timetrap, {seconds, 60}}].


all() -> [happy_case].


init_per_testcase(happy_case, Config) ->
    % UDP random dispatch is simulated with reuseaddr and localhost (although it's not
    % entirely random, it's enough for tests)
    application:set_env(service_discovery, listen_options, [{reuseaddr, true}, {ip, {0,0,0,0}}], [{persistent, true}]),
    application:set_env(service_discovery, discovery_address, ["localhost"], [{persistent, true}]),
    application:ensure_all_started(service_discovery),
    init_per_testcase(undefined, [{extra_nodes, ['node@localhost']} | Config]);
init_per_testcase(_, Config) ->
    Config.


end_per_testcase(happy_case, Config) ->
    application:stop(service_discovery),
    end_per_testcase(undefined, Config);
end_per_testcase(_, Config) ->
    stop_nodes(Config),
    ok.


%%====================================================================
%% Test cases
%%====================================================================

happy_case(Config) ->
    ExtraNodes = get_extra_nodes(Config),
    ok = net_kernel:monitor_nodes(true),

    % Create extra nodes
    start_nodes(Config),
    % Receive connection from all the nodes
    [receive {nodeup, Node} -> ok
     after 5000 -> exit({missing, {nodeup1, Node}})
     end || Node <- ExtraNodes],

    % Disconnect those nodes
    [rpc:cast(Node, ?MODULE, node_disconnect_and_start, [node()]) || Node <- ExtraNodes],

    % Receive disconnection from all the nodes
    [receive {nodedown, Node} -> ok
     after 5000 -> exit({missing, {nodedown, Node}})
     end || Node <- ExtraNodes],

    [] = nodes(),

    % Receive connection from all the nodes
    [receive {nodeup, Node} -> ok
     after 5000 -> exit({missing, {nodeup2, Node}})
     end || Node <- ExtraNodes],
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

get_extra_nodes(Config) ->
    proplists:get_value(extra_nodes, Config, []).


start_nodes(Config) ->
    [start_node(Name) || Name <- get_extra_nodes(Config)].


start_node(Name) ->
    AppConfig = [
                 {application, set_env, [service_discovery, Par, Value, [{persistent, true}]]}
                 || {Par, Value} <- application:get_all_env(service_discovery)
                ],
    StartupMFAs = AppConfig ++ [
                                {code, add_paths, [code:get_path()]},
                                {?MODULE, node_terminate_after, [60000]}
                               ],
    {ok, HostNode} = ct_slave:start(Name,
                                    [{kill_if_fail, true},
                                     {init_timeout, 3000},
                                     {startup_timeout, 3000},
                                     {startup_functions, StartupMFAs},
                                     {erlang_flags, "-setcookie " ++ atom_to_list(erlang:get_cookie())}]),

    ct:pal("\e[32m Node ~p [OK] \e[0m", [HostNode]),
    HostNode.


stop_nodes(Config) ->
    [case net_adm:ping(Name) of
         pong ->
             case catch ct_slave:stop(Name) of
                 {ok, _} -> ok;
                 Other -> ct:pal("Error while stopping ~p: ~p", [Name, Other])
             end;
         _ -> ok
     end || Name <- get_extra_nodes(Config)],
    proplists:delete(extra_nodes, Config).


node_terminate_after(N) ->
    % Make sure that spawned nodes do not survive
    spawn(fun() -> timer:sleep(N), init:stop() end).

node_disconnect_and_start(RunnerNode) ->
    % Disconnect from the runner and start the discovery app
    spawn(fun() ->
                  disconnect_node(RunnerNode),
                  application:ensure_all_started(service_discovery)
          end).

