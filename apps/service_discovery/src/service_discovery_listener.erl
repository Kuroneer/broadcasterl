-module(service_discovery_listener).

-export([start_link/0]).

-behaviour(gen_server).
-export([
         init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2
        ]).

-record(state, {
          node_name,
          port,
          socket,
          timer_ref,
          timer_msg_ref
         }).

-define(TIMER, 5000).
-define(TIMEOUT, timeout).
-define(BROADCAST, 5).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init(_) ->
    ListenPort = case os:getenv("DISCOVERY_PORT", 6350) of
                     I when is_integer(I) -> I;
                     Other -> list_to_integer(Other)
                 end,
    true = ListenPort > 0,
    {ok, UdpSocket} = gen_udp:open(ListenPort, [{ip, {0,0,0,0}}, {active, true}]),
    link(UdpSocket),
    {ok, set_timer(#state{port = ListenPort, socket = UdpSocket, node_name = atom_to_binary(node(), latin1)})}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call(_E, _From, State) ->
    {noreply, State}.

handle_info({?TIMEOUT, Ref}, #state{timer_msg_ref = Ref} = State) ->
    [ broadcast_info(State) || _ <- lists:seq(1, 1 + rand:uniform(?BROADCAST - 1)) ],
    logger:notice("Service discovery: Cluster node count is ~w", [length(nodes())+1]),
    {noreply, set_timer(State)};
handle_info({udp, Socket, _Ip, _IpPortNo, Data}, #state{socket = Socket} = State) ->
    handle_msg(Data),
    {noreply, State};
handle_info(check_nodes, State) ->
    logger:notice("Service discovery: Cluster node count is ~w", [length(nodes())+1]),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.


%%====================================================================
%% Internal functions
%%====================================================================

handle_msg(Msg) ->
    Key = crypto:hash(sha256, atom_to_binary(erlang:get_cookie(), latin1)),
    case catch crypto:crypto_one_time(rc4, Key, Msg, false) of
        <<3, NameByteSize/integer, Name:NameByteSize/binary>> ->
            Node = binary_to_atom(Name, latin1),
            case lists:member(Node, [node() | nodes()]) of
                true ->
                    ok;
                _ ->
                    logger:notice("Connecting to node ~s...", [Name]),
                    case net_adm:ping(Node) of
                        pong ->
                            % Let the other node know
                            {?MODULE, Node} ! check_nodes,
                            ok;
                        _ ->
                            ok
                    end
            end;
        _ ->
            ok
    end,
    ok.

broadcast_info(#state{port = Port, socket = Socket, node_name = Name}) ->
    DiscoveryAddress = os:getenv("DISCOVERY_ADDRESS", "erlang-discovery"),
    Key = crypto:hash(sha256, atom_to_binary(erlang:get_cookie(), latin1)),
    Data = <<3, (byte_size(Name))/integer, Name/binary>>, % TODO use HMAC instead of '3', add random bytes to prevent replay attacks
    Packet = crypto:crypto_one_time(rc4, Key, Data, true),
    gen_udp:send(Socket, DiscoveryAddress, Port, Packet),
    ok.

set_timer(#state{timer_ref = undefined} = State) ->
    %TODO Make this timer an exponential backoff
    MsgRef = make_ref(),
    Rand = trunc(rand:uniform() * 2500),
    TimerRef = erlang:send_after(?TIMER + Rand, self(), {?TIMEOUT, MsgRef}),
    State#state{timer_msg_ref = MsgRef, timer_ref = TimerRef};
set_timer(#state{timer_ref = TimerRef} = State) ->
    erlang:cancel_timer(TimerRef),
    set_timer(State#state{timer_ref = undefined}).

