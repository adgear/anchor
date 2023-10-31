-module(anchor_sup).

-include("anchor.hrl").

-export([
    start_link/0
]).

-behaviour(supervisor).

-export([
    init/1
]).

%% public
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
-spec init([]) -> {ok, {{one_for_one, 5, 10}, []}}.
init([]) ->
    PoolOptions = default_pool_options(),
    ClientOptions = default_client_options(),
    ShackleConfig = ?GET_ENV(shackle, []),

    PoolConfig = proplists:from_map(
        maps:merge(
            maps:from_list(PoolOptions),
            proplists:to_map(proplists:get_value(pool, ShackleConfig, []))
        )
    ),
    ClientConfig = proplists:from_map(
        maps:merge(
            maps:from_list(ClientOptions),
            proplists:to_map(proplists:get_value(client, ShackleConfig, []))
        )
    ),

    ok = shackle_pool:start(
        ?APP,
        ?CLIENT,
        ClientConfig,
        PoolConfig
    ),

    {ok, {{one_for_one, 5, 10}, []}}.

default_client_options() ->
    [
        {ip, ?GET_ENV(ip, ?DEFAULT_IP)},
        {port, ?GET_ENV(port, ?DEFAULT_PORT)},
        {reconnect, ?GET_ENV(reconnect, ?DEFAULT_RECONNECT)},
        {reconnect_time_max, ?GET_ENV(reconnect_time_max, ?DEFAULT_RECONNECT_MAX)},
        {reconnect_time_min, ?GET_ENV(reconnect_time_min, ?DEFAULT_RECONNECT_MIN)},
        {socket_options, ?GET_ENV(socket_options, ?DEFAULT_SOCKET_OPTIONS)},
        {bounce_interval_secs, ?DEFAULT_BOUNCE_INTERVAL}
].

default_pool_options() ->
    [
        {backlog_size, ?GET_ENV(backlog_size, ?DEFAULT_BACKLOG_SIZE)},
        {pool_size, ?GET_ENV(pool_size, ?DEFAULT_POOL_SIZE)},
        {pool_strategy, ?GET_ENV(pool_strategy, ?DEFAULT_POOL_STRATEGY)}
    ].
