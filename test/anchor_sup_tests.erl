-module(anchor_sup_tests).
-include_lib("anchor/include/anchor.hrl").
-include_lib("eunit/include/eunit.hrl").

anchor_sup_creates_all_pools_test_() ->
    {setup,
     fun() ->
             application:set_env(anchor, pools, [{foo, []}, {bar, []}]),
             application:ensure_all_started(shackle),
             anchor_sup:start_link(),
             ok
     end,
     fun(_) ->
             shackle_pool:stop(foo),
             shackle_pool:stop(bar),
             ok
     end,
     [fun () -> server_presence_test(shackle_sup, [foo, bar]) end]}.

anchor_uses_config_from_env_test_() ->
    {setup,
     fun() ->
             meck:new(shackle_pool),
             meck:expect(shackle_pool, start, fun(_, _, _, _) -> ok end),
             ok
     end,
     fun(_) ->
             meck:unload(shackle_pool)
     end,
     [fun() ->
              MemcachedConfig = [{ip, 1},
                                 {port, 4},
                                 {reconnect, 5},
                                 {reconnect_time_max, 6},
                                 {reconnect_time_min, 7},
                                 {socket_options, 8}],
              PoolConfig = [{backlog_size, 0},
                            {pool_size, 2},
                            {pool_strategy, 3}],
              application:set_env(anchor,
                                  pools,
                                  [{foo, [{backlog_size, 0},
                                          {ip, 1},
                                          {pool_size, 2},
                                          {pool_strategy, 3},
                                          {port, 4},
                                          {reconnect, 5},
                                          {reconnect_time_max, 6},
                                          {reconnect_time_min, 7},
                                          {socket_options, 8}]}]),
              anchor_sup:init([]),
              StartArgs = [foo, anchor_client, MemcachedConfig, PoolConfig],
              ?assert(meck:called(shackle_pool,
                                  start,
                                  StartArgs))
      end]}.

%% tests
server_presence_test(Sup, Pools) ->
  Running = lists:map(fun (X) -> element(1, X) end,
    supervisor:which_children(Sup)),
  Expecting = lists:flatten(lists:map(
    fun (X) -> mk_pool_spec(X, ?DEFAULT_POOL_SIZE) end,
    Pools)),
  lists:foreach(fun (X) ->
      ?assert(lists:member(X, Running), atom_to_list(X) ++ " is not running")
    end,
    Expecting).

%% utils
server_name(Name, Index) ->
%% from shackle_pool:server_name
  list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(Index)).

server_monitor_name(Name) ->
%% from shackle_pool:server_monitor_name
  list_to_atom(atom_to_list(Name) ++ "_server_monitor").

mk_pool_spec(Name, Size) ->
  [server_monitor_name(Name) |
    [server_name(Name, I) || I <- lists:seq(1, Size)]].
