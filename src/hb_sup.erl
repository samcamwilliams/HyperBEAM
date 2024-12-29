-module(hb_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-define(SERVER, ?MODULE).
-include("src/include/hb.hrl").

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                intensity => 0,
                period => 1},
    case hb_opts:get(store) of
        [RocksDBOpts = {hb_store_rocksdb, _}] ->
            ChildSpecs = [
                #{
                    id => hb_store_rocksdb,
                    start => {hb_store_rocksdb, start_link, [RocksDBOpts]}
                }
            ],
            {ok, {SupFlags, ChildSpecs}};
        _ -> {ok, {SupFlags, []}}
    end.
