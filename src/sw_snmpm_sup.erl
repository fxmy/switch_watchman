-module(sw_snmpm_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  %% assums just one single user for now
  [User] = snmpm:which_users(),
  Agents = snmpm:which_agents(User),
  {ok, Scalars} = application:get_env(scalar),
  {ok, Tables} = application:get_env(table),
  ManagedObjs = [{scalar, Scalars}, {table, Tables}],
  Children =
  [{A,
    {sw_snmpm, start_link, [User, A, ManagedObjs]},
    permanent, 5000, worker, [sw_snmpm]}
   || A <- Agents],
  {ok, {{one_for_one, length(Agents), 5*60}, Children}}.
