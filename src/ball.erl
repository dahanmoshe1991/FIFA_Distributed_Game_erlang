%%%-------------------------------------------------------------------
%%% @author Moshe.Yuval
%%% @copyright Moshe & Yuval
%%% @doc
%%%
%%% @end
%%% Created : 14. Aug 2020 12:22
%%%-------------------------------------------------------------------
-module(ball).
-author("Moshe.Yuval").

-behaviour(gen_statem).

-include("../params.hrl").
-export([startme/4, init/1, callback_mode/0, moving/3, delete/1, switchMonitor/3, terminate/3]).


startme(ID, Location, Destination, MonitorName) ->
  gen_statem:start(?MODULE, [ID, Location, Destination, MonitorName], []).

init([ID, Location, Destination, MonitorName]) ->
  Self = self(),
  FunToSpawn = fun() -> global:register_name(ID, Self) end,
  erlang:link(global:whereis_name(MonitorName)),
  spawn(FunToSpawn),
  put(id, ID),
  put(monitor, MonitorName),
  put(location, Location),
  put(dest, Destination),
  insertComponent(),
  timer:send_interval(?REFRESH_RATE, Self, refresh),
  loopAction(?TimeToWait1, {move, Destination}),
  {ok, moving, {}}.

loopAction(Time, Content) ->
  NextActionTimerRef = erlang:send_after(Time, self(), Content),
  put(previousAction, Content),
  put(nextActionTimerRef, NextActionTimerRef).

callback_mode() ->
  state_functions.

delete(ID) ->
  gen_statem:cast({global, ID}, delete).

switchMonitor(ID, MonitorsNumber, MonitorsNames) ->
  gen_statem:cast({global, ID}, {switchMonitor, MonitorsNumber, MonitorsNames}).

insertComponent() ->
  monitor:insertComponent(get(monitor), get(id), {ball, get(location), {get(dest)}}).

refreshInfo() ->
  monitor:refreshInfo(get(monitor), get(id), {ball, get(location), {get(dest)}}).

moving(_, refresh, Data) ->
  refreshInfo(),
  {next_state, moving, Data};

moving(_, {move, Destination}, Data) ->
  NextLocation = common:movementDrawing(get(location), Destination, 2),
  put(location, NextLocation),
  refreshInfo(),
  loopAction(?TimeToWait1, {move, Destination}),
  {next_state, moving, Data};

moving(cast, delete, Data) ->
  abortNextAction(),
  global:unregister_name(get(id)),
  {stop, normal, Data};

moving(cast, {switchMonitor, MonitorsNumber, MonitorsNames}, Data) ->
  abortNextAction(),
  {X, _} = get(location),
  NextOwner = common:pickNextMonitor(X, MonitorsNumber, MonitorsNames),
  ID = get(id),
  monitor:switchedMonitor(get(monitor), ID),
  global:unregister_name(ID),
  monitor:addBall(NextOwner, {ID, get(location), get(dest), old}),
  {stop, normal, Data}.

terminate(_Reason, _StateName, _StatData) ->
  ok.

abortNextAction() ->
  erlang:cancel_timer(get(nextActionTimerRef)).
