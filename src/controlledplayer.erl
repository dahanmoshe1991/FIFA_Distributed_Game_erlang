%%%-------------------------------------------------------------------
%%% @author Moshe.Yuval
%%% @copyright Moshe & Yuval
%%% @doc
%%%
%%% @end
%%% Created : 14. Aug 2020 10:00
%%%-------------------------------------------------------------------
-module(controlledplayer).
-author("Moshe.Yuval").

-behaviour(gen_statem).

-include("../params.hrl").

-export([init/1, startme/3, move/2, moving/3, delete/1, switchMonitor/3,  callback_mode/0, terminate/3]).


startme(ID, Location,MonitorName) ->
  gen_statem:start(?MODULE, [ID, Location, MonitorName], []).

init([ID, Location, MonitorName]) ->
  erlang:link(global:whereis_name(MonitorName)),
  Self = self(),
  FunToSpawn = fun() -> global:register_name(ID, Self) end,
  spawn(FunToSpawn),
  put(id, ID),
  put(monitor, MonitorName),
  put(location, Location),
  insertComponent(),
  loopAction(?TimeToWait1, {move, Location}),
  {ok, moving, {}}.

loopAction(Time, Content) ->
  NextActionTimerRef = erlang:send_after(Time, self(), Content),
  put(nextActionTimerRef, NextActionTimerRef).

move(ID, Location) ->
  gen_statem:cast({global, ID}, {move, Location}).


delete(ID) ->
  gen_statem:cast({global, ID}, delete).

switchMonitor(ID, MonitorsNumber, MonitorsNames) ->
  gen_statem:cast({global, ID}, {switchMonitor, MonitorsNumber, MonitorsNames}).

moving(_,{move, Location}, Content) ->
  abortNextAction(),
  put(location, Location),
  refreshInfo(),
  {next_state, moving, Content};

moving(_,delete, Content) ->
  abortNextAction(),
  {stop, normal, Content};

moving(_,{switchMonitor, MonitorsNumber, MonitorsNames}, Content) ->
  abortNextAction(),
  {X, Y} = get(location),
  ID = get(id),
  monitor:switchedMonitor( get(monitor), ID), %delete obj from ets @ monitor
  NextOwner = common:pickNextMonitor(X, MonitorsNumber, MonitorsNames),
  global:unregister_name(ID),
  monitor:addControlledplayer(NextOwner, {ID, {X,Y}, old}),
  {stop, normal, Content}.

abortNextAction() ->
  erlang:cancel_timer(get(nextActionTimerRef)).

terminate(_Reason, _StateName, _StatData) ->
  ok.

refreshInfo() ->
  monitor:refreshInfo(get(monitor), get(id), {controlledplayer, get(location), { get(monitor)}}).

insertComponent() ->
  monitor:insertComponent(get(monitor), get(id), {controlledplayer, get(location), {get(monitor)}}).

callback_mode() ->
  state_functions.