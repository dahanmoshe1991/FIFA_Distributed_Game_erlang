%%%-------------------------------------------------------------------
%%% @author Moshe.Yuval
%%% @copyright Moshe & Yuval
%%% @doc
%%%
%%% @end
%%% Created : 13. Aug 2020 10:39
%%%-------------------------------------------------------------------
-module(computerplayer).
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
  put(goalieOneLocation, ?GOALIE_Y_START),
  put(goalieTwoLocation, ?GOALIE_Y_START),
  put(goalieOneDirection, 1), % 0 - down, 1 - up
  put(goalieTwoDirection, 1), % 0 - down, 1 - up
  put(goalieOneShouldMove, 0),
  put(goalieTwoShouldMove, 0),
  insertComponent(),
  timer:send_interval(?REFRESH_RATE, self(), refresh),
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
  monitor:insertComponent(get(monitor), get(id), {computerplayer, get(location), {get(dest)}}).

refreshInfo() ->
  monitor:refreshInfo(get(monitor), get(id), {computerplayer, get(location), {get(dest)}}).

moving(_, refresh, Data) ->
  refreshInfo(),
  {next_state, moving, Data};

moving(_, {move, Destination}, Data) ->
  MyId = get(id),
  GoalieOneLocation = get(goalieOneLocation),
  GoalieTwoLocation = get(goalieTwoLocation),
  GoalieOneDirection = get(goalieOneDirection),
  GoalieTwoDirection = get(goalieTwoDirection),
  GoalieOneShouldMove = get(goalieOneShouldMove),
  GoalieTwoShouldMove = get(goalieTwoShouldMove),
  NextLocation = common:movementDrawing(get(location), Destination, 2),
  if MyId == 1.0 ->
    moveGoalie(GoalieOneShouldMove, goalieOneShouldMove, GoalieOneDirection, goalieOneDirection, GoalieOneLocation, goalieOneLocation, 70);
    MyId == 2.0 ->
      moveGoalie(GoalieTwoShouldMove, goalieTwoShouldMove, GoalieTwoDirection, goalieTwoDirection, GoalieTwoLocation, goalieTwoLocation, 1365);
    true ->
      put(location, NextLocation)
  end,
  refreshInfo(),
  case NextLocation == Destination of
    false ->
      loopAction(?TimeToWait1, {move, Destination});
    _ ->
      {NextX, NextY} = randomPlayerDest(MyId),
      put(dest, {NextX, NextY}),
      loopAction(?TimeToWait1, {move, {NextX, NextY}})
  end,
  {next_state, moving, Data};

moving(cast, delete, Data) ->
  abortNextAction(),
  {stop, normal, Data};

moving(cast, {switchMonitor, MonitorsNumber, MonitorsNames}, Data) ->
  abortNextAction(),
  {X, _} = get(location),
  NextOwner = common:pickNextMonitor(X, MonitorsNumber, MonitorsNames),
  ID = get(id),
  monitor:switchedMonitor(get(monitor), ID),
  global:unregister_name(ID),
  monitor:addAPlayer(NextOwner, {ID, get(location), get(dest)}),
  {stop, normal, Data}.

abortNextAction() ->
  erlang:cancel_timer(get(nextActionTimerRef)).

moveGoalie(GoalieShouldMove, AtomGoalieShouldMove, GoalieDirection, AtomGoalieDirection, GoalieLocation, AtomGoalieLocation, X_Coordinate) ->
  if GoalieShouldMove == 4 ->
    put(AtomGoalieShouldMove, 0),
    if
      GoalieDirection == 1 -> % Going up
        if GoalieLocation < ?GATE_UPPER_LIMIT ->
          put(AtomGoalieLocation, GoalieLocation + ?PLAYER_STEP_SIZE),
          put(location, {X_Coordinate, GoalieLocation + ?PLAYER_STEP_SIZE});
          true ->
            put(AtomGoalieLocation, ?GATE_UPPER_LIMIT),
            put(AtomGoalieDirection, 0),
            put(location, {X_Coordinate, ?GATE_UPPER_LIMIT})
        end;
      true -> % Going down.
        if GoalieDirection > ?GATE_LOWER_LIMIT ->
          put(AtomGoalieLocation, GoalieLocation - ?PLAYER_STEP_SIZE),
          put(location, {X_Coordinate, GoalieLocation - ?PLAYER_STEP_SIZE});
          true ->
            put(AtomGoalieLocation, ?GATE_LOWER_LIMIT),
            put(AtomGoalieDirection, 1),
            put(location, {X_Coordinate, ?GATE_LOWER_LIMIT})
        end
    end;
    true ->
      put(AtomGoalieShouldMove, GoalieShouldMove + 1)
  end.

terminate(_Reason, _StateName, _StatData) ->
  ok.

randomPlayerDest(ID) ->
  if ((ID =< 1.5) and (ID /= 1.0)) ->
    Res = {rand:uniform(680) + 40, rand:uniform(460) + 40};
    true ->
      Res = {rand:uniform(1360) + 40, rand:uniform(460) + 40}
  end,
  Res.