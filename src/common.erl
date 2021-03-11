%%%-------------------------------------------------------------------
%%% @author Moshe.Yuval
%%% @copyright Moshe & Yuval
%%% @doc
%%%
%%% @end
%%% Created : 13. Aug 2020 20:06
%%%-------------------------------------------------------------------
-module(common).
-author("Moshe.Yuval").

-include("../params.hrl").

-export([pickNextMonitor/3, movementDrawing/3, getDistance/2, ballMovementDrawing/3]).


pickNextMonitor(X, 4, MonitorsNames) ->
  [Monitor1, Monitor2, Monitor3, Monitor4] = MonitorsNames,
  SectionWidth = getSectionWidth(4),
  case {X < SectionWidth, X < 2 * SectionWidth, X < 3 * SectionWidth, X < 4 * SectionWidth + 100} of
    {true, _, _, _} -> Monitor1;
    {false, true, _, _} -> Monitor2;
    {false, false, true, _} -> Monitor3;
    {false, false, false, true} -> Monitor4
  end;
pickNextMonitor(X, 3, MonitorsNames) ->
  [Monitor1, Monitor2, Monitor3] = MonitorsNames,
  SectionWidth = getSectionWidth(3),
  case {X < SectionWidth, X < 2 * SectionWidth, X < 3 * SectionWidth + 100} of
    {true, _, _} -> Monitor1;
    {false, true, _} -> Monitor2;
    {false, false, true} -> Monitor3
  end;
pickNextMonitor(X, 2, MonitorsNames) ->
  [Monitor1, Monitor2] = MonitorsNames,
  SectionWidth = getSectionWidth(2),
  case {X < SectionWidth, X < 2 * SectionWidth + 100} of
    {true, _} -> Monitor1;
    {false, true} -> Monitor2
  end;
pickNextMonitor(_, 1, MonitorsNames) ->
  [Monitor1] = MonitorsNames,
  Monitor1.

getSectionWidth(MonitorsNumber) ->
  ((?X_Upper_Limit + 1) / MonitorsNumber).

getDistance({X1, Y1}, {X2, Y2}) ->
  DiffX = X1 - X2,
  DiffY = Y1 - Y2,
  DiffX_2 = math:pow(DiffX, 2),
  DiffY_2 = math:pow(DiffY, 2),
  math:sqrt(DiffX_2 + DiffY_2).

movementDrawing({X, Y_1}, {X, Y_2}, StepSize) ->
  RelativeDistance = getDistance({X, Y_1}, {X, Y_2}) - StepSize,
  case RelativeDistance < 0 of
    true -> {X, Y_2};
    _ ->
      case Y_1 > Y_2 of
        true ->
          NextY = Y_1 - StepSize,
          {X, NextY};
        false ->
          NextY = Y_1 + StepSize,
          {X, NextY}
      end
  end;

movementDrawing({X_1, Y_1}, {X_2, Y_2}, StepSize) ->
  RelativeDistance = getDistance({X_1, Y_1}, {X_2, Y_2}) - StepSize,
  case RelativeDistance < 0 of
    true -> {X_2, Y_2};
    _ ->
      {Slope, Direction_Y, Direction_X, Small_Y_Step, Small_X_Step} = calcDirections(X_1,Y_1,X_2,Y_2,StepSize),
      if Direction_X < 0 ->
        NextX = X_1 + Small_X_Step;
        true -> NextX = X_1 - Small_X_Step
      end,
      if (((Direction_Y < 0) and (Slope > 0)) or ((Direction_Y > 0) and (Slope < 0))) ->
        NextY = Y_1 + Small_Y_Step,
        {NextX, NextY};
        true ->
          NextY = Y_1 - Small_Y_Step,
          {NextX, NextY}
      end
  end.

ballMovementDrawing({X, Y_1}, {X, Y_2}, StepSize) ->
  RelativeDistance = getDistance({X, Y_1}, {X, Y_2}) - StepSize,
  case RelativeDistance < 0 of
    true ->
      ets:insert(isOwner, {kickDirection, 0}),
      ets:insert(ball, {flag, false}),
      {X, Y_2};
    _ ->
      case Y_1 > Y_2 of
        true ->
          NextY = Y_1 - StepSize;
        false ->
          NextY = Y_1 + StepSize
      end,
      {X, NextY}
  end;

ballMovementDrawing({X_1, Y_1}, {X_2, Y_2}, StepSize) ->
  RelativeDistance = getDistance({X_1, Y_1}, {X_2, Y_2}) - StepSize,
  case RelativeDistance < 0 of
    true -> {X_2, Y_2};
    _ ->
      {Slope, Direction_Y, Direction_X, Small_Y_Step, Small_X_Step} = calcDirections(X_1,Y_1,X_2,Y_2,StepSize),
      if Direction_X < 0 ->
        NextX = X_1 + Small_X_Step;
        true -> NextX = X_1 - Small_X_Step
      end,
      if (((Direction_Y < 0) and (Slope > 0)) or ((Direction_Y > 0) and (Slope < 0))) ->
        NextY = Y_1 + Small_Y_Step;
        true ->
          NextY = Y_1 - Small_Y_Step
      end,
      {NextX, NextY}
  end.

calcDirections(X_1,Y_1,X_2,Y_2,StepSize)->
  Slope = (Y_2 - Y_1) / (X_2 - X_1),
  SlopeSquared = math:pow(Slope, 2),
  StepSquared = math:pow(StepSize, 2),
  Small_X_Step = math:sqrt(StepSquared / (SlopeSquared + 1)),
  Small_Y_Step = Small_X_Step * Slope,
  Direction_X = X_1 - X_2,
  Direction_Y = Y_1 - Y_2,
  {Slope, Direction_Y, Direction_X, Small_Y_Step, Small_X_Step} .