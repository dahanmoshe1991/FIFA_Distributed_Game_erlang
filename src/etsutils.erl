%%%-------------------------------------------------------------------
%%% @author Moshe.Yuval
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2020 19:06
%%%-------------------------------------------------------------------
-module(etsutils).
-author("Moshe.Yuval").
-include("../params.hrl").

%% API
-export([generateEts/0]).

generateEts() ->
  ets:new(playersETS, [set, public, named_table]),
  ets:new(monitors, [set, public, named_table]),
  ets:new(generalState, [set, public, named_table]),

  ets:new(controlledplayerDir, [set, public, named_table]),
  ets:new(controlledplayerX, [set, public, named_table]),
  ets:insert(controlledplayerDir, {direction, 0}),
  ets:insert(controlledplayerX, {xCoordinate, 1}),

  ets:new(ball, [set, public, named_table]),
  utils:initializeBallLocation(),
  ets:insert(ball, {previousOwner, 0}),

  ets:new(isOwner, [set, public, named_table]),
  ets:insert(isOwner, {ball, 0}),   % 0 - no movement of the ball, 1 - kick left, 2 - kick right
  ets:insert(isOwner, {kickDirection, 0}), % "1" - if the ball need to be kicked left, "2" - right, "0" - no kick.
  ets:insert(isOwner, {owned, 0}),

  ets:new(statistics, [set, public, named_table]),
  ets:insert(statistics, {1.0, 0}),
  ets:insert(statistics, {1.1, 0}),
  ets:insert(statistics, {1.2, 0}),
  ets:insert(statistics, {1.3, 0}),
  ets:insert(statistics, {1.4, 0}),
  ets:insert(statistics, {1.5, 0}),
  ets:insert(statistics, {1.6, 0}),
  ets:insert(statistics, {1.7, 0}),
  ets:insert(statistics, {1.8, 0}),
  ets:insert(statistics, {1.9, 0}),
  ets:insert(statistics, {1.11, 0}),
  ets:insert(statistics, {2.0, 0}),
  ets:insert(statistics, {2.1, 0}),
  ets:insert(statistics, {2.2, 0}),
  ets:insert(statistics, {2.3, 0}),
  ets:insert(statistics, {2.4, 0}),
  ets:insert(statistics, {2.5, 0}),
  ets:insert(statistics, {2.6, 0}),
  ets:insert(statistics, {2.7, 0}),
  ets:insert(statistics, {2.8, 0}),
  ets:insert(statistics, {2.9, 0}),

  ets:insert(statistics, {controlledPlayer, 0}),
  ets:insert(statistics, {prevStatupdate, -1}),
  ets:insert(statistics, {teamOnePoints, 0}),
  ets:insert(statistics, {teamTwoPoints, 0}),

  ets:new(playerDirection, [set, public, named_table]),
  ets:insert(playerDirection, {1.0, 1}),
  ets:insert(playerDirection, {1.1, 1}),
  ets:insert(playerDirection, {1.2, 1}),
  ets:insert(playerDirection, {1.3, 1}),
  ets:insert(playerDirection, {1.4, 1}),
  ets:insert(playerDirection, {1.5, 1}),
  ets:insert(playerDirection, {1.6, 1}),
  ets:insert(playerDirection, {1.7, 1}),
  ets:insert(playerDirection, {1.8, 1}),
  ets:insert(playerDirection, {1.9, 1}),
  ets:insert(playerDirection, {1.11, 1}),
  ets:insert(playerDirection, {2.0, 1}),
  ets:insert(playerDirection, {2.1, 1}),
  ets:insert(playerDirection, {2.2, 1}),
  ets:insert(playerDirection, {2.3, 1}),
  ets:insert(playerDirection, {2.4, 1}),
  ets:insert(playerDirection, {2.5, 1}),
  ets:insert(playerDirection, {2.6, 1}),
  ets:insert(playerDirection, {2.7, 1}),
  ets:insert(playerDirection, {2.8, 1}),
  ets:insert(playerDirection, {2.9, 1}),

  ets:insert(generalState, {status, waitForStartGame}),
  ets:insert(generalState, {round, 0}).