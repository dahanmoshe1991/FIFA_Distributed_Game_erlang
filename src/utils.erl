%%%-------------------------------------------------------------------
%%% @author Moshe.Yuval
%%% @copyright Moshe & Yuval
%%% @doc
%%%
%%% @end
%%% Created : 28. Sep 2020 17:53
%%%-------------------------------------------------------------------
-module(utils).
-author("Moshe.Yuval").

-include("../params.hrl").
-include_lib("wx/include/wx.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([getComponentsLayoutList/0, loadPlayerImage/4, loadImageControlledPlayer/4, printToScreen/3, initializeBallLocation/0]).


loadPlayerImage(ID, Orientation, BallWidth, X) ->
  if ID >= 2.0 -> Pic = wxImage:new(?TEAM2IMAGE),
    if
      (ID =:= 2.0) ->
        NewX = X - BallWidth / 135;
      (Orientation > 0) ->
        NewX = X + 10;
      true ->
        NewX = X - ?BALL_WIDTH_MULTIPLY_FACTOR * BallWidth
    end;
    true -> Pic = wxImage:new(?TEAM1IMAGE),
      if
        (ID =:= 1.0) ->
          NewX = X + 10;
        (Orientation > 0) ->
          NewX = X + 10;
        true ->
          NewX = X - ?BALL_WIDTH_MULTIPLY_FACTOR * BallWidth
      end
  end,
  {NewX, Pic}.

loadImageControlledPlayer(Orientation, _, X, PreviousLocation) ->
  Pic = wxImage:new(?CONTROLLED_PLAYER_IMAGE),
  if
    Orientation > 0 ->
      NewX = X + 10;
    Orientation == 0 ->
      if
        PreviousLocation > 0 ->
          NewX = X + 10;
        true ->
          NewX = X - 20
      end;
    Orientation < 0 ->
      NewX = X - 20
  end,
  {NewX, Pic}.

getComponentsLayoutList() ->
  List = [
    {1.0, {50, 300}, {50, 300}}, % goalie1
    {2.0, {1350, 300}, {1350, 300}}, % goalie2
    {1.1, {100, 600}, {100, 600}},
    {1.2, {200, 600}, {200, 600}},
    {1.3, {300, 550}, {300, 550}},
    {1.4, {400, 450}, {400, 450}},
    {1.5, {500, 600}, {500, 600}},
    {1.6, {600, 750}, {600, 800}},
    {1.7, {200, 750}, {200, 750}},
    {1.8, {550, 410}, {550, 410}},
    {1.9, {100, 400}, {450, 300}},
    {1.11, {200, 400}, {450, 300}},

    {2.1, {1040, 300}, {1050, 100}},
    {2.2, {1100, 30}, {450, 300}},
    {2.3, {1100, 40}, {460, 300}},
    {2.4, {1200, 50}, {470, 300}},
    {2.5, {1200, 60}, {480, 300}},
    {2.6, {1300, 100}, {490, 300}},
    {2.7, {1300, 600}, {510, 300}},
    {2.8, {1350, 750}, {520, 300}},
    {2.9, {1350, 750}, {530, 300}}],
  List.

printToScreen(ShouldPrint, WXPaint, Status) ->
  case (ShouldPrint) of
    true ->
      [{_, Player1_0}] = ets:lookup(statistics, 1.0),
      [{_, Player1_1}] = ets:lookup(statistics, 1.1),
      [{_, Player1_2}] = ets:lookup(statistics, 1.2),
      [{_, Player1_3}] = ets:lookup(statistics, 1.3),
      [{_, Player1_4}] = ets:lookup(statistics, 1.4),
      [{_, Player1_5}] = ets:lookup(statistics, 1.5),
      [{_, Player1_6}] = ets:lookup(statistics, 1.6),
      [{_, Player1_7}] = ets:lookup(statistics, 1.7),
      [{_, Player1_8}] = ets:lookup(statistics, 1.8),
      [{_, Player1_9}] = ets:lookup(statistics, 1.9),
      [{_, Player1_11}] = ets:lookup(statistics, 1.11),
      [{_, Player2_0}] = ets:lookup(statistics, 2.0),
      [{_, Player2_1}] = ets:lookup(statistics, 2.1),
      [{_, Player2_2}] = ets:lookup(statistics, 2.2),
      [{_, Player2_3}] = ets:lookup(statistics, 2.3),
      [{_, Player2_4}] = ets:lookup(statistics, 2.4),
      [{_, Player2_5}] = ets:lookup(statistics, 2.5),
      [{_, Player2_6}] = ets:lookup(statistics, 2.6),
      [{_, Player2_7}] = ets:lookup(statistics, 2.7),
      [{_, Player2_8}] = ets:lookup(statistics, 2.8),
      [{_, Player2_9}] = ets:lookup(statistics, 2.9),
      [{_, ControlledPlayer}] = ets:lookup(statistics, controlledPlayer),
      if Status == finishedGame ->
        Winner = whoIsTheWinner(),
        wxDC:drawLabel(WXPaint, "Team " ++ integer_to_list(Winner) ++ " Wins", {?PRINT_X_START + ?DOUBLE_TAB, ?PRINT_Y_START + ?TAB, ?PRINT_W_H, ?PRINT_W_H});
        true ->
          [{_, TeamOnePoints}] = ets:lookup(statistics, teamOnePoints),
          [{_, TeamTwoPoints}] = ets:lookup(statistics, teamTwoPoints),
          wxDC:drawLabel(WXPaint, "Team One Points: " ++ integer_to_list(TeamOnePoints) ++ "                       Team Two Points: " ++ integer_to_list(TeamTwoPoints), {?PRINT_X_START + ?DOUBLE_TAB, ?PRINT_Y_START + ?TAB, ?PRINT_W_H, ?PRINT_W_H})
      end,
      TeamOnePossessions = Player1_0 + Player1_1 + Player1_2 + Player1_3 + Player1_4 + Player1_5 + Player1_6 + Player1_7 + Player1_8 + Player1_9 + Player1_11,
      TeamTwoPossessions = Player2_0 + Player2_1 + Player2_2 + Player2_3 + Player2_4 + Player2_5 + Player2_6 + Player2_7 + Player2_8 + Player2_9 + ControlledPlayer,
      TotalNumberOfPossessions = TeamOnePossessions + TeamTwoPossessions,
      wxDC:drawLabel(WXPaint, "Statistics:", {?PRINT_X_START + ?DOUBLE_TAB, 100, ?PRINT_W_H, ?PRINT_W_H}),
      wxDC:drawLabel(WXPaint, "Total Number Of Ball Possesions: " ++ integer_to_list(TotalNumberOfPossessions), {?PRINT_X_START + ?DOUBLE_TAB, ?PRINT_Y_START + 2 * ?TAB, ?PRINT_W_H, ?PRINT_W_H}),
      wxDC:drawLabel(WXPaint, "Team One - Number Of Ball Possesions: " ++ integer_to_list(TeamOnePossessions), {?PRINT_X_START + ?DOUBLE_TAB, ?PRINT_Y_START + 3 * ?TAB, ?PRINT_W_H, ?PRINT_W_H}),
      wxDC:drawLabel(WXPaint, "Team Two - Number Of Ball Possesions: " ++ integer_to_list(TeamTwoPossessions), {?PRINT_X_START + ?DOUBLE_TAB, ?PRINT_Y_START + 4 * ?TAB, ?PRINT_W_H, ?PRINT_W_H}),
      wxDC:drawLabel(WXPaint, "Team 1 - Possesions                       Team 2 Possesions", {?PRINT_X_START + ?DOUBLE_TAB, ?PRINT_Y_START + 5 * ?TAB, ?PRINT_W_H, ?PRINT_W_H}),
      drawBallPossessionsLabel("Goalie      - ", WXPaint, Player1_0, Player2_0, ?DOUBLE_TAB),
      drawBallPossessionsLabel("Player 1   - ", WXPaint, Player1_1, Player2_1, 3 * ?TAB),
      drawBallPossessionsLabel("Player 2   - ", WXPaint, Player1_2, Player2_2, 4 * ?TAB),
      drawBallPossessionsLabel("Player 3   - ", WXPaint, Player1_3, Player2_3, 5 * ?TAB),
      drawBallPossessionsLabel("Player 4   - ", WXPaint, Player1_4, Player2_4, 6 * ?TAB),
      drawBallPossessionsLabel("Player 5   - ", WXPaint, Player1_5, Player2_5, 7 * ?TAB),
      drawBallPossessionsLabel("Player 6   - ", WXPaint, Player1_6, Player2_6, 8 * ?TAB),
      drawBallPossessionsLabel("Player 7   - ", WXPaint, Player1_7, Player2_7, 9 * ?TAB),
      drawBallPossessionsLabel("Player 8   - ", WXPaint, Player1_8, Player2_8, 10 * ?TAB),
      drawBallPossessionsLabel("Player 9   - ", WXPaint, Player1_9, Player2_9, 11 * ?TAB),
      wxDC:drawLabel(WXPaint, "Player 10 - " ++ integer_to_list(Player1_11) ++ "                                   Controlled Player - "
        ++ integer_to_list(ControlledPlayer), {?PRINT_X_START + 2 * ?TAB, ?PRINT_Y_START + 12 * ?TAB + ?BIGGER_INDENTATIONS, ?PRINT_W_H, ?PRINT_W_H});
    _ -> continue
  end.

drawBallPossessionsLabel(PlayerType, WXPaint, PlayerStatistics, Player2Statistics, Indentations) ->
  Text = PlayerType ++ integer_to_list(PlayerStatistics) ++ "                                   " ++ PlayerType ++ integer_to_list(Player2Statistics),
  wxDC:drawLabel(WXPaint, Text, {?PRINT_X_START + ?DOUBLE_TAB, ?PRINT_Y_START + Indentations + ?BIGGER_INDENTATIONS, ?PRINT_W_H, ?PRINT_W_H}).

whoIsTheWinner() ->
  [{_, TeamOnePoints}] = ets:lookup(statistics, teamOnePoints),
  if (TeamOnePoints == 3) -> 1;
    true -> 2
  end.

initializeBallLocation()->
  ets:insert(ball, {ballX, ?BALL_INIT_X}),
  ets:insert(ball, {ballY, ?BALL_INIT_Y}).