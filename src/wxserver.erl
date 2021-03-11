%%%-------------------------------------------------------------------
%%% @author Moshe.Yuval
%%% @copyright Moshe & Yuval
%%% @doc
%%%
%%% @end
%%% Created : 15. Aug 2020 13:45
%%%-------------------------------------------------------------------
-module(wxserver).
-author("Moshe.Yuval").

-behaviour(wx_object).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("wx/include/wx.hrl").
-include("../params.hrl").

-export([startme/0, applyChangesInDataBase/4, linkedMonitor/2, finishedGame/1, serverDown/1]).
-export([init/1, handle_event/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(wXRecord, {wXPanel, wxFrame, wxBall, self, round}).


startme() ->
  compile:file(ball),
  compile:file(controlledplayer),
  compile:file(computerplayer),
  compile:file(common),
  compile:file(monitor),
  compile:file(utils),
  compile:file(etsutils),
  compile:file(wxutils),
  WX = wx:new(),
  wx_object:start_link({global, main}, ?MODULE, WX, []).

applyChangesInDataBase(EntriesToChange, WhatToDelete, MonitorName, ID) ->
  gen_server:cast(ID, {applyChangesInDataBase, EntriesToChange, WhatToDelete, MonitorName}).

linkedMonitor(ID, MonitorName) ->
  gen_server:cast(ID, {linkedMonitor, MonitorName}).

finishedGame(ID) ->
  gen_server:cast(ID, {finishedGame}).

serverDown(ID) ->
  gen_server:cast(ID, {serverDown}).

init(ID) ->
  FunToBatch = fun() -> initialize(ID) end,
  wx:batch(FunToBatch).

initialize(ID) ->
  IsMainDefined = whereis(main),
  if IsMainDefined /= undefined ->
    unregister(main),
    rpc:multicall(?MONITOR_PROCESSES, global, unregister, [main]);
    true -> continue
  end,
  global:register_name(main, self()),
  wxutils:setUpConnectionWithAllServers(),
  etsutils:generateEts(),
  FunToSpawn1 = fun() ->
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [controlledplayer]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [computerplayer]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [monitor]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [common]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [ball]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [wxserver]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [utils]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [etsutils]),
    rpc:multicall(?MONITOR_PROCESSES, compile, file, [wxutils]),
    rpc:call(?MONITOR_LONG_NAME_A, monitor, startme, [?MONITORA, 1, 4, [?MONITORA, ?MONITORB, ?MONITORC, ?MONITORD]]),
    rpc:call(?MONITOR_LONG_NAME_B, monitor, startme, [?MONITORB, 2, 4, [?MONITORA, ?MONITORB, ?MONITORC, ?MONITORD]]),
    rpc:call(?MONITOR_LONG_NAME_C, monitor, startme, [?MONITORC, 3, 4, [?MONITORA, ?MONITORB, ?MONITORC, ?MONITORD]]),
    rpc:call(?MONITOR_LONG_NAME_D, monitor, startme, [?MONITORD, 4, 4, [?MONITORA, ?MONITORB, ?MONITORC, ?MONITORD]]) end,
  FunToSpawn2 = fun() -> wxutils:superviseAllMonitors() end,
  spawn(FunToSpawn1),
  spawn(FunToSpawn2),
  WXFrame = wxFrame:new(ID, -1, "FIFA Distrabuted Game", [{size, ?SCREEN_SIZE}]),
  WXPanel = wxPanel:new(WXFrame, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
  wxFrame:createStatusBar(WXFrame),
  wxFrame:connect(WXPanel, motion),
  wxFrame:connect(WXFrame, command_menu_selected, []),
  wxFrame:connect(WXPanel, left_up),
  wxWindow:setBackgroundColour(WXFrame, ?BACKGROUND_COLOR),
  wxFrame:show(WXFrame),
  WXBall = wxImage:new(?BALL_IMAGE),
  WXRecord = #wXRecord{wXPanel = WXPanel, wxFrame = WXFrame, wxBall = WXBall, self = self(), round = 0},
  WXIMGBackground = wxImage:new(?STAGE_IMAGE),
  FunForConnectwxFrame = fun(#wx{event = #wxPaint{}}, _wxObj) ->
    WxBufferedPaint = wxBufferedPaintDC:new(WXPanel),
    ets:insert(statistics, {180, WxBufferedPaint}),
    WXBitmap = wxBitmap:new(WXIMGBackground),
    wxDC:drawBitmap(WxBufferedPaint, WXBitmap, {0, 0}),
    wxBitmap:destroy(WXBitmap),
    wxDC:setTextForeground(WxBufferedPaint, ?wxBLACK),
    wxutils:mainLoop(WxBufferedPaint),
    wxBufferedPaintDC:destroy(WxBufferedPaint) end,
  wxFrame:connect(WXPanel, paint, [{callback, FunForConnectwxFrame}]),
  timer:send_interval(?SCREEN_REFRESH_TIME, self(), refreshWindow),
  {WXPanel, WXRecord}.

handle_event(#wx{obj = _, event = #wxMouse{type = motion, x = XCoordinate, y = YCoordinate}}, State) ->
  case ets:lookup(playersETS, controlledplayer) of
    [] -> continue;
    [{_, {_, {_, _}, {MonitorName}}, _}] ->
      DistanceLowerX = ?X_Lower_Limit + ?CHARACTER_WIDTH,
      DistanceUpperX = ?X_Upper_Limit - ?CHARACTER_WIDTH,
      DistanceLowerY = ?Y_Lower_Limit + ?CHARACTER_WIDTH,
      DistanceUpperY = ?Y_Upper_Limit - ?CHARACTER_WIDTH,
      case XCoordinate < DistanceLowerX of
        false -> X1Coordinate = XCoordinate;
        _ -> X1Coordinate = DistanceLowerX
      end,
      X2Coordinate = X1Coordinate,
      case X2Coordinate > DistanceUpperX of
        false -> X3Coordinate = X2Coordinate;
        _ -> X3Coordinate = DistanceUpperX
      end,
      case YCoordinate < DistanceLowerY of
        false -> Y1Coordinate = YCoordinate;
        _ -> Y1Coordinate = DistanceLowerY
      end,
      Y2Coordinate = Y1Coordinate,
      case Y2Coordinate > DistanceUpperY of
        false -> Y3Coordinate = Y2Coordinate;
        _ -> Y3Coordinate = DistanceUpperY
      end,
      {X4Coordinate, Y4Coordinate} = {X3Coordinate, Y3Coordinate},
      monitor:refreshLocation(MonitorName, controlledplayer, {X4Coordinate, Y4Coordinate})
  end,
  {noreply, State};

handle_event(#wx{event = #wxMouse{type = left_up}}, Data) ->
  [{_, Status}] = ets:lookup(generalState, status),
  case Status of
    startGame ->
      wxutils:startRound(1, true),
      ets:insert(generalState, {status, idle});
    resetRound ->
      wxutils:resetRound(),
      ets:insert(generalState, {status, idle});
    finishedGame ->
      stay;
    idle ->
      wxutils:kick(Data#wXRecord.wxBall);
    _ -> continue
  end,
  {noreply, Data};

handle_event(#wx{}, State) ->
  {noreply, State}.

handle_call(_, _, State) ->
  {stop, normal, ok, State}.

handle_cast({applyChangesInDataBase, EntriesToChange, WhatToDelete, _}, State) ->
  [{_, Status}] = ets:lookup(generalState, status),
  case Status of
    idle ->
      FunInsertPlayers = fun() ->
        lists:foreach(
          fun({ID, Args}) -> [{_, Status}] = ets:lookup(generalState, status),
            if Status =:= idle ->
              ets:insert(playersETS, {ID, Args, ID})
            end
          end, EntriesToChange) end,
      FunDelPlayers = fun() -> lists:foreach(fun({ID, _}) ->
        ets:delete(playersETS, ID) end, WhatToDelete) end,
      spawn(FunDelPlayers),
      spawn(FunInsertPlayers);
    _ -> continue
  end,
  {noreply, State};

handle_cast({linkedMonitor, MonitorName}, Data) ->
  case MonitorName of
    ?MONITORA ->
      insertMonitor(?MONITORA);
    ?MONITORB ->
      insertMonitor(?MONITORB);
    ?MONITORC ->
      insertMonitor(?MONITORC);
    ?MONITORD ->
      insertMonitor(?MONITORD)
  end,
  {noreply, Data};

handle_cast({serverDown}, State) ->
  {stop, shutdown, State};

handle_cast(finishedGame, State) ->
  ets:insert(generalState, {status, finishedGame}),
  wxutils:getMonitorsAndDeleteAll(),
  ets:delete_all_objects(playersETS),
  {noreply, State};

handle_cast(_Cast, State) ->
  {noreply, State}.

insertMonitor(MonitorName) ->
  case ets:lookup(monitors, MonitorName) of
    [] ->
      ets:insert(monitors, {MonitorName, {true}}),
      List = ets:tab2list(monitors),
      case length(List) of
        4 -> ets:insert(generalState, {status, startGame});
        _ -> continue
      end;
    [{MonitorName, {false}}] ->
      wxutils:regainConnectionToMonitor(MonitorName),
      ets:insert(monitors, {MonitorName, {true}})
  end.

handle_info(refreshWindow, Data) ->
  wxWindow:refresh(Data#wXRecord.wXPanel, [{eraseBackground, false}]),
  {noreply, Data};

handle_info(_, Data) ->
  {noreply, Data}.

code_change(_, _, Data) ->
  {stop, ignore, Data}.

terminate(_, Data) ->
  FunGetMonitors = fun({_, {A}}) -> A end,
  MList = ets:tab2list(monitors),
  ListOfMonitors = lists:filter(FunGetMonitors, MList),
  lists:foreach(fun({Monitor, _}) ->
    monitor:shutDown(Monitor) end, ListOfMonitors),
  PIDA = whereis(?MONITORA),
  PIDB = whereis(?MONITORB),
  PIDC = whereis(?MONITORC),
  PIDD = whereis(?MONITORD),
  if PIDA /= undefined ->
    PIDA ! kill;
    true -> continue
  end,
  if PIDB /= undefined ->
    PIDB ! kill;
    true -> continue
  end,
  if PIDC /= undefined ->
    PIDC ! kill;
    true -> continue
  end,
  if PIDD /= undefined ->
    PIDD ! kill;
    true -> continue
  end,
  wxPanel:destroy(Data#wXRecord.wXPanel),
  wx:destroy(),
  ok.
