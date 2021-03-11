%%%-------------------------------------------------------------------
%%% @author Moshe.Yuval
%%% @copyright Moshe & Yuval
%%% @doc
%%%
%%% @end
%%% Created : 28. Jul 2020 16:01
%%%-------------------------------------------------------------------
-module(monitor).
-author("Moshe.Yuval").

-behaviour(gen_server).

-include("../params.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([startme/4, addListOfPlayers/2, addAPlayer/2, addBall/2, refreshInfo/3, addControlledplayer/2, refreshActiveMonitors/5, refreshLocation/3, switchedMonitor/2, insertComponent/3, deleteAll/1, shutDown/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


-record(stateRecord, {monitorName, etsAll, etsToDelete, pid_ReceiveBlock, monitorId, monitorsNumber, monitorsNames}).


startme(Name, ID, MonitorsNumber, MonitorsNames) ->
  FunToSpawn = fun() ->
    receiveBlock(Name, [])
               end,
  PID_ReceiveBlock = spawn(FunToSpawn),
  group_leader(whereis(user), self()), % User is a server that responds to all messages defined in the I/O interface.
  gen_server:start_link({global, Name}, ?MODULE, [ID, Name, PID_ReceiveBlock, MonitorsNumber, MonitorsNames], []).

init([ID, Name, PID_ReceiveBlock, MonitorsNumber, MonitorsNames]) ->
  Self = self(),
  global:register_name(Name, Self),
  EtsToDelete = ets:new(etsToDelete, [set]),
  EtsAll = ets:new(etsAll, [set, public, named_table]),
  PID_ReceiveBlock ! {monitor, Self},
  busyWait_WxServer(),
  State = #stateRecord{monitorName = Name, etsAll = EtsAll, etsToDelete = EtsToDelete, pid_ReceiveBlock = PID_ReceiveBlock,
    monitorId = ID, monitorsNumber = MonitorsNumber, monitorsNames = MonitorsNames},
  erlang:send_after(?MONITOR_REFRESH_RATE, self(), refreshMonitor),
  {ok, State}.

refreshActiveMonitors(Name, ID, MonitorsNumber, MonitorsNames, ComponentsInMonitor) ->
  gen_server:cast({global, Name}, {refreshActiveMonitors, ID, MonitorsNumber, MonitorsNames, ComponentsInMonitor}).

refreshInfo(Name, BallOrPlayerID, Arguments) ->
  gen_server:cast({global, Name}, {refreshInfo, BallOrPlayerID, Arguments}).

insertComponent(Name, ID, Arguments) ->
  gen_server:cast({global, Name}, {insertComponent, ID, Arguments}).

addControlledplayer(Name, {ID, Location, OldOrNew}) ->
  gen_server:cast({global, Name}, {addControlledplayer, ID, Location, OldOrNew}).

addAPlayer(Name, {ID, Location, Dest}) ->
  gen_server:cast({global, Name}, {addAPlayer, ID, Location, Dest}).

addListOfPlayers(Name, List) ->
  gen_server:cast({global, Name}, {addListOfPlayers, List}).

addBall(Name, {ID, Location, Destination, Exist}) ->
  gen_server:cast({global, Name}, {addBall, ID, Location, Destination, Exist}).

refreshLocation(Name, BallOrPlayerID, Location) ->
  gen_server:cast({global, Name}, {refreshLocation, BallOrPlayerID, Location}).

switchedMonitor(Name, BallOrPlayerID) ->
  gen_server:cast({global, Name}, {switchedMonitor, BallOrPlayerID}).

deleteAll(Name) ->
  gen_server:cast({global, Name}, {deleteAll}).

shutDown(Name) ->
  gen_server:cast({global, Name}, {shutDown}).

busyWait_WxServer() ->
  IsDefined = global:whereis_name(main),
  case IsDefined of
    undefined -> busyWait_WxServer();
    _ -> ok
  end.

handle_call(_, _, Data) ->
  {reply, Data}.

handle_cast({addListOfPlayers, List}, Data) ->
  FunForEach = fun({ID, {_ObjType, Location, {Destination}}}) ->
    NameFromState = Data#stateRecord.monitorName,
    computerplayer:startme(ID, Location, Destination, NameFromState)
               end,
  lists:foreach(FunForEach, List),
  {noreply, Data};

handle_cast({addAPlayer, ID, Location, Destination}, Data) ->
  NameFromState = Data#stateRecord.monitorName,
  computerplayer:startme(ID, Location, Destination, NameFromState),
  {noreply, Data};

handle_cast({addBall, ID, Location, Destination, _}, Data) ->
  NameFromState = Data#stateRecord.monitorName,
  ball:startme(ID, Location, Destination, NameFromState),
  {noreply, Data};

handle_cast({addControlledplayer, ID, Location, _}, Data) ->
  controlledplayer:startme(ID, Location, Data#stateRecord.monitorName),
  {noreply, Data};

handle_cast({insertComponent, BallOrPlayerID, Arguments}, Data) ->
  MonitorID = ets:lookup(Data#stateRecord.etsAll, BallOrPlayerID),
  case MonitorID of
    [] ->
      ets:insert(Data#stateRecord.etsAll, {BallOrPlayerID, Arguments});
    _ -> continue
  end,
  {noreply, Data};

handle_cast({refreshInfo, PlayerID, Arguments}, Data) ->
  GetPlayer = ets:lookup(Data#stateRecord.etsAll, PlayerID),
  case GetPlayer of
    [] -> continue;
    [{_, {controlledplayer, _, _}}] ->
      {controlledplayer, Location, {MonitorName}} = Arguments,
      IsInside = isInsideCurrentScreen(Location, Data#stateRecord.monitorId, Data#stateRecord.monitorsNumber, controlledplayer),
      case IsInside of
        false ->
          controlledplayerSwitchMonitor(Data, PlayerID);
        true ->
          ets:insert(Data#stateRecord.etsAll, {PlayerID, {controlledplayer, Location, {MonitorName}}})
      end;
    [{_, {computerplayer, _, {_}}}] ->
      {ObjType, Location, {Destination}} = Arguments,
      IsInside = isInsideCurrentScreen(Location, Data#stateRecord.monitorId, Data#stateRecord.monitorsNumber, computerplayer),
      case IsInside of
        false ->
          computerplayerSwitchMonitor(Data, PlayerID);
        true ->
          ets:insert(Data#stateRecord.etsAll, {PlayerID, {ObjType, Location, {Destination}}})
      end;
    [{_, {ball, _, {_}}}] ->
      {ObjType, Location, {Destination}} = Arguments,
      IsInside = isInsideCurrentScreen(Location, Data#stateRecord.monitorId, Data#stateRecord.monitorsNumber, ball),
      case IsInside of
        false ->
          ballSwitchMonitor(Data, PlayerID);
        true ->
          ets:insert(Data#stateRecord.etsAll, {PlayerID, {ObjType, Location, {Destination}}})
      end
  end,
  {noreply, Data};

handle_cast({refreshActiveMonitors, ID, MonitorsNumber, MonitorsNames, ComponentsInMonitor}, Data) ->
  NewState = Data#stateRecord{monitorsNumber = MonitorsNumber, monitorId = ID, monitorsNames = MonitorsNames},
  AllPlayersL = ets:tab2list(Data#stateRecord.etsAll),
  ets:delete_all_objects(Data#stateRecord.etsAll),
  FunForEach1 = fun({PlayerID, {ObjType, _, _}}) ->
    case ObjType of
      controlledplayer -> controlledplayer:delete(PlayerID);
      computerplayer -> computerplayer:delete(PlayerID);
      ball -> ball:delete(PlayerID)
    end
                end,
  lists:foreach(FunForEach1, AllPlayersL),
  FunForEach2 = fun({_, {ObjType, Location, Args}, MyId}) ->
    case ObjType of
      controlledplayer ->
        controlledplayer:startme(controlledplayer, Location, Data#stateRecord.monitorName);
      computerplayer ->
        {Destination} = Args,
        computerplayer:startme(MyId, Location, Destination, Data#stateRecord.monitorName);
      ball ->
        {Destination} = Args,
        ball:startme(MyId, Location, Destination, Data#stateRecord.monitorName);
      _ -> continue
    end end,
  lists:foreach(FunForEach2, ComponentsInMonitor),
  {noreply, NewState};

handle_cast({refreshLocation, BallOrPlayerID, Location}, Data) ->
  controlledplayer:move(BallOrPlayerID, Location),
  {noreply, Data};

handle_cast({switchedMonitor, BallOrPlayerID}, Data) ->
  ets:delete(Data#stateRecord.etsAll, BallOrPlayerID),
  {noreply, Data};

handle_cast({deleteAll}, Data) ->
  etsDeleteAll(Data#stateRecord.etsAll),
  {noreply, Data};

handle_cast({shutDown}, Data) ->
  {stop, shutdown, Data};

handle_cast(_Result, Data) ->
  {noreply, Data}.

handle_info(refreshMonitor, Data) ->
  EtsAll = ets:tab2list(Data#stateRecord.etsAll),
  EtsToDelete = ets:tab2list(Data#stateRecord.etsToDelete),
  WxPID = global:whereis_name(main),
  wxserver:applyChangesInDataBase(EtsAll, EtsToDelete, Data#stateRecord.monitorName, WxPID),
  ets:delete_all_objects(Data#stateRecord.etsToDelete),
  erlang:send_after(?MONITOR_REFRESH_RATE, self(), refreshMonitor),
  {noreply, Data};

handle_info(_, Data) ->
  {noreply, Data}.

terminate(_, Data) ->
  Data#stateRecord.pid_ReceiveBlock ! kill,
  etsDeleteAll(Data#stateRecord.etsAll).

etsDeleteAll(ToDeleteEts) ->
  ToDeleteList = ets:tab2list(ToDeleteEts),
  ets:delete_all_objects(ToDeleteEts),
  FunToDel = fun({ID, {ObjType, _, _}}) ->
    case ObjType of
      controlledplayer -> controlledplayer:delete(ID);
      computerplayer -> computerplayer:delete(ID);
      ball -> ball:delete(ID)
    end
             end,
  lists:foreach(FunToDel, ToDeleteList).

receiveBlock(MonitorName, ToAdd) ->
  receive
    {monitor, MonitorPid} ->
      IsDefiend = global:whereis_name(main),
      wxserver:linkedMonitor(IsDefiend, MonitorName),
      erlang:monitor(process, MonitorPid),
      receiveBlock(MonitorName, ToAdd);
    kill ->
      stop;
    _ -> continue
  end.

controlledplayerSwitchMonitor(Data, PlayerID) ->
  controlledplayer:switchMonitor(PlayerID, Data#stateRecord.monitorsNumber, Data#stateRecord.monitorsNames).

computerplayerSwitchMonitor(Data, PlayerID) ->
  computerplayer:switchMonitor(PlayerID, Data#stateRecord.monitorsNumber, Data#stateRecord.monitorsNames).

ballSwitchMonitor(Data, PlayerID) ->
  ball:switchMonitor(PlayerID, Data#stateRecord.monitorsNumber, Data#stateRecord.monitorsNames).

isInsideCurrentScreen({X, _}, ID, NumOfMonitors, _) ->
  {LeftEdge, RightEdge} = checkWhichMonitor(ID, NumOfMonitors),
  ((X =< RightEdge) and (X >= LeftEdge)).

checkWhichMonitor(MonitorNum, MonitorsNumber) ->
  MonitorSize = (?X_Upper_Limit + 1) / MonitorsNumber,
  MonitorXEdgeMin = MonitorSize * (MonitorNum - 1),
  MonitorXEdgeMax = MonitorSize * MonitorNum - 1,
  case {MonitorXEdgeMin, MonitorXEdgeMax} of
    {0, ?X_Upper_Limit} ->
      Ret = {-100, ?X_Upper_Limit + 100};
    {0, _} ->
      Ret = {-100, MonitorXEdgeMax};
    {_, ?X_Upper_Limit} ->
      Ret = {MonitorXEdgeMin, ?X_Upper_Limit + 100};
    {_, _} ->
      Ret = {MonitorXEdgeMin, MonitorXEdgeMax}
  end,
  Ret.