%%%-------------------------------------------------------------------
%%% @author michalinahytrek
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. kwi 2024 14:49
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("michalinahytrek").

%% API
-export([init/0, create_monitor/0, loop/1, start/0, stop/0, add_value/4,remove_value/3, add_station/2, get_daily_mean/2, get_one_value/3, get_location_value/2, get_station_mean/2]).

start() ->
  register (pollutionServer, spawn(pollution_server, init, [])).

stop() ->
  pollutionServer ! stop.

init() ->
  loop(none).

loop(Monitor) ->
  receive
    stop -> ok;
    {createMonitor} ->
      NewMonitor = pollution:create_monitor(),
      case NewMonitor of
        {error, Message} ->
          io:format("ERROR: ~s. ~n", [Message]),
          pollution_server:loop(Monitor);
        _ ->
          pollution_server:loop(NewMonitor)
      end;
    {addStation, N, C} ->
      NewMonitor = pollution:add_station(N,C,Monitor),
      case NewMonitor of
        {error, Message} ->
          io:format("ERROR: ~s. ~n", [Message]),
          pollution_server:loop(Monitor);
        _ ->
          pollution_server:loop(NewMonitor)
      end;
    {addValue, S, D, T, V} ->
      NewMonitor = pollution:add_value(S, D, T, V, Monitor),
      case NewMonitor of
        {error, Message} ->
          io:format("ERROR: ~s. ~n", [Message]),
          pollution_server:loop(Monitor);
        _ ->
          pollution_server:loop(NewMonitor)
      end;
    {removeValue, N, D, T} ->
      NewMonitor = pollution:remove_value(N, D, T, Monitor),
      case NewMonitor of
        {error, Message} ->
          io:format("ERROR: ~s. ~n", [Message]),
          pollution_server:loop(Monitor);
        _ ->
          pollution_server:loop(NewMonitor)
      end;
    {getOneValue, N, D, T, PID} ->
      Value = pollution:get_one_value(N, D, T, Monitor),
      case Value of
        {error, Message} ->
          io:format("ERROR: ~s. ~n", [Message]);
        _ ->
          ok
      end,
      PID ! Value,
      pollution_server:loop(Monitor);
    {getStationMean, N,  T, PID} ->
      Value = pollution:get_station_mean(N, T, Monitor),
      case Value of
        {error, Message} ->
          io:format("ERROR: ~s. ~n", [Message]);
        _ ->
          ok
      end,
      PID ! Value,
      pollution_server:loop(Monitor);
    {getDailyMean, T, D, PID} ->
      Value = pollution:get_daily_mean(T, D, Monitor),
      case Value of
        {error, Message} ->
          io:format("ERROR: ~s. ~n", [Message]);
        _ ->
          ok
      end,
      PID ! Value,
      pollution_server:loop(Monitor);
    {getLocationValue, L, T, PID} ->
      Value = pollution:get_daily_mean(L, T, Monitor),
      case Value of
        {error, Message} ->
          io:format("ERROR: ~s. ~n", [Message]);
        _ ->
          ok
      end,
      PID ! Value,
      pollution_server:loop(Monitor)
  end.

create_monitor() ->
  pollutionServer ! {createMonitor}.

add_station(Name, Coordinates) ->
  pollutionServer ! {addStation, Name, Coordinates}.

add_value(S_name, Date, Type, Value) ->
  pollutionServer ! {addValue, S_name, Date, Type, Value}.

remove_value(Name, Date, Type) ->
  pollutionServer ! {removeValue, Name, Date, Type}.

get_one_value(Name, Date, Type) ->
  pollutionServer ! {getOneValue, Name, Date, Type, self()},
  receive Result -> Result end.

get_station_mean(Name,  Type) ->
  pollutionServer ! {getStationMean, Name,  Type, self()},
  receive Result -> Result end.

get_daily_mean(Type, Date) ->
  pollutionServer ! {getDailyMean, Type, Date, self()},
  receive Result -> Result end.

get_location_value(Location, Type) ->
  pollutionServer ! {getLocationValue, Location, Type, self()},
  receive Result -> Result end.

