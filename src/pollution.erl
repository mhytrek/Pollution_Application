%%%-------------------------------------------------------------------
%%% @author michalinahytrek
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. mar 2024 17:15
%%%-------------------------------------------------------------------
-module(pollution).
-author("michalinahytrek").

-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, look_for_station/2, get_one_value/4, get_station_mean/3, get_daily_mean/3]).

-record(station, {name, coordinates}).
-record(measurement, {station_name, time, type, result}).

look_for_station(C, Stations) ->
  Which_station = fun
                    S([#station{coordinates = Cor, name = Name}|_], Cor) -> Name;
                    S([_|T], Cor) -> S(T,Cor);
                    S([], _) -> "" end,
  Which_station(Stations, C).


create_monitor() ->
  #{stations => [], measurements => []}.

add_station(Name, Coordinates, Monitor) ->
  Stations = maps:get(stations, Monitor),
  Same_values = fun
                  ({_,N, _}, N, _) -> true;
                  ({_,_, Co}, _, Co) -> true;
                  (_, _, _) -> false
                end,
  Bool_duplicates = [Same_values(X, Name, Coordinates) || X <- Stations],
  Result = fun
             (false) ->
               S = #station{name = Name, coordinates = Coordinates},
               NewStations = [S | Stations],
               NewMonitor = Monitor#{stations => NewStations},
               NewMonitor;
             (true) ->
               {error, "You cannot add this station due to duplicates"}
           end,
  Any_duplicates = lists:any(fun(C) -> C end, Bool_duplicates),
  Result(Any_duplicates).


add_value({V1,V2}, Date, Type, Value, Monitor) ->
  Stations = maps:get(stations, Monitor),
  Station_name = look_for_station({V1,V2}, Stations),
  add_value(Station_name,Date, Type, Value, Monitor);
add_value(S_name, Date, Type, Value, Monitor) ->
  Measurements = maps:get(measurements, Monitor),
  Stations = maps:get(stations, Monitor),

  Same_values = fun
                  (#station{name = N}, N) -> true;
                  (_, _) -> false
                end,
  Bool_duplicates_s = [Same_values(X, S_name) || X <- Stations],
  Station_exist = lists:any(fun(C) -> C end, Bool_duplicates_s),

  Same_measurements = fun
                        (#measurement{station_name = N, time = Ti, type = Ty}, N, Ti, Ty) -> true;
                        (_,_,_,_) -> false end,
  Bool_duplicates_m = [Same_measurements(X, S_name, Date, Type) || X <- Measurements],
  Measurement_exist = lists:any(fun(C) -> C end, Bool_duplicates_m),
  Result = fun
             (true, false) ->
               New_measurement = #measurement{station_name = S_name, time = Date, type = Type, result = Value},
               New_measurements = [New_measurement | Measurements],
               NewMonitor = Monitor#{measurements => New_measurements},
               NewMonitor;
             (false,false) ->
               {error, "That station do not exist"};
             (true,true) ->
               {error, "You cannot add this measurements due to duplicates"};
             (false,true) ->
               {error, "duplicates and not exisiting station"}
           end,
  Result(Station_exist, Measurement_exist).



remove_value({V1,V2},Date, Type, Monitor) ->
  Stations = maps:get(stations, Monitor),
  Station_name = look_for_station({V1,V2}, Stations),
  remove_value(Station_name,Date, Type, Monitor);
remove_value(Name, Date, Type, Monitor) ->
  Measurements = maps:get(measurements, Monitor),
  Look_for_Measurement = fun
                           (#measurement{station_name = N, time = D, type = T}, N, D, T) -> false;
                           (_,_,_,_) -> true end,
  NewMeasurements = [X || X<-Measurements, Look_for_Measurement(X, Name, Date, Type)],
  Result = fun
             (M, M) ->
               {error, "Nothing to delete"};
             (_,_) ->
               NewMonitor = Monitor#{measurements => NewMeasurements},
               NewMonitor end,
  Result(Measurements, NewMeasurements).


get_one_value({V1,V2}, Date, Type, Monitor) ->
  Stations = maps:get(stations, Monitor),
  Station_name = look_for_station({V1,V2}, Stations),
  get_one_value(Station_name, Date, Type, Monitor);
get_one_value(Name, Date, Type, Monitor) ->
  Measurements = maps:get(measurements, Monitor),
  Look_for_Measurement = fun
                           (#measurement{station_name = N, time = D, type = T}, N, D, T) -> true;
                           (_,_,_,_) -> false end,
  NewMeasurements = [X || X<-Measurements, Look_for_Measurement(X, Name, Date, Type)],
  Result = fun
             (M, M) ->
               {error, "Nothing to get"};
             (_,[#measurement{result = R}]) ->
               R end,
  Result([], NewMeasurements).

get_station_mean({V1,V2}, Type, Monitor) ->
  Stations = maps:get(stations, Monitor),
  Station_name = look_for_station({V1,V2}, Stations),
  get_station_mean(Station_name, Type, Monitor);
get_station_mean(Name,  Type, Monitor) ->
  Measurements = maps:get(measurements, Monitor),
  Look_for_Measurement = fun
                           (#measurement{station_name = N, type = T}, N, T) -> true;
                           (_,_,_) -> false end,
  Value = fun
            (#measurement{result=V}) -> V;
            (_) -> 0 end,
  NewMeasurements = [Value(X) || X<-Measurements, Look_for_Measurement(X, Name, Type)],
  Sum = lists:foldl(fun (X,Y) -> X+Y end, 0, NewMeasurements),
  Result = fun
             (0, 0) ->
               {error, "No data"};
             (L,S) ->
               S / L end,
  Result(length(NewMeasurements), Sum).


get_daily_mean(Type, Date, Monitor) ->
  Measurements = maps:get(measurements, Monitor),
  Look_for_Measurement = fun
                           (#measurement{time = {D,_}, type = T}, D, T) -> true;
                           (_,_, _) -> false end,
  Value = fun
            (#measurement{result=V}) -> V;
            (_) -> 0 end,
  NewMeasurements = [Value(X) ||  X<-Measurements, Look_for_Measurement(X, Date, Type)],
  Sum = lists:foldl(fun (X,Y) -> X+Y end, 0, NewMeasurements),
  Result1 = fun
             (0, 0) ->
               {error, "No data"};
             (L,S) ->
               S / L end,
  Result1(length(NewMeasurements), Sum).


get_location_value(Location, Monitor) ->
  Stations = maps:get(stations, Monitor),
  Distance = fun (#station{coordinates = {V1,V2}}, {L1,L2}) -> (V1-L1)*(V1-L1) + (V2-L2)(V2-L2) end,
  Stations_Distances = [Distance(X, Location) || X<-Stations],
  [A,B,C | _] = lists:sort(Stations_Distances),

  Closest = fun (A1, A1, _,_) -> true; (B1, _, B1, _) -> true; (C1, _, _, C1) -> true; (_,_,_,_) -> false end,
  Stations_closest = [X || X<- Stations, Closest(Distance(X,Location),A,B,C)],
  [S1,S2,S3] = Stations_closest,

  Measurements = maps:get(measurements, Monitor),
  Look_for_Measurement = fun
                           (#measurement{station_name = N}, #station{name = N}) -> true;
                           (_,_, _) -> false end,
  S1_measurements = [X || X<-Measurements, Look_for_Measurement(X, S1)],
  S2_measurements = [X || X<-Measurements, Look_for_Measurement(X, S2)],
  S3_measurements = [X || X<-Measurements, Look_for_Measurement(X, S3)],

  S1_sorted = lists:foldl(fun (X,[H|T]) when X < H -> [X,H|T]; (X,[H|T]) -> [[H,X,T]] end, [], S1_measurements),





























