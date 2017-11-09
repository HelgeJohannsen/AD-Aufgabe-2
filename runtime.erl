%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Algorithmen und Datenstrukturen                       %%
%% Praktikumsaufgabe 2 : naives vs komplexes Sortieren   %%
%%                                                       %%
%% bearbeitet von Helge Johannsen und Christian Stüber   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(runtime).

%% API
-export([run_tests/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Laufzeit Tests der Algorithmen mit unterschiedlich    %%
%% sortierten Listen                                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Test

%%%%%%%%%%%%%%%%%
%%	Entrypoint %%
%%%%%%%%%%%%%%%%%
run_tests() ->
  ListSize = 12345,
  Min = 10,
  Max = 5000,
  SwitchNum = 8,

  Funcs = [
    {"insertion sort", fun ssort:insertionS/1},
    {"selection sort", fun ssort:selectionS/1},
    {"merge sort    ", fun ksort:msort/1},
    {"qsort left    ", fun(L) -> ksort:qsort(left, L, SwitchNum) end},
    {"qsort middle  ", fun(L) -> ksort:qsort(middle, L, SwitchNum) end},
    {"qsort right   ", fun(L) -> ksort:qsort(right, L, SwitchNum) end},
    {"qsort median  ", fun(L) -> ksort:qsort(median, L, SwitchNum) end},
    {"qsort random  ", fun(L) -> ksort:qsort(random, L, SwitchNum) end}
  ],

  Lists = [
    {"random list", fun() -> util:randomliste(ListSize) end},
    {"random minmax list", fun() -> util:randomlisteD(ListSize, Min, Max) end},
    {"sorted list", fun() -> util:sortliste(ListSize) end},
    {"reversed list", fun() -> util:resortliste(ListSize) end}
  ],

  io:format("Size: ~p , Min: ~p , Max: ~p , SwitchNum: ~p~n", [ListSize, Min, Max, SwitchNum]),
  test(Lists, Funcs),
  test_switchnum(Lists),
  io:format("~nDone.").

%%%%%%%%%%%%%%%%%%%%%%%

test(Lists, Functions) ->
  lists:foreach(fun
    ({Desc, ListGen}) ->
      io:format("~n~n===== Test ~p =====~n~n", [Desc]),
      lists:foreach(fun(DescFuncTuple) -> calcTime(ListGen(), DescFuncTuple) end, Functions)
  end, Lists).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Optimaler SwitchNum Test %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_switchnum(Lists) ->
  Pivots = [left, middle, right, median, random],
  lists:foreach(fun
    ({Desc, ListGen}) ->
      io:format("~n~n===== Test optimale SwitchNum ~p =====~n~n", [Desc]),
      lists:foreach(fun(Pivot) -> test_switchnum(Pivot, ListGen(), 1, 100, {nil,nil}) end, Pivots)
  end, Lists).

test_switchnum(Pivot, _, CurrentSwitchNum, LIM, {SwitchNum, _Time}) when CurrentSwitchNum > LIM->
  io:format("Pivot: ~p , BestSwitchNum: ~p~n", [Pivot, SwitchNum]);

test_switchnum(Pivot, L, CurrentSwitchNum, LIM, Best) ->
  StartTime = erlang:timestamp(),
  ksort:qsort(Pivot, L, CurrentSwitchNum),
  StopTime = erlang:timestamp(),
  Duration = round(timer:now_diff(StopTime,StartTime)/1000),
  NewBest = case Best of
              {nil,nil} -> {CurrentSwitchNum, Duration};
              {_, Time} when Duration < Time -> {CurrentSwitchNum, Duration};
              _ -> Best
            end,
  io:format("."),
  test_switchnum(Pivot, L, CurrentSwitchNum+1, LIM, NewBest).

%%%%%%%%%%%%%%%%%%%%%%%

calcTime(ListToSort, {Description, Func}) ->
  StartTime = erlang:timestamp(),
  ReturnedList = Func(ListToSort),
  StopTime = erlang:timestamp(),
  Successful = (ReturnedList == lists:sort(ListToSort)),
  Duration = round(timer:now_diff(StopTime,StartTime)/1000),
  io:format(lists:concat([Description," - Sortiert: ", Successful, "\t\tDauer in ms: ", Duration, "~n"])).

