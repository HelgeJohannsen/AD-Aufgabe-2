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

  io:format("Size: ~p , Min: ~p , Max: ~p , SwitchNum: ~p~n", [ListSize, Min, Max, SwitchNum]),

  test_random(ListSize, Funcs),
  test_randomMinMax(ListSize, Min, Max, Funcs),
  test_sorted(ListSize, Funcs),
  test_reverse(ListSize, Funcs),
  test_switchnum(ListSize),

  io:format("~nDone.").

%%%%%%%%%%%%%%%%%%%%%%%
%% Listen unsortiert %%
%%%%%%%%%%%%%%%%%%%%%%%

test_random(ListSize, Functions) ->
  RandomList = util:randomliste(ListSize),
  SRandomList = lists:sort(RandomList),
  io:format("~n~n===== Test zufaellige Liste =====~n~n"),
  lists:foreach(fun(DescFuncTuple) -> calcTime(RandomList, SRandomList, DescFuncTuple) end, Functions).

test_randomMinMax(ListSize, Min, Max, Functions) ->
  RandomList = util:randomlisteD(ListSize,Min,Max),
  SRandomList = lists:sort(RandomList),
  io:format(lists:concat(["~n~n===== Test Random mit Min: ",Min," und Max: ",  Max, " =====~n~n"])),
  lists:foreach(fun(DescFuncTuple) -> calcTime(RandomList, SRandomList, DescFuncTuple) end, Functions).

%%%%%%%%%%%%%%%%%%%%%%%
%% Listen sortiert   %%
%%%%%%%%%%%%%%%%%%%%%%%

test_sorted(ListSize, Functions) ->
  RandomList = util:sortliste(ListSize),
  SRandomList = lists:sort(RandomList),
  io:format("~n~n===== Test sortierte Liste =====~n~n"),
  lists:foreach(fun(DescFuncTuple) -> calcTime(RandomList, SRandomList, DescFuncTuple) end, Functions).

%%%%%%%%%%%%%%%%%%%%%%%
%% Listen reverse    %%
%%%%%%%%%%%%%%%%%%%%%%%

test_reverse(ListSize, Functions) ->
  RandomList = util:resortliste(ListSize),
  SRandomList = lists:sort(RandomList),
  io:format("~n~n===== Test reversierte Liste =====~n~n"),
  lists:foreach(fun(DescFuncTuple) -> calcTime(RandomList, SRandomList, DescFuncTuple) end, Functions).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Optimaler SwitchNum Test %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Testing SwitchNum 1 to 100

test_switchnum(ListSize) ->
  RandomList = util:resortliste(ListSize),
  io:format("~n~n===== Test optimale SwitchNum =====~n~n"),
  Pivots = [left,middle,right,median,random],
  lists:foreach(fun(Pivot) -> test_switchnum(Pivot, RandomList, 1, {nil,nil}) end, Pivots).

test_switchnum(Pivot, _, 101, {SwitchNum, _Time}) ->
  io:format("~nPivot: ~p , BestSwitchNum: ~p~n", [Pivot, SwitchNum]);

test_switchnum(Pivot, L, CurrentSwitchNum, Best) ->
  io:format("~p ", [CurrentSwitchNum]),
  StartTime = erlang:timestamp(),
  ksort:qsort(Pivot, L, CurrentSwitchNum),
  StopTime = erlang:timestamp(),
  Duration = round(timer:now_diff(StopTime,StartTime)/1000),
  NewBest = case Best of
              {nil,nil} -> {CurrentSwitchNum, Duration};
              {_, Time} when Duration < Time -> {CurrentSwitchNum, Duration};
              _ -> Best
            end,
  test_switchnum(Pivot, L, CurrentSwitchNum+1, NewBest).

%%%%%%%%%%%%%%%%%%%%%%%

calcTime(ListToSort, SortedList, {Description, Func}) ->
  StartTime = erlang:timestamp(),
  ReturnedList = Func(ListToSort),
  StopTime = erlang:timestamp(),
  Successful = ReturnedList == SortedList,
  Duration = round(timer:now_diff(StopTime,StartTime)/1000),
  io:format(lists:concat([Description," - Sortiert: ",Successful, "\t\tDauer in ms: ", Duration, "~n"])).

