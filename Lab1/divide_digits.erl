-module(divide_digits).
-export([getList/1, printList/1, printListParallel/1]).

getList(0) -> [0];
getList(N) when (N < 0) -> getList(-1*N);
getList(N) -> getNotNullList(N).

getNotNullList(0) -> [];
getNotNullList(N) -> getNotNullList(N div 10) ++ [N rem 10].

printList([]) ->
  io:format("List printed.~n");
printList([H|T]) ->
  spawn(fun() ->
    io:format("~p~n", [H]),
    printList(T)
        end).

joinNumbers(List) ->
  spawn(fun() ->
    Res = lists:foldl(
      fun(X, Acc) ->
        Acc * 10 + X
      end, 0, List),
    io:format("Joined number is ~p~n", [Res])
        end).

printListParallel(N) ->
  List = getList(N),
  _Pid = printList(List),
  joinNumbers(List).