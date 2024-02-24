-module(div_dig_pl).
-author("Semen").



%% API
-export([get_digits/1, main/1, getList/1, pong/0]).

getList(0) -> [0];
getList(N)
  when (N < 0)
  -> getList(-1*N);
getList(N) ->
  getNotNullList(N).
getNotNullList(0)-> [];
getNotNullList(N)->
  getNotNullList(N div 10) ++ [N rem 10].


get_digits(N) ->
  Digits = trunc(math:log10(N)) + 1,
  {ok, Digits}.

pong()->
  io:format("I'm pong, my pid is : ~w ~n ", [self()]),
  receive
    {Half , PidPing} ->
      io:format("ping is : ~w ~n ", [Half]),
      Result = getList(Half),
      PidPing ! {Result, self()}
  end.


main(N) ->
  {ok, Digits} = get_digits(N),
  HalfDigits = Digits div 2,
  if
    Digits rem 2 == 0 ->
      FirstHalf = trunc(N / math:pow(10, HalfDigits)),
      SecondHalf = trunc(N - FirstHalf * math:pow(10, HalfDigits));
    true ->
      FirstHalf = trunc(N / math:pow(10, HalfDigits + 1)),
      SecondHalf = trunc(N - FirstHalf * math:pow(10, HalfDigits + 1))
  end,
  io:fwrite("~w~n",[FirstHalf]),
  io:fwrite("~w~n",[SecondHalf]),
  Pid1 = spawn(fun() ->pong() end),
  Pid2 = spawn(fun() ->pong() end),
  Pid1 ! {FirstHalf, self()},
  Pid2 ! {SecondHalf, self()},
  receive
     {Result1, Pid1} -> ok
  end,
  receive
    {Result2, Pid2} -> ok
  end,
  io:fwrite("~w~n",[Result1]),
  io:fwrite("~w~n",[Result2]),
  CombinedList = Result1 ++ Result2,
  io:format("~p~n", [CombinedList]).


