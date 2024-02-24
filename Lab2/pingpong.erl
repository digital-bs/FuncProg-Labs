-module(test).
-export([start/0, my_function/2, main/0, ping/1, pong/0]).

my_function(Number, ParentPid) ->
  List = lists:seq(1, Number),
  io:format("Received number ~w. Returning list: ~w~n", [Number, List]),
  ParentPid ! List.

start() ->
  spawn(fun() -> my_function(10, self()) end),
  receive
    Response -> io:format("Received response: ~w~n", [Response])
  after
    5000 -> io:format("Timeout occurred!~n")
  end.

main() ->
  spawn(fun() ->
    ping(spawn(fun()-> pong() end))
        end).

ping(PidPong)->
  io:format("I'm ping, pids are : ~w ~w ~n ", [PidPong, self()]),
  PidPong ! {ping, self()},
  io:format("waiting for pong...~n"),
  receive
    pong -> io:format("PONG received~n")
  end.

pong()->
  io:format("I'm pong, my pid is : ~w ~n ", [self()]),
  receive
    {ping , PidPing} ->
      io:format("PING received, sending pong~n"),
      timer:sleep(2000),
      PidPing ! pong
  end.

