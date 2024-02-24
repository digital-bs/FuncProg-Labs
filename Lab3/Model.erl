-module(Model).
-author("Semen").

-import(timer,[sleep/1]).
-export([queue/1, bank/0, client/0, terminal/0, operator/0, atm/0, start/0,
  checkBank/0, checkClient/0, checkOperator/0, checkTerminal/0,
  pingBank/1, pingClient/1, pingOperator/1, pingTerminal/1,
  startBank/0, startClient/0, startOperator/0, startTerminal/0, startAtm/0]).


checkOperator() -> pingOperator(net_adm:ping('operator@127.0.0.1')).

pingOperator(pang) -> io:format("Pang(Operator). ~n", []), timer:sleep(2000), checkOperator();
pingOperator(pong) -> io:format("Pong(Operator). ~n", []).

checkTerminal() -> pingTerminal(net_adm:ping('terminal@127.0.0.1')).

pingTerminal(pang) -> io:format("Pang(Terminal). ~n", []), timer:sleep(2000), checkTerminal();
pingTerminal(pong) -> io:format("Pong(Terminal). ~n", []).

checkClient() -> pingClient(net_adm:ping('client@127.0.0.1')).

pingClient(pang) -> io:format("Pang(Client). ~n", []), timer:sleep(2000), checkClient();
pingClient(pong) -> io:format("Pong(Client). ~n", []).

checkAtm() -> pingAtm(net_adm:ping('atm@127.0.0.1')).

pingAtm(pang) -> io:format("Pang(Atm). ~n", []), timer:sleep(2000), checkAtm();
pingAtm(pong) -> io:format("Pong(Atm). ~n", []).

checkBank() -> pingBank(net_adm:ping('bank@127.0.0.1')).

pingBank(pang) -> io:format("Pang(Bank). ~n", []), timer:sleep(2000), checkBank();
pingBank(pong) -> io:format("Pong(Bank). ~n", []).

startClient() -> global:register_name(client, spawn('client@127.0.0.1', func, client, [])), checkBank().

startTerminal() -> global:register_name(terminal, spawn('terminal@127.0.0.1', func, terminal, [])), checkBank().

startOperator() -> global:register_name(operator, spawn('operator@127.0.0.1', func, operator, [])),  checkBank().

startAtm() -> global:register_name(atm, spawn('atm@127.0.0.1', func, atm, [])), checkBank().

startBank() -> global:register_name(bank, spawn('bank@127.0.0.1', func, bank, [])), checkTerminal(), checkClient(), checkTerminal(), checkAtm().

bank() ->
  N = 10,
  global:whereis_name(client) ! {service_started, N},
  receive
    {finished, N} ->
      io:format("~s ~5..0B ~s ~n", ['Client', N, 'finished maintenance.']),
      io:format("*************************** ~n", []),
      bank()
  end.

queue(0) -> io:format("The bank was closed.~n", []);
queue(N) ->
  global:whereis_name(client) ! {service_started, N, self()},
  receive
    {finished, N} ->
      io:format("~s ~5..0B ~s ~n", ['Client', N, 'finished maintenance.']),
      io:format("*************************** ~n", [])
  end,
  queue(N-1).

client() ->
  receive
    {service_started, N} ->
      io:format("~s ~5..0B ~s ~n", ['Client', N, 'go to terminal.']),
      global:whereis_name(terminal) ! {print_ticket, N},
      client();
    {go_to_operator, N} ->
      io:format("~s ~5..0B ~s ~n", ['The client', N, 'went to the operator.']),
      global:whereis_name(operator) ! {give_me_card, N},
      client();
    {take_your_card, N} ->
      io:format("~s ~5..0B ~s ~n", ['The client', N, 'received the card.']),
      global:whereis_name(atm) ! {give_me_money, N},
      client();
    {money_charged, N} ->
      io:format("~s ~5..0B ~s ~n", ['The client', N, 'got  his money.']),
      global:whereis_name(bank) ! {finished, N},
      client()
  end.

atm() ->
  receive
    {give_me_money, N} ->
      io:format("~s ~5..0B~s ~n", ['The terminal gave money to client', N, '.']),
      global:whereis_name(client) ! {money_charged, N},
      atm()
  end.

terminal() ->
  receive
    {print_ticket, N} ->
      io:format("~s ~5..0B~s ~n", ['The terminal issued a ticket to the client', N, '.']),
      global:whereis_name(operator) ! {call_client, N},
      terminal()
  end.

operator() ->
  receive
    {call_client, N} ->
      io:format("~s ~5..0B~s ~n", ['The operator called the client', N, '.']),
      global:whereis_name(client) ! {go_to_operator, N},
      operator();
    {give_me_card, N} ->
      global:whereis_name(client) ! {take_your_card, N},
      operator()
  end.

start() ->  spawn(func, queue, [13]).

