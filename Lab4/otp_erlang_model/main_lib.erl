-module(main_lib).
-author("Semen").

%% API
-export([client_go_to_bank/1, client_go_to_home/1, bank_closed/0,checker/0, checker/1]).
checker() -> 0.
checker(N) -> gen_event:notify(main_event, {checker, N}).


client_go_to_bank(N) ->	gen_event:notify(main_event, {client_go_to_bank, N}).

client_go_to_home(N) ->	gen_event:notify(main_event, {client_go_to_home, N}).

bank_closed() -> gen_event:notify(main_event, {bank_closed}).
