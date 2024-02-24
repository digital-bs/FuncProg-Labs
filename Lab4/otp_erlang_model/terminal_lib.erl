-module(terminal_lib).
-author("Semen").

%% API
-export([print_ticket/1]).


print_ticket(N)-> gen_event:notify(terminal_event, {print_ticket, N}).
