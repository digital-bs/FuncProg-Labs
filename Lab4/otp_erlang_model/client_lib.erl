-module(client_lib).
-author("Semen").

%% API
-export([go_to_operator/1, take_your_card/1, take_your_money/1]).

go_to_operator(N) -> gen_event:notify(client_event, {go_to_operator, N}).

take_your_card(N) -> gen_event:notify(client_event, {take_your_card, N}).

take_your_money(N) -> gen_event:notify(client_event, {take_your_money, N}).

