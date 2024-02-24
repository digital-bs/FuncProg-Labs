-module(operator_lib).
-author("Semen").

%% API
-export([call_client/1, take_me_card/1]).

call_client(N) -> gen_event:notify(operator_event, {call_client, N}).

take_me_card(N) -> gen_event:notify(operator_event, {take_me_card, N}).
