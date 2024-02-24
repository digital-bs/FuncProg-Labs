-module(client_lib2).
-author("Semen").

%% API
-export([]).

-export([take_your_money/1]).

take_your_money(N) -> gen_event:notify(client_event, {not_take_your_money, N}).