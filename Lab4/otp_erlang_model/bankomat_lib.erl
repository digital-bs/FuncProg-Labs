-module(bankomat_lib).
-author("Semen").

%% API
-export([take_me_money/1]).

take_me_money(N) -> gen_event:notify(bankomat_event, {take_me_money, N}).
