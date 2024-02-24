-module(div_dig_tests).
-author("Semen").

-include_lib("eunit/include/eunit.hrl").

getList1_test() ->
  ?assertEqual(erlang_module:getList(123), [1,2,3]).

getList2_test() ->
  ?assertEqual(erlang_module:getList(0), [0]).

getList3_test() ->
  ?assertEqual(erlang_module:getList(-0), [0]).

getList4_test() ->
  ?assertEqual(erlang_module:getList(-12345), [1,2,3,4,5]).

getNotNullList1_test() ->
  ?assertEqual(erlang_module:getNotNullList(25321), [2,5,3,2,1]).

getNotNullList2_test() ->
  ?assertEqual(erlang_module:getNotNullList(0), []).
