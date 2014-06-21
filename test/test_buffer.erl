-module(test_buffer).
-include_lib("eunit/include/eunit.hrl").
% unittest for utils.erl and buffer.erl


% simple utility tests
util_test_() ->
    [test_allocate(),
     test_capfirst(),
     test_lowerfirst(),
     test_fetchWithDefault()].

test_allocate() -> 
    [?_assertEqual(<<0, 0>>, buffer:allocate(2))].

test_capfirst() ->
    [?_assertEqual("Hello", utils:capfirst("hello")),
     ?_assertEqual("Hello", utils:capfirst("Hello"))].

test_lowerfirst() ->
    [?_assertEqual("hello", utils:lowerfirst("Hello")),
     ?_assertEqual("hello", utils:lowerfirst("hello"))].

test_fetchWithDefault() -> 
    [?_assertEqual(2, utils:fetchWithDefault(a, dict:store(a, 2, dict:new()), 3)),
     ?_assertEqual(3, utils:fetchWithDefault(b, dict:store(a, 2, dict:new()), 3))].

% test different buffer put methods
buffer_put_test_() -> 
    {foreach,
     fun allocate_buffer/0,
     fun tear_down/1, 
     [fun test_int8Put/1, 
      fun test_uint8Put/1,
      fun test_uint16Put_little/1,
      fun test_uint16Put_big/1,
      fun test_int16Put_big/1, 
      fun test_int16Put_little/1]}.


allocate_buffer() -> buffer:allocate(8).
tear_down(_) -> ok.

test_int8Put(Buffer) ->
    [?_assertEqual(<<0, 0, 131, 0, 0, 0, 0, 0>>, buffer:int8Put(Buffer, 2, -125, little))].
test_uint8Put(Buffer) ->
    [?_assertEqual(<<0, 0, 2, 0, 0, 0, 0, 0>>, buffer:uint8Put(Buffer, 2, 2, little))].
test_uint16Put_big(Buffer) ->
    [?_assertEqual(<<0, 0, 1, 2, 0, 0, 0, 0>>, buffer:uint16Put(Buffer, 2, 258, big))].
test_uint16Put_little(Buffer) -> 
    [?_assertEqual(<<0, 0, 2, 1, 0, 0, 0, 0>>, buffer:uint16Put(Buffer, 2, 258, little))].
test_int16Put_big(Buffer) ->
    [?_assertEqual(<<0, 0, 255, 3, 0, 0, 0, 0>>, buffer:int16Put(Buffer, 2, -253, big))].
test_int16Put_little(Buffer) -> 
    [?_assertEqual(<<0, 0, 3, 255, 0, 0, 0, 0>>, buffer:int16Put(Buffer, 2, -253, little))].

% test different buffer get methods
buffer_get_test_() ->
    [test_int8Get(),
     test_uint8Get(), 
     test_uint16Get_big(), 
     test_uint16Get_little(),
     test_int16Get_big(),
     test_int16Get_little()].

test_int8Get() -> 
    [?_assertEqual(-125, buffer:int8Get(<<0, 0, 131, 0, 0, 0, 0, 0>>, 2, little))].

test_uint8Get() -> 
    [?_assertEqual(2, buffer:uint8Get(<<0, 0, 2, 0, 0, 0, 0, 0>>, 2, little))].

test_uint16Get_big() -> 
    [?_assertEqual(258, buffer:uint16Get(<<0, 0, 1, 2, 0, 0, 0, 0>>, 2, big))].

test_uint16Get_little() -> 
    [?_assertEqual(258, buffer:uint16Get(<<0, 0, 2, 1, 0, 0, 0, 0>>, 2, little))].

test_int16Get_big() -> 
    [?_assertEqual(-253, buffer:int16Get(<<0, 0, 255, 3, 0, 0, 0, 0>>, 2, big))].

test_int16Get_little() -> 
    [?_assertEqual(-253, buffer:int16Get(<<0, 0, 3, 255, 0, 0, 0, 0>>, 2, little))].

