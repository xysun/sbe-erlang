-module(test_buffer).
-include_lib("eunit/include/eunit.hrl").
% unittest for utils.erl and buffer.erl
% simple utility tests
simple_test_() ->
    [simple_test_buffer_module(),
     test_capfirst(),
     test_lowerfirst(),
     test_fetchWithDefault()].

simple_test_buffer_module() -> 
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

% test different buffer methods
buffer_test_() -> 
    {foreach,
     fun allocate_buffer/0,
     fun tear_down/1, 
     [fun test_uint16Put_little/1,
      fun test_uint16Put_big/1,
      fun test_int8Put/1, 
      fun test_uint8Put/1]}.


allocate_buffer() -> buffer:allocate(8).
tear_down(_) -> ok.

test_uint16Put_little(Buffer) -> 
    [?_assertEqual(<<0, 0, 2, 1, 0, 0, 0, 0>>, buffer:uint16Put(Buffer, 2, 258, little))].
test_uint16Put_big(Buffer) ->
    [?_assertEqual(<<0, 0, 1, 2, 0, 0, 0, 0>>, buffer:uint16Put(Buffer, 2, 258, big))].
test_int8Put(Buffer) ->
    [?_assertEqual(<<0, 0, 131, 0, 0, 0, 0, 0>>, buffer:int8Put(Buffer, 2, -125, little))].
test_uint8Put(Buffer) ->
    [?_assertEqual(<<0, 0, 2, 0, 0, 0, 0, 0>>, buffer:uint8Put(Buffer, 2, 2, little))].


