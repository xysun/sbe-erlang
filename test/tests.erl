-module(tests).
-include_lib("eunit/include/eunit.hrl").

% simple utility tests
simple_test_() ->
    [simple_test_util_module(),
     simple_test_buffer_module()].

simple_test_util_module() ->
    [?_assertEqual([2,1,0], util:int_to_list(3)),
     ?_assertEqual([], util:int_to_list(0))].

simple_test_buffer_module() -> 
    [?_assertEqual(<<0, 0>>, buffer:allocate(2))].

% test different buffer methods
buffer_test_() -> 
    {foreach,
     fun allocate_buffer/0,
     fun tear_down/1, 
     [fun test_uint16Put/1]}.


allocate_buffer() -> buffer:allocate(8).
tear_down(_) -> ok.

test_uint16Put(Buffer) -> 
    [?_assertEqual(<<0, 0, 2, 1, 0, 0, 0, 0>>, buffer:uint16Put(Buffer, 2, 258))].
