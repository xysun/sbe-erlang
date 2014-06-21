% test parsing schema file
-module(test_parseschema).
-include_lib("eunit/include/eunit.hrl").

basic_schema_file_test() -> 
    sbetool:main(["test/resources/basic-schema.xml", "test/"]).

basic_variable_length_test() -> 
    sbetool:main(["test/resources/basic-variable-length-schema.xml", "test/"]).

multiple_messages_test() -> 
    sbetool:main(["test/resources/multiple-messages-schema.xml", "test/"]).
