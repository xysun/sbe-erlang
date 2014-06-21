-module(sbetool).
-export([main/1]).

main([SchemaFile, OutDir|_]) -> 
    xml_parser:parse(SchemaFile, OutDir).
