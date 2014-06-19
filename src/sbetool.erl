-module(sbetool).
-export([main/1]).

-record(messageSchema, {package, % dir name
                         id,
                         version,
                         semanticVersion,
                         byteOrder="littleEndian", % endian
                         description,
                         headerType="messageHeader"}).

main([SchemaFile, OutDir|_]) -> 
    %xml_parser:parse("../schemas/example-schema-simple.xml").
    xml_parser:parse(SchemaFile, OutDir).
