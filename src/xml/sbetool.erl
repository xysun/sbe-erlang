-module(sbetool).
-compile(export_all).

-record(messageSchema, {package, % dir name
                         id,
                         version,
                         semanticVersion,
                         byteOrder="littleEndian", % endian
                         description,
                         headerType="messageHeader"}).

main() -> 
    xml_parser:parse("../example-schema-simple.xml").
