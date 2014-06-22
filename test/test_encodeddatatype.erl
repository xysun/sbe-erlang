% test various encoded data type generation
-module(test_encodeddatatype).
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("../src/types.hrl").

multiple_types_test() -> 
    Xmlstring = "<messageSchema>"
              ++"    <types>"
              ++"        <type name='testType1' primitiveType='char'/>"
              ++"        <type name='testType2' primitiveType='int8' length='2'/>"
              ++"    </types>"
              ++"</messageSchema>",
    {Root, _} = xmerl_scan:string(Xmlstring),
    TypeMap = xml_parser:findTypes(Root),
    ?assert(dict:is_key("testType1", TypeMap)),
    ?assert(dict:is_key("testType2", TypeMap)),

    Type1 = dict:fetch("testType1", TypeMap),
    ?assertEqual(Type1#simpleType.primitiveType, "char"),
    ?assertEqual(Type1#simpleType.name, "testType1"),
    ?assertEqual(Type1#simpleType.length, 1),


    Type2 = dict:fetch("testType2", TypeMap),
    ?assertEqual(Type2#simpleType.primitiveType, "int8"),
    ?assertEqual(Type2#simpleType.name, "testType2"),
    ?assertEqual(Type2#simpleType.length, 2).

% should return correct size for primitive types
primitive_type_size_test() ->
    Xmlstring = "<messageSchema>"
              ++"    <types>"
              ++"       <type name='testTypeChar' primitiveType='char'/>"
              ++"       <type name='testTypeInt8' primitiveType='int8'/>"
              ++"       <type name='testTypeInt16' primitiveType='int16'/>"
              ++"       <type name='testTypeInt32' primitiveType='int32'/>"
              ++"       <type name='testTypeInt64' primitiveType='int64'/>"
              ++"       <type name='testTypeUInt8' primitiveType='uint8'/>"
              ++"       <type name='testTypeUInt16' primitiveType='uint16'/>"
              ++"       <type name='testTypeUInt32' primitiveType='uint32'/>"
              ++"       <type name='testTypeUInt64' primitiveType='uint64'/>"
              ++"    </types>"
              ++"</messageSchema>",
    {Root, _} = xmerl_scan:string(Xmlstring),
    TypeMap = xml_parser:findTypes(Root),
    
    TypeChar   = dict:fetch("testTypeChar",   TypeMap),
    TypeInt8   = dict:fetch("testTypeInt8",   TypeMap),
    TypeInt16  = dict:fetch("testTypeInt16",  TypeMap),
    TypeInt32  = dict:fetch("testTypeInt32",  TypeMap),
    TypeInt64  = dict:fetch("testTypeInt64",  TypeMap),
    TypeUInt8  = dict:fetch("testTypeUInt8",  TypeMap),
    TypeUInt16 = dict:fetch("testTypeUInt16", TypeMap),
    TypeUInt32 = dict:fetch("testTypeUInt32", TypeMap),
    TypeUInt64 = dict:fetch("testTypeUInt64", TypeMap),

    ?assertEqual(TypeChar#simpleType.size,   1),
    ?assertEqual(TypeInt8#simpleType.size,   1),
    ?assertEqual(TypeInt16#simpleType.size,  2),
    ?assertEqual(TypeInt32#simpleType.size,  4),
    ?assertEqual(TypeInt64#simpleType.size,  8),
    ?assertEqual(TypeUInt8#simpleType.size,  1),
    ?assertEqual(TypeUInt16#simpleType.size, 2),
    ?assertEqual(TypeUInt32#simpleType.size, 4),
    ?assertEqual(TypeUInt64#simpleType.size, 8).


