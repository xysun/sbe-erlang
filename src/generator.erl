-module(generator).
-include("types.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-compile(export_all).

% main function
generate(MessageSchema, TypeMap, MessageMap, OutDir) -> 
    lists:map(
        fun(ID) -> generateMessageStub(MessageSchema, TypeMap, dict:fetch(ID, MessageMap), OutDir) end,
        dict:fetch_keys(MessageMap)).


% generate message.erl for single message
generateMessageStub(MessageSchema, TypeMap, MessageNode, OutDir) -> 
    MessageAttributes = lists:foldl(
        fun(#xmlAttribute{} = T, Acc) -> dict:store(T#xmlAttribute.name, T#xmlAttribute.value, Acc) end,
        dict:new(),MessageNode#xmlElement.attributes),
    
    ModuleName = utils:lowerfirst(dict:fetch(name, MessageAttributes)),
    FilePath = filename:join([OutDir, ModuleName ++ ".erl"]),
    filelib:ensure_dir(FilePath),
    {ok, IoDevice} = file:open(FilePath, [write]),
    
    generateFileHeader(IoDevice, ModuleName),
    generateMetaFunctions(IoDevice, MessageSchema, MessageAttributes),

    % generate methods for fixed-length data types
    Fields = xmerl_xpath:string("field", MessageNode),
    BlockLength = lists:foldl(
        fun(Field, Acc) -> Acc + generateFixedLengthMethods(IoDevice, MessageSchema, Field, TypeMap, Acc) end,
        0, Fields),

    io:format(IoDevice, 
        "~n~nsbeBlockLength() -> ~w.", [BlockLength]),

    % generate methods for variable-length string
    DataNodes = xmerl_xpath:string("data", MessageNode),
    lists:foreach(
        fun(DataNode) -> generateVarDataMethods(IoDevice, MessageSchema, DataNode, TypeMap) end, DataNodes),
        
    file:close(IoDevice).


% generate methods for variable-length string

generateVarDataMethods(IoDevice, MessageSchema, DataNode, TypeMap) -> 
    Endian = getSchemaEndian(MessageSchema),
    Attributes = utils:getAttributesDict(DataNode),
    Name = dict:fetch(name, Attributes),
    TypeName = dict:fetch(type, Attributes),
    Type = dict:fetch(TypeName, TypeMap),
    LengthPrimitiveTypeName = Type#varDataType.lengthPrimitiveType,
    LengthPrimitiveType = dict:fetch(LengthPrimitiveTypeName, TypeMap),
    LengthFieldSize = LengthPrimitiveType#primitiveType.size,
    
    % characterEncoding
    io:format(IoDevice,
        "~n~n~sCharacterEncoding() -> utf8.", [Name]),
    
    % semanticType        
    HasSemanticType = dict:is_key(semanticType, Attributes),
    if HasSemanticType ->
        SemanticType = dict:fetch(semanticType, Attributes),
        io:format(IoDevice,
            "~n~n~sMetaAttribute(semanticType) -> ~p.", [Name, SemanticType]);
        true -> ok
    end,

    % get
    io:format(IoDevice,
        "~n~nget~s({Buffer, Offset, Limit}, Length) ->"
        ++ "~n    SizeofLengthField = ~w,"
        ++ "~n    buffer:checkLimit(Buffer, Limit + SizeofLengthField),"
        ++ "~n    DataLength = buffer:~sGet(Buffer, Limit, ~w),"
        ++ "~n    BytesCopied = min(Length, DataLength),"
        ++ "~n    NewLimit = limit(Buffer, Limit + SizeofLengthField + DataLength),"
        ++ "~n    {{Buffer, Offset, NewLimit}, buffer:charsGet(Buffer, Limit + SizeofLengthField, BytesCopied)}.",
        [Name, LengthFieldSize, LengthPrimitiveTypeName, Endian]),

    % set
    io:format(IoDevice,
        "~n~nset~s(Src, SrcOffset, Length) ->"
        ++ "~n    fun({Buffer, Offset, Limit}) ->"
        ++ "~n        SizeOfLengthField = ~w,"
        ++ "~n        NewLimit = limit(Buffer, Limit + SizeOfLengthField + Length),"
        ++ "~n        NewBuffer = buffer:~sPut(Buffer, Limit, Length, ~w),"
        ++ "~n        NewBuffer2 = buffer:charsPut(NewBuffer, Limit + SizeOfLengthField, Src, SrcOffset, Length),"
        ++ "~n        {NewBuffer2, Offset, NewLimit}"
        ++ "~n    end.",
        [Name, LengthFieldSize, LengthPrimitiveTypeName, Endian]),

    ok.


% write fixed-length datatype methods
% FieldNode is an #xmlElement
% return Size

generateFixedLengthMethods(IoDevice, MessageSchema, FieldNode, TypeMap, Offset) ->
    Endian = getSchemaEndian(MessageSchema),
    FieldAttributes = utils:getAttributesDict(FieldNode),
    FieldName = dict:fetch(name, FieldAttributes),
    TypeName = dict:fetch(type, FieldAttributes),
    Type = dict:fetch(TypeName, TypeMap),
    Size = case Type of
        #primitiveType{} -> generatePrimitiveTypeMethods(IoDevice, FieldName, Type, Offset, Endian);
        #simpleType{} -> generateSimpleTypeMethods(IoDevice, TypeMap, FieldName, Type, Offset, Endian);
        _ -> 0
    end,
    %io:format("size:~w~n", [Size]),
    Size.


generatePrimitiveTypeMethods(IoDevice, FieldName, Type, Offset, Endian) ->
    io:format(IoDevice,
        "~n~nset~s(Value) ->"
        ++ "~n    fun({Buffer, Offset, Limit}) ->"
        ++ "~n        NewBuffer = buffer:~sPut(Buffer, Offset + ~w, Value, ~w),"
        ++ "~n        {NewBuffer, Offset, Limit}" 
        ++ "~n    end.",
        [FieldName, Type#primitiveType.name, Offset, Endian]),

    io:format(IoDevice,
        "~n~nget~s({Buffer, Offset, Limit}) ->"
        ++ "~n    buffer:~sGet(Buffer, Offset + ~w, ~w).",
        [FieldName, Type#primitiveType.name, Offset, Endian]),

    Type#primitiveType.size.


generateSimpleTypeMethods(IoDevice, TypeMap, FieldName, Type, Offset, Endian) ->
    Length = Type#simpleType.length,
    PrimitiveType = dict:fetch(Type#simpleType.primitiveType, TypeMap),
    case {Length, PrimitiveType#primitiveType.name} of
        {1, _} -> generatePrimitiveTypeMethods(IoDevice, FieldName, PrimitiveType, Offset, Endian);
        {_, "char"} -> generateFixedLengthString(IoDevice, FieldName, Length, Offset);
        {_, _} -> generatePrimitiveArrayMethods(IoDevice, FieldName, PrimitiveType, Length, Offset, Endian)
    end.


% fixed-length string
% assuming character encoding is always utf8
generateFixedLengthString(IoDevice, FieldName, Length, Offset) -> 
    io:format(IoDevice, 
        "~n~n~sLength() -> ~w.",
        [FieldName, Length]),
    
    io:format(IoDevice,
        "~n~n~sCharacterEncoding() -> utf8.", [FieldName]),

    io:format(IoDevice,
        "~n~nget~s({Buffer, Offset, Limit}, Index) ->"
        ++ "~n    if Index < 0; Index >= ~w -> error(index_out_of_range);"
        ++ "~n        true -> buffer:charGet(Buffer, Offset + ~w + (1*Index))"
        ++ "~n    end.",
        [FieldName, Length, Offset]),
    
    io:format(IoDevice,
        "~n~nset~s(Value, SrcOffset) ->"
        ++ "~n    fun({Buffer, Offset, Limit}) ->"
        ++ "~n        Length = ~w,"
        ++ "~n        if SrcOffset < 0; SrcOffset > size(Value) - Length"
        ++ "~n            -> error(srcOffset_out_of_range_for_copy);"
        ++ "~n        true ->"
        ++ "~n            NewBuffer = buffer:charsPut(Buffer, Offset + ~w, Value, SrcOffset, Length),"
        ++ "~n            {NewBuffer, Offset, Limit}"
        ++ "~n        end"
        ++ "~n    end.",
        [FieldName, Length, Offset]),

    Length.

% generate primitiveType arrays
generatePrimitiveArrayMethods(IoDevice, FieldName, PrimitiveType, Length, Offset, Endian) -> 
    UnitSize = PrimitiveType#primitiveType.size,
    PrimitiveTypeName = PrimitiveType#primitiveType.name,
    
    io:format(IoDevice,
        "~n~n~sLength() -> ~w.",
        [FieldName, Length]),

    io:format(IoDevice,
        "~n~nget~s({Buffer, Offset, Limit}, Index) ->"
        ++ "~n    if Index < 0; Index >= ~w -> error(index_out_of_range);"
        ++ "~n        true -> buffer:~sGet(Buffer, Offset + ~w + (~w*Index), ~w)"
        ++ "~n    end.",
        [FieldName, Length, PrimitiveTypeName, Offset, UnitSize, Endian]),

    io:format(IoDevice,
        "~n~nset~s(Index, Value) ->"
        ++ "~n    fun({Buffer, Offset, Limit}) ->"
        ++ "~n        if Index < 0; Index >= ~w -> erlang:error(index_out_of_range);"
        ++ "~n            true ->"
        ++ "~n                NewBuffer = buffer:~sPut(Buffer, Offset + ~w + (~w*Index), Value, ~w),"
        ++ "~n                {NewBuffer, Offset, Limit}"
        ++ "~n        end"
        ++ "~n    end.",
        [FieldName, Length, PrimitiveTypeName, Offset, UnitSize, Endian]),

    
    Length * UnitSize.

% write meta functions: 

generateMetaFunctions(IoDevice, MessageSchema, MessageAttributes) -> 
    TemplateId = dict:fetch(id, MessageAttributes),
    SchemaId = dict:fetch(id, MessageSchema),
    SchemaVersion = dict:fetch(version, MessageSchema),

    io:format(IoDevice,
        "~n~nsbeTemplateId() -> ~s."
        ++ "~nsbeSchemaId() -> ~s."
        ++ "~nsbeSchemaVersion() -> ~s.",
        [TemplateId, SchemaId, SchemaVersion]),
    
    io:format(IoDevice,
        "~n~nwrapForEncode(Buffer, Offset) ->"
        ++ "~n    Limit = limit(Buffer, Offset + sbeBlockLength()),"
        ++ "~n    {Buffer, Offset, Limit}.",
        []),
    
    io:format(IoDevice,
        "~n~nwrapForDecode(Buffer, Offset, ActingBlockLength, ActingVersion) ->"
        ++ "~n    Limit = limit(Buffer, Offset + ActingBlockLength),"
        ++ "~n    {Buffer, Offset, Limit}.", 
        []),

    io:format(IoDevice,
        "~n~nlimit(Buffer, Value) ->"
        ++ "~n    buffer:checkLimit(Buffer, Value),"
        ++ "~n    Value.",
        []),

    io:format(IoDevice,
        "~n~ngetSize({Buffer, Offset, Limit}) -> Limit - Offset.",
        []),
    
    ok.

% generate messageHeader.erl

generateMessageHeader(Attributes, OutDir) -> 
    FilePath = filename:join([OutDir, "messageHeader.erl"]),
    filelib:ensure_dir(FilePath),
    % make sure we start fresh
    {ok, IoDevice_overwrite} = file:open(FilePath, [write]),
    
    % file header
    generateFileHeader(IoDevice_overwrite, "messageHeader"),
    file:close(IoDevice_overwrite),
    {ok, IoDevice} = file:open(FilePath, [append]),
    
    % wrap
    io:format(IoDevice,
        "~n~nwrap(Buffer, Offset, MessageTemplateVersion) ->"
         ++"~n    {Buffer, Offset, MessageTemplateVersion}.", []),

    % fields: blockLength, schemaId, templateId, version
    % must be unsigned integer type
    % following xml ordering
    Size = lists:foldl(
        fun(D, Acc) -> 
            Dname = utils:capfirst(dict:fetch(name, D)),
            case dict:fetch(primitiveType, D) of
                "uint8"  -> generateMessageHeaderMethods(IoDevice,Dname, ?UINT8, Acc, little), Acc + ?UINT8#primitiveType.size;
                "uint16" -> generateMessageHeaderMethods(IoDevice,Dname,?UINT16, Acc, little), Acc + ?UINT16#primitiveType.size;
                "uint32" -> generateMessageHeaderMethods(IoDevice,Dname,?UINT32, Acc, little), Acc + ?UINT32#primitiveType.size;
                "uint64" -> generateMessageHeaderMethods(IoDevice,Dname,?UINT64, Acc, little), Acc + ?UINT64#primitiveType.size;
                _ -> error(must_be_unsigned_integer)
            end
        end, 0, Attributes),

    % size
    io:format(IoDevice, 
        "~n~nsize() -> ~w.", [Size]),

    % close
    file:close(IoDevice),
    lists:map(fun(D) -> dict:fetch(name, D) end, Attributes).

generateMessageHeaderMethods(IoDevice, Name, #primitiveType{} = P, Offset, Endian) ->
    % set methods
    io:format(IoDevice,
        "~n~nset~s(~s) ->"
        ++"~n    fun({Buffer, Offset, MessageTemplateVersion}) ->"
        ++"~n        NewBuffer = buffer:~sPut(Buffer, Offset + ~w, ~s, ~w),"
        ++"~n        {NewBuffer, Offset, MessageTemplateVersion}"
        ++"~n    end.",
        [Name, Name, P#primitiveType.name, Offset, Name, Endian]),
    
    % get methods
    io:format(IoDevice,
        "~n~nget~s({Buffer, Offset, _}) ->"
        ++"~n    buffer:~sGet(Buffer, Offset + ~w, ~w).",
        [Name, P#primitiveType.name, Offset, Endian]).

generateFileHeader(IoDevice, ModuleName) -> 
    io:format(IoDevice, "-module(~s).~n-compile(export_all).", [ModuleName]).


getSchemaEndian(MessageSchema) -> 
    SchemaEndian = utils:fetchWithDefault(byteOrder, MessageSchema, "littleEndian"),
    Endian = case SchemaEndian of
        "littleEndian" -> little;
        "bigEndian" -> big;
        _ -> error(undefined_endian)
    end,
    Endian.
   
