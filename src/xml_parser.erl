-module(xml_parser).
-export([parse/2, findTypes/1]).
-include_lib("xmerl/include/xmerl.hrl").
-include("types.hrl").

findTypes(Node) -> 
    SimpleTypes_XPATH = "/messageSchema/types/type",
    %VarDataTypes_XPATH = "/messageSchema/types/composite[type/@length='0']",
    VarDataTypes_XPATH = "/messageSchema/types/composite[descendant::type[@name='varData']]",
    
    PrimitiveTypeMap = lists:foldl(
        fun(#primitiveType{} = T, Acc) ->  
            dict:store(T#primitiveType.name, T, Acc)                
        end, dict:new(), [?INT8, ?UINT8, ?INT16, ?UINT16, ?INT32, ?UINT32, ?INT64, ?UINT64, ?CHAR]
        ),
    
    SimpleTypeNodes = getAttributesFromXpath(Node, SimpleTypes_XPATH),
    
    SimpleTypeMap = lists:foldl(
            fun(D, Acc) ->
                NewSimpleType = createSimpleType(D, PrimitiveTypeMap),
                Name = dict:fetch(name, D),
                addTypeWithNameCheck(Name, NewSimpleType, Acc)
            end, PrimitiveTypeMap, SimpleTypeNodes
        ),

    VarDataTypeNodes = xmerl_xpath:string(VarDataTypes_XPATH, Node),
    TypeMap = lists:foldl(
        fun(VarDataNode, Acc) ->
            NewVarDataType = createVarDataType(VarDataNode),
            Name = NewVarDataType#varDataType.name,
            addTypeWithNameCheck(Name, NewVarDataType, Acc)
        end, SimpleTypeMap, VarDataTypeNodes
    ),

    TypeMap.

% add new type to typemap, with name check
addTypeWithNameCheck(Name, Type, TypeMap) -> 
    NameInMap = dict:is_key(Name, TypeMap),
    if NameInMap -> error(type_already_exists);
       true -> dict:store(Name, Type, TypeMap)
    end.

% generate a SimpleType record from an attributes dictionary
createSimpleType(D, PrimitiveTypeMap) -> 
    Name = dict:fetch(name, D), 
    PrimitiveTypeName = dict:fetch(primitiveType, D),
    PrimitiveType = dict:fetch(PrimitiveTypeName, PrimitiveTypeMap),
    LengthStr = utils:fetchWithDefault(length, D, "1"),
    {Length, _} = string:to_integer(LengthStr),
    Size = Length * PrimitiveType#primitiveType.size, 
    #simpleType{name = Name, 
                primitiveType = PrimitiveTypeName,
                length = Length,
                size = Size}.

createVarDataType(Node) ->
    Attributes = utils:getAttributesDict(Node),
    Name = dict:fetch(name, Attributes),
    [LengthNode|_] = xmerl_xpath:string("type[@name='length']", Node),
    [DataNode|_] = xmerl_xpath:string("type[@name='varData']", Node),
    LengthNodeAttributes = utils:getAttributesDict(LengthNode),
    DataNodeAttributes = utils:getAttributesDict(DataNode),
    #varDataType{name = Name,
                 lengthPrimitiveType = dict:fetch(primitiveType, LengthNodeAttributes)}.

% find messages in xml and store in messageMap
findMessages(Node) ->
    Messages_XPATH = "/messageSchema/message",
    MessageNodes= xmerl_xpath:string(Messages_XPATH, Node),
    lists:foldl(
        fun(MessageNode, Acc) -> 
            AttributesDict = lists:foldl(
                fun(#xmlAttribute{} = A, Dict) -> 
                    dict:store(A#xmlAttribute.name, A#xmlAttribute.value, Dict)
                end, dict:new(), MessageNode#xmlElement.attributes),
            MessageId = dict:fetch(id, AttributesDict),
            addMessageWithIDCheck(MessageId, MessageNode, Acc)
        end, dict:new(), MessageNodes
    ).


addMessageWithIDCheck(Id, Node, MessageMap) -> 
    IdInMap = dict:is_key(Id, MessageMap),
    if IdInMap -> error(message_ID_already_exists);
       true -> dict:store(Id, Node, MessageMap)
    end.


parse(Filename, OutDir) -> 
    {Root, _} = xmerl_scan:file(Filename),

    %XPATH
    MessageHeader_XPATH = "/messageSchema/types/composite[@name='messageHeader']/type",
    MessageSchema_XPATH = "/messageSchema",
    
    [MessageSchema|_] = getAttributesFromXpath(Root, MessageSchema_XPATH),
    SubDirName = filename:join([OutDir, dict:fetch(package, MessageSchema)]),
    
    %generate message header
    MessageHeaderAttributes = getAttributesFromXpath(Root, MessageHeader_XPATH),
    generator:generateMessageHeader(MessageHeaderAttributes, SubDirName),
    
    %create typeMap and messageMap
    TypeMap = findTypes(Root),
    MessageMap = findMessages(Root),
    generator:generate(MessageSchema, TypeMap, MessageMap, SubDirName),

    TypeMap.

% return a {name:value} attribute dicts for each matching element
getAttributesFromXpath(Node, Xpath) -> 
    lists:reverse(
        lists:foldl(
            fun(#xmlElement{} = E, Acc) -> 
                [lists:foldl(
                    fun(#xmlAttribute{} = A, Dict) ->
                        dict:store(A#xmlAttribute.name, A#xmlAttribute.value, Dict)
                    end,
                    dict:new(), E#xmlElement.attributes
                )|Acc]
            end,
            [], xmerl_xpath:string(Xpath, Node)
        )
    ).
