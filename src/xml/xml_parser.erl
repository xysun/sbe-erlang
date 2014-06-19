-module(xml_parser).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").
-include("types.hrl").

findTypes(Node) -> 
    SimpleTypes_XPATH = "/messageSchema/types/type",
    CompositeTypes_XPATH = "/messageSchema/types/composite[@name!='messageHeader']/type",
    
    PrimitiveTypeMap = lists:foldl(
        fun(#primitiveType{} = T, Acc) ->  
            dict:store(T#primitiveType.name, T, Acc)                
        end, dict:new(), [?INT8, ?UINT8, ?INT16, ?UINT16, ?INT32, ?UINT32, ?INT64, ?UINT64, ?CHAR]
        ),
    
    SimpleTypeNodes = getAttributesFromXpath(Node, SimpleTypes_XPATH),
    
    SimpleTypeMap = lists:foldl(
            fun(D, Acc) ->
                NewSimpleType = createSimpleType(D),
                Name = dict:fetch(name, D),
                addTypeWithNameCheck(Name, NewSimpleType, Acc)
            end, PrimitiveTypeMap, SimpleTypeNodes
        ),
    SimpleTypeMap.

% add new type to typemap, with name check
addTypeWithNameCheck(Name, Type, TypeMap) -> 
    NameInMap = dict:is_key(Name, TypeMap),
    if NameInMap -> error(type_already_exists);
       true -> dict:store(Name, Type, TypeMap)
    end.

% generate a SimpleType record
createSimpleType(D) -> 
    Name = dict:fetch(name, D), 
    PrimitiveType = dict:fetch(primitiveType, D),
    LengthStr = utils:fetchWithDefault(length, D, "1"),
    {Length, _} = string:to_integer(LengthStr),
    #simpleType{name = Name, 
                primitiveType = PrimitiveType,
                length = Length}.

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


parse(Filename) -> 
    {Root, _} = xmerl_scan:file(Filename),

    %generate message header
    MessageHeader_XPATH = "/messageSchema/types/composite[@name='messageHeader']/type",
    MessageSchema_XPATH = "/messageSchema",
    MessageHeaderAttributes = getAttributesFromXpath(Root, MessageHeader_XPATH),
    generator:generateMessageHeader(MessageHeaderAttributes),
    
    %create typeMap and messageMap
    TypeMap = findTypes(Root),
    MessageMap = findMessages(Root),
    [MessageSchema|_] = getAttributesFromXpath(Root, MessageSchema_XPATH),
    generator:generate(MessageSchema, TypeMap, MessageMap),

    TypeMap.
    


main() ->
    parse("../example-schema-simple.xml").

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
