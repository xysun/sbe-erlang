% corresponds to ExampleUsingGeneratedStub.java
-module(example).
-export([main/0, readJava/0, encode/2]).

main() ->
    io:format("****** SBE Car example ******", []),
    Buffer = buffer:allocate(64),
    BufferOffset = 0,
    MessageTemplateVersion = 0,
    % encode a message
    MessageHeader = buffer:chainFunctions(
        messageHeader:wrap(Buffer, BufferOffset, MessageTemplateVersion),
        [messageHeader:setBlockLength(car:sbeBlockLength()),
         messageHeader:setTemplateId(car:sbeTemplateId()),
         messageHeader:setSchemaId(car:sbeSchemaId()),
         messageHeader:setVersion(car:sbeSchemaVersion())]
    ),

    HeaderOffset = BufferOffset + messageHeader:size(),
    {EncodeBuffer, _, _} = MessageHeader,
    {MessageBuffer, _, _} = encode(EncodeBuffer, HeaderOffset),

    % optionally write the encoded buffer to a file for decoding
    file:write_file("car_erlang", MessageBuffer),

    % decode the encoded message
    MessageHeaderForDecode = messageHeader:wrap(MessageBuffer , BufferOffset, MessageTemplateVersion),
    % match template ID
    TemplateId = messageHeader:getTemplateId(MessageHeaderForDecode),
    CarTemplateId = car:sbeTemplateId(),
    if TemplateId =/= CarTemplateId -> error(templateId_do_not_match);
       true -> ok
    end,
    
    ActingBlockLength = messageHeader:getBlockLength(MessageHeaderForDecode),
    SchemaId = messageHeader:getSchemaId(MessageHeaderForDecode),
    ActingVersion = messageHeader:getVersion(MessageHeaderForDecode),

    decode(MessageBuffer, 
           BufferOffset + messageHeader:size(),
           ActingBlockLength,
           SchemaId,
           ActingVersion),
    
    ok.

readJava() -> 
    {ok, Binary} = file:read_file("car_java"),
    MessageHeaderForDecode = messageHeader:wrap(Binary, 0, 0),
    TemplateId = messageHeader:getTemplateId(MessageHeaderForDecode),

    CarTemplateId = car:sbeTemplateId(),
    if TemplateId =/= CarTemplateId -> error(templateId_do_not_match);
       true -> ok
    end,
    
    ActingBlockLength = messageHeader:getBlockLength(MessageHeaderForDecode),
    SchemaId = messageHeader:getSchemaId(MessageHeaderForDecode),
    ActingVersion = messageHeader:getVersion(MessageHeaderForDecode),

    decode(Binary, 
           0 + messageHeader:size(),
           ActingBlockLength,
           SchemaId,
           ActingVersion),

    ok.


encode(Buffer, Offset) -> 
    SrcOffset = 0, 
    VehicleCode = list_to_binary("abcdef"),
    Make = list_to_binary("Honda"),
    Model = list_to_binary("Civic VTi"),
    Message = 
        buffer:chainFunctions(
            car:wrapForEncode(Buffer, Offset), 
            [
                car:setserialNumber(1234), 
                car:setmodelYear(2023), 
                car:setvehicleCode(VehicleCode, SrcOffset),
                car:setmake(Make, SrcOffset, size(Make)),
                car:setmodel(Model, SrcOffset, size(Model))
            ]
        ),
    
    Message2 = 
        buffer:chainFunctions(
            Message,
            [car:setsomeNumbers(X, X) || X <- lists:seq(0, car:someNumbersLength() - 1)]
        ),
    
    Message2.

decode(Buffer, Offset, ActingBlockLength, SchemaId, ActingVersion) -> 
    Message = car:wrapForDecode(Buffer, Offset, ActingBlockLength, ActingVersion), 
    io:format("~ncar.templateId = ~w", [car:sbeTemplateId()]),
    io:format("~ncar.schemaId = ~w", [SchemaId]),
    io:format("~ncar.schemaVersion = ~w", [car:sbeSchemaVersion()]),
    io:format("~ncar.serialNumber = ~w", [car:getserialNumber(Message)]),
    io:format("~ncar.modelYear = ~w", [car:getmodelYear(Message)]),
    
    io:format("~ncar.someNumbers = "),
    lists:foreach(fun(X) -> 
                    io:format("~w,", [car:getsomeNumbers(Message, X)]) end, 
                    lists:seq(0, car:someNumbersLength() - 1)),
    
    VehicleCode = lists:reverse(
                      lists:foldl(fun(X, Acc) -> [car:getvehicleCode(Message, X)|Acc] end,
                      [],
                      lists:seq(0, car:vehicleCodeLength() - 1))),
    %io:format("~p", [VehicleCode]),
    io:format("~ncar.vehicleCode = ~p", [VehicleCode]),
    
    io:format("~ncar.make.semanticType = ~p", [car:makeMetaAttribute(semanticType)]),
    {Message2, Make} = car:getmake(Message, 128),
    io:format("~ncar.make = ~p", [Make]), 
    
    {Message3, Model} = car:getmodel(Message2, 128),
    io:format("~ncar.model = ~p", [Model]),
    
    io:format("~ncar.size = ~p~n", [car:getSize(Message3)]),

    ok.

