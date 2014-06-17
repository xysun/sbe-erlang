% corresponds to ExampleUsingGeneratedStub.java
-module(example).
-compile(export_all).

main() ->
    Buffer = buffer:allocate(64),
    BufferOffset = 0,
    MessageTemplateVersion = 0,
    % encode a message
    % TODO: allow chain_last
    MH1 = messageHeader:wrap(Buffer, BufferOffset, MessageTemplateVersion),
    MH2 = messageHeader:setBlockLength(MH1, car:sbeBlockLength()),
    MH3 = messageHeader:setTemplateId(MH2, car:sbeTemplateId()),
    MH4 = messageHeader:setSchemaId(MH3, car:sbeSchemaId()),
    MH5 = messageHeader:setVersion(MH4, car: sbeSchemaVersion()),

    HeaderOffset = BufferOffset + messageHeader:size(),
    {EncodeBuffer, _, _} = MH5,
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
    
    MessageBuffer.


readJava() -> 
    {ok, Binary} = file:read_file("car_erlang"),
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
        util:chain_last(
            car:wrapForEncode(Buffer, Offset), 
            [
                car:setserialNumber(1234), % uint64
                car:setmodelYear(2023), % uint16
                car:putvehicleCode(VehicleCode, SrcOffset),
                car:putMake(Make, SrcOffset,size(Make)),
                car:putModel(Model, SrcOffset, size(Model))
            ]
        ),
    
    Message2 = 
        util:chain_last(
            Message,
            [car:setsomeNumbers(X, X) || X <- util:int_to_list(car:someNumbersLength())]
        ),
    
    Message2.

decode(Buffer, Offset, ActingBlockLength, SchemaId, ActingVersion) -> 
    Message = car:wrapForDecode(Buffer, Offset, ActingBlockLength, ActingVersion), 
    io:format("~ncar.templateId = ~p", [car:sbeTemplateId()]),
    io:format("~ncar.schemaId = ~p", [SchemaId]),
    io:format("~ncar.schemaVersion = ~p", [car:sbeSchemaVersion()]),
    io:format("~ncar.serialNumber = ~p", [car:getserialNumber(Message)]),
    io:format("~ncar.modelYear = ~p", [car:getmodelYear(Message)]),
    
    io:format("~ncar.someNumbers = "),
    lists:foreach(fun(X) -> 
                    io:format("~p,", [car:getsomeNumbers(Message, X)]) end, 
                    lists:seq(0, car:someNumbersLength() - 1)),
    
    VehicleCode = lists:reverse(
                      lists:foldl(fun(X, Acc) -> [car:getvehicleCode(Message, X)|Acc] end,
                      [],
                      lists:seq(0, car:vehicleCodeLength() - 1))),
    io:format("~ncar.vehicleCode = ~p", [string:join(VehicleCode,"")]),
    
    io:format("~ncar.make.semanticType = ~p", [car:makeMetaAttribute(semantic_type)]),
    {Message2, Make} = car:getMake(Message, 128),
    io:format("~ncar.make = ~p", [Make]), 
    
    {Message3, Model} = car:getModel(Message2, 128),
    io:format("~ncar.model = ~p", [Model]),
    
    io:format("~ncar.size = ~p", [car:getSize(Message3)]),

    ok.
