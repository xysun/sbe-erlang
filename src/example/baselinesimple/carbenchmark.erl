-module(carbenchmark).
-compile(export_all).


main() -> 
    lists:foreach(fun(_) -> perfEncode() end, lists:seq(1, 5)).

perfEncode() -> 
    {EncodeBuffer, HeaderOffset} = prepareEncode(),
    {MessageBuffer, _, _} = encode(EncodeBuffer, HeaderOffset),

    {ActingBlockLength, SchemaId, ActingVersion} = prepareDecode(MessageBuffer),
    HeaderSize = messageHeader:size(),
    
    Repeats = 100000,

    EncodeStartTime = now(),
    lists:foreach(fun(_) -> encode(EncodeBuffer, HeaderOffset) end, lists:seq(1, Repeats)),
    EncodeEndTime = now(),
    EncodeTimeDiff = timeDiff(EncodeStartTime, EncodeEndTime)/Repeats,
    io:format("~nencoding...~fns per operation~n", [1000*EncodeTimeDiff]),
    
    DecodeStartTime = now(),
    lists:foreach(fun(_) -> decode(MessageBuffer, HeaderSize, ActingBlockLength, SchemaId, ActingVersion) end, lists:seq(1, Repeats)),
    DecodeEndTime = now(),
    DecodeTimeDiff = timeDiff(DecodeStartTime, DecodeEndTime)/Repeats,
    io:format("~ndecoding...~fns per operation~n", [1000*DecodeTimeDiff]),
    
    ok.


timeDiff({S1, S2, S3}, {E1, E2, E3}) -> 
    TotalMs1 = 1000000*(S1*1000000+S2) + S3,
    TotalMs2 = 1000000*(E1*1000000+E2) + E3,
    TotalMs2 - TotalMs1.


prepareDecode(MessageBuffer) ->
    MessageHeaderForDecode = messageHeader:wrap(MessageBuffer , 0, 0),
    ActingBlockLength = messageHeader:getBlockLength(MessageHeaderForDecode),
    SchemaId = messageHeader:getSchemaId(MessageHeaderForDecode),
    ActingVersion = messageHeader:getVersion(MessageHeaderForDecode),
    {ActingBlockLength, SchemaId, ActingVersion}.

   

prepareEncode() -> 
    Buffer = buffer:allocate(64),
    BufferOffset = 0,
    MessageTemplateVersion = 0,
    MessageHeader = buffer:chainFunctions(
        messageHeader:wrap(Buffer, BufferOffset, MessageTemplateVersion),
        [messageHeader:setBlockLength(car:sbeBlockLength()),
         messageHeader:setTemplateId(car:sbeTemplateId()),
         messageHeader:setSchemaId(car:sbeSchemaId()),
         messageHeader:setVersion(car:sbeSchemaVersion())]
    ),

    HeaderOffset = BufferOffset + messageHeader:size(),
    {EncodeBuffer, _, _} = MessageHeader,
    {EncodeBuffer, HeaderOffset}.

   

encode(Buffer, Offset) -> 
    SrcOffset = 0, 
    VehicleCode = <<"abcdef">>,
    Make = <<"Honda">>,
    Model = <<"Civic VTi">>,

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
    car:sbeTemplateId(),
    car:sbeSchemaVersion(),
    car:getserialNumber(Message),
    car:getmodelYear(Message),
    
    lists:foreach(fun(X) -> 
                    car:getsomeNumbers(Message, X) end, 
                    lists:seq(0, car:someNumbersLength() - 1)),
    
    VehicleCode = lists:reverse(
                      lists:foldl(fun(X, Acc) -> [car:getvehicleCode(Message, X)|Acc] end,
                      [],
                      lists:seq(0, car:vehicleCodeLength() - 1))),
    
    car:makeMetaAttribute(semanticType),
    {Message2, Make} = car:getmake(Message, 128),
    
    {Message3, Model} = car:getmodel(Message2, 128),
    
    car:getSize(Message3),

    ok.

