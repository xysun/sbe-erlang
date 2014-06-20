-module(carbenchmark).
-compile(export_all).


main() -> 
    lists:foreach(fun(_) -> perfEncode() end, lists:seq(1, 5)).

perfEncode() -> 
    {EncodeBuffer, HeaderOffset} = prepareEncode(),
    {MessageBuffer, _, _} = encode(EncodeBuffer, HeaderOffset),

    {ActingBlockLength, SchemaId, ActingVersion} = prepareDecode(MessageBuffer),
    HeaderSize = messageHeader:size(),
    
    Repeats = 10000,

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
    
    M = messageHeader:wrap(Buffer, BufferOffset, MessageTemplateVersion),
    M1 = messageHeader:setBlockLength(M, car:sbeBlockLength()),
    M2 = messageHeader:setTemplateId(M1, car:sbeTemplateId()),
    M3 = messageHeader:setSchemaId(M2, car:sbeSchemaId()),
    MessageHeader = messageHeader:setVersion(M3, car:sbeSchemaVersion()),

    HeaderOffset = BufferOffset + messageHeader:size(),
    {EncodeBuffer, _, _} = MessageHeader,
    {EncodeBuffer, HeaderOffset}.


encode(Buffer, Offset) -> 
    SrcOffset = 0, 
    VehicleCode = <<"abcdef">>,
    Make = <<"Honda">>,
    Model = <<"Civic VTi">>,

    M = car:wrapForEncode(Buffer, Offset),
    %M1 = car:setserialNumber(M, 1234),
    %M2 = car:setmodelYear(M1, 2023),
    %M2 = car:testQuicker(M, 1234, 2023),
    %io:format("M2:~w~n", [M2]),
    %io:format("M5:~w~n", [M5]),
    %M3 = car:setvehicleCode(M2, VehicleCode, SrcOffset),
    M3 = car:setAll(M, 1234, 2023, <<0,1,2,3,4>>, <<"abcdef">>),
    M4 = car:setmake(M3, Make, SrcOffset, size(Make)),
    Message = car:setmodel(M4, Model, SrcOffset, size(Model)),
    Message.
   
    %lists:foldl(fun(X, AccM) -> car:setsomeNumbers(AccM, X, X) end,
    %            Message, lists:seq(0, car:someNumbersLength() - 1)).


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

