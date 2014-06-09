% corresponds to ExampleUsingGeneratedStub.java
-module(example).
-compile(export_all).

main() ->
    Buffer = buffer:allocate(60),
    BufferOffset = 0,
    MessageTemplateVersion = 0,
    % encode a message
    MessageHeader = 
        util:chain_last(
            messageHeader:wrap(Buffer, BufferOffset, MessageTemplateVersion),
            [
                messageHeader:blockLength(car:sbeBlockLength()),
                messageHeader:templateId(car:sbeTemplateId()),
                messageHeader:schemaId(car:sbeSchemaId()),
                messageHeader:version(car:sbeSchemaVersion())
            ]
        ),
    HeaderOffset = BufferOffset + messageHeader:size(),
    {EncodeBuffer, _, _} = MessageHeader,
    Message = encode(EncodeBuffer, HeaderOffset),

    % optionally write the encoded buffer to a file for decoding

    % decode the encoded message
    Message.

encode(Buffer, Offset) -> 
    SrcOffset = 0, 
    VehicleCode = list_to_binary("abcdef"),
    {NewBuffer, NewOffset} = 
        util:chain_last(
            car:wrapForEncode(Buffer, Offset), 
            [
                car:serialNumber(1234), % uint64
                car:modelYear(2013), % uint16
                car:putVehicleCode(VehicleCode, SrcOffset)
            ]
        ),
    
    {NewBuffer2, NewOffset2} = 
        util:chain_last(
            {NewBuffer, NewOffset}, 
            [car:someNumbers(X, X) || X <- util:int_to_list(car:someNumbersLength())]
        ),
    
    Message = {NewBuffer2, NewOffset2},
    Message.
