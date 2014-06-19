-module(car_bk).
-compile(export_all).

% "flyweight" fields
sbeBlockLength() -> 36. % length of fixed-size fields 
sbeTemplateId() -> 3.
sbeSchemaId() -> 2.
sbeSchemaVersion() -> 0.

wrapForEncode(Buffer, Offset) -> 
    Limit = limit(Buffer, Offset + sbeBlockLength()), 
    {Buffer, Offset, Limit}.

wrapForDecode(Buffer, Offset, ActingBlockLength, ActingVersion) -> 
    Limit = limit(Buffer, Offset + ActingBlockLength),
    {Buffer, Offset, Limit}.

limit(Buffer, Value) -> 
    buffer:checkLimit(Buffer, Value), 
    Value.

getSize({Buffer, Offset, Limit}) -> Limit - Offset.

% data fields
% we know the sequence:
% serialNumber (uint 64) -> modelYear (uint16) -> someNumbers(int32, 5)-> vehicleCode(char, 6)

setserialNumber(Value) ->
    fun({Buffer, Offset, Limit}) ->
        NewBuffer = buffer:uint64Put(Buffer, Offset + 0, Value, little),
        {NewBuffer, Offset, Limit}
    end.

getserialNumber({Buffer, Offset, Limit}) ->
    buffer:uint64Get(Buffer, Offset + 0, little).

setmodelYear(Value) ->
    fun({Buffer, Offset, Limit}) ->
        NewBuffer = buffer:uint16Put(Buffer, Offset + 8, Value, little),
        {NewBuffer, Offset, Limit}
    end.

getmodelYear({Buffer, Offset, Limit}) -> 
    buffer:uint16Get(Buffer, Offset + 8, little).

someNumbersLength() -> 5.

setsomeNumbers(Index, Value) -> 
    fun({Buffer, Offset, Limit}) -> 
        if Index < 0; Index >= 5 -> erlang:error(index_out_of_range);
           true ->
                NewBuffer = buffer:int32Put(Buffer, Offset + 10 + (4*Index), Value, little),
                {NewBuffer, Offset, Limit}
        end
    end.

getsomeNumbers({Buffer, Offset, Limit}, Index) -> 
    if Index < 0; Index >= 5 -> error(index_out_of_range);
        true -> buffer:int32Get(Buffer, Offset + 10 + (4*Index), little)
    end.

% fixed-length string
vehicleCodeLength() -> 6.
vehicleCodeCharacterEncoding() -> utf8.

setvehicleCode(Value, SrcOffset) -> 
    fun({Buffer, Offset, Limit}) ->
        Length = 6,
        if SrcOffset < 0; SrcOffset > size(Value) - Length 
                -> error(srcOffset_out_of_range_for_copy);
            true ->
                NewBuffer = buffer:charsPut(Buffer, Offset + 30, Value, SrcOffset, Length),
                {NewBuffer, Offset, Limit}
        end
    end.

getvehicleCode({Buffer, Offset, Limit}, Index) ->
    if Index < 0; Index >= 6 -> error(index_out_of_range);
        true -> buffer:charGet(Buffer, Offset + 30 + (1*Index))
    end.

% variable-length string
makeCharacterEncoding() -> utf8.
makeMetaAttribute(semanticType) -> "Make".

setmake(Src, SrcOffset, Length) -> 
    fun({Buffer, Offset, Limit}) -> 
        SizeOfLengthField = 1,
        NewLimit = limit(Buffer, Limit + SizeOfLengthField + Length), 
        NewBuffer = buffer:uint8Put(Buffer, Limit, Length, little),
        NewBuffer2 = buffer:charsPut(NewBuffer, Limit + SizeOfLengthField, Src, SrcOffset, Length),
        {NewBuffer2, Offset, NewLimit}
    end.

getmake({Buffer, Offset, Limit}, Length) -> 
    SizeofLengthField = 1,
    buffer:checkLimit(Buffer, Limit + SizeofLengthField),
    DataLength = buffer:uint8Get(Buffer, Limit, little), 
    BytesCopied = min(Length, DataLength),
    NewLimit = limit(Buffer, Limit + SizeofLengthField + DataLength),
    {{Buffer, Offset, NewLimit}, buffer:charsGet(Buffer, Limit + SizeofLengthField, BytesCopied)}.


modelCharacterEncoding() -> utf8.
setmodel(Src, SrcOffset, Length) -> 
     fun({Buffer, Offset, Limit}) -> 
        SizeOfLengthField = 1,
        NewLimit = limit(Buffer, Limit + SizeOfLengthField + Length), 
        NewBuffer = buffer:uint8Put(Buffer, Limit, Length, little),
        NewBuffer2 = buffer:charsPut(NewBuffer, Limit + SizeOfLengthField, Src, SrcOffset, Length),
        {NewBuffer2, Offset, NewLimit}
    end.

getmodel({Buffer, Offset, Limit}, Length) -> 
    SizeofLengthField = 1,
    buffer:checkLimit(Buffer, Limit + SizeofLengthField),
    DataLength = buffer:uint8Get(Buffer, Limit, little), 
    BytesCopied = min(Length, DataLength),
    NewLimit = limit(Buffer, Limit + SizeofLengthField + DataLength),
    {{Buffer, Offset, NewLimit}, buffer:charsGet(Buffer, Limit + SizeofLengthField, BytesCopied)}.


