-module(car).
-compile(export_all).

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

setserialNumber({Buffer, Offset, Limit}, Value) ->
   NewBuffer = buffer:uint64Put(Buffer, Offset + 0, Value, little),
   {NewBuffer, Offset, Limit}.

getserialNumber({Buffer, Offset, Limit}) ->
    buffer:uint64Get(Buffer, Offset + 0, little).

setmodelYear({Buffer, Offset, Limit}, Value) ->
   NewBuffer = buffer:uint16Put(Buffer, Offset + 8, Value, little),
   {NewBuffer, Offset, Limit}.

getmodelYear({Buffer, Offset, Limit}) ->
    buffer:uint16Get(Buffer, Offset + 8, little).

someNumbersLength() -> 5.

getsomeNumbers({Buffer, Offset, Limit}, Index) ->
    if Index < 0; Index >= 5 -> error(index_out_of_range);
        true -> buffer:int32Get(Buffer, Offset + 10 + (4*Index), little)
    end.

setsomeNumbers({Buffer, Offset, Limit}, Index, Value) ->
    if Index < 0; Index >= 5 -> erlang:error(index_out_of_range);
        true ->
            NewBuffer = buffer:int32Put(Buffer, Offset + 10 + (4*Index), Value, little),
            {NewBuffer, Offset, Limit}
    end.

vehicleCodeLength() -> 6.

vehicleCodeCharacterEncoding() -> utf8.

getvehicleCode({Buffer, Offset, Limit}, Index) ->
    if Index < 0; Index >= 6 -> error(index_out_of_range);
        true -> buffer:charGet(Buffer, Offset + 30 + (1*Index))
    end.

setvehicleCode({Buffer, Offset, Limit}, Value, SrcOffset) ->
    Length = 6,
    if SrcOffset < 0; SrcOffset > byte_size(Value) - Length
        -> error(srcOffset_out_of_range_for_copy);
    true ->
        NewBuffer = buffer:charsPut(Buffer, Offset + 30, Value, SrcOffset, Length),
        {NewBuffer, Offset, Limit}
    end.

sbeBlockLength() -> 36.

makeCharacterEncoding() -> utf8.

makeMetaAttribute(semanticType) -> "Make".

getmake({Buffer, Offset, Limit}, Length) ->
    SizeofLengthField = 1,
    buffer:checkLimit(Buffer, Limit + SizeofLengthField),
    DataLength = buffer:uint8Get(Buffer, Limit, little),
    BytesCopied = min(Length, DataLength),
    NewLimit = limit(Buffer, Limit + SizeofLengthField + DataLength),
    {{Buffer, Offset, NewLimit}, buffer:charsGet(Buffer, Limit + SizeofLengthField, BytesCopied)}.

setmake({Buffer, Offset, Limit}, Src, SrcOffset, Length) ->
    SizeOfLengthField = 1,
    NewLimit = limit(Buffer, Limit + SizeOfLengthField + Length),
    NewBuffer = buffer:uint8Put(Buffer, Limit, Length, little),
    NewBuffer2 = buffer:charsPut(NewBuffer, Limit + SizeOfLengthField, Src, SrcOffset, Length),
    {NewBuffer2, Offset, NewLimit}.

modelCharacterEncoding() -> utf8.

getmodel({Buffer, Offset, Limit}, Length) ->
    SizeofLengthField = 1,
    buffer:checkLimit(Buffer, Limit + SizeofLengthField),
    DataLength = buffer:uint8Get(Buffer, Limit, little),
    BytesCopied = min(Length, DataLength),
    NewLimit = limit(Buffer, Limit + SizeofLengthField + DataLength),
    {{Buffer, Offset, NewLimit}, buffer:charsGet(Buffer, Limit + SizeofLengthField, BytesCopied)}.

setmodel({Buffer, Offset, Limit}, Src, SrcOffset, Length) ->
    SizeOfLengthField = 1,
    NewLimit = limit(Buffer, Limit + SizeOfLengthField + Length),
    NewBuffer = buffer:uint8Put(Buffer, Limit, Length, little),
    NewBuffer2 = buffer:charsPut(NewBuffer, Limit + SizeOfLengthField, Src, SrcOffset, Length),
    {NewBuffer2, Offset, NewLimit}.