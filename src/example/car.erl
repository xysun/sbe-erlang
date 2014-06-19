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

getsomeNumbers({Buffer, Offset, Limit}, Index) ->
    if Index < 0; Index >= 5 -> error(index_out_of_range);
        true -> buffer:int32Get(Buffer, Offset + 10 + (4*Index), little)
    end.

setsomeNumbers(Index, Value) ->
    fun({Buffer, Offset, Limit}) ->
        if Index < 0; Index >= 5 -> erlang:error(index_out_of_range);
            true ->
                NewBuffer = buffer:int32Put(Buffer, Offset + 10 + (4*Index), Value, little),
                {NewBuffer, Offset, Limit}
        end
    end.

vehicleCodeLength() -> 6.

vehicleCodeCharacterEncoding() -> us_ascii.

getvehicleCode({Buffer, Offset, Limit}, Index) ->
    if Index < 0; Index >= 6 -> error(index_out_of_range);
        true -> buffer:charGet(Buffer, Offset + 30 + (1*Index))
    end.

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

sbeBlockLength() -> 36.