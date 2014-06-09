-module(car).
-compile(export_all).

sbeBlockLength() -> 36. % length of fixed-size fields 
sbeTemplateId() -> 3.
sbeSchemaId() -> 2.
sbeSchemaVersion() -> 0.

wrapForEncode(Buffer, Offset) -> {Buffer, Offset}.

% we know the sequence:
% serialNumber (uint 64) -> modelYear (uint16) -> someNumbers(int32, 5)
% -> vehicleCode(char, 6)

serialNumber(Value) ->
    fun({Buffer, Offset}) ->
        NewBuffer = buffer:uint64Put(Buffer, Offset + 0, Value),
        {NewBuffer, Offset}
    end.

modelYear(Value) ->
    fun({Buffer, Offset}) ->
        NewBuffer = buffer:uint16Put(Buffer, Offset + 8, Value),
        {NewBuffer, Offset}
    end.

someNumbersLength() -> 5.

someNumbers(Index, Value) -> 
    fun({Buffer, Offset}) -> 
        if Index < 0; Index >= 5 -> erlang:error(index_out_of_range);
           true ->
                NewBuffer = buffer:int32Put(Buffer, Offset + 10 + (4*Index), Value),
                {NewBuffer, Offset}
        end
    end.

% fixed-length string
vehicleCodeCharacterEncoding() -> 'us-ascii'.

putVehicleCode(Value, SrcOffset) -> 
    fun({Buffer, Offset}) ->
        Length = 6,
        if SrcOffset < 0; SrcOffset > size(Value) - Length 
                -> error(srcOffset_out_of_range_for_copy);
            true ->
                NewBuffer = buffer:charsPut(Buffer, Offset + 30, Value, SrcOffset, Length),
                {NewBuffer, Offset}
        end
    end.
