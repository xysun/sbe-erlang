% buffer utilities
% these are not "real" buffers though..

-module(buffer).
-compile(export_all).


% allocate x bytes
allocate(Capacity) -> << <<0>> || _ <- util:int_to_list(Capacity) >>.

checkLimit(Buffer, Limit) -> 
    if Limit > size(Buffer) -> error(limit_beyond_capacity);
       true -> ok
    end.

% Size is in bits
% unsigned Put
uPut(Size, Buffer, Offset, Value) ->
    OffsetInBits = 8 * Offset,
    case Buffer of
        << Header:OffsetInBits, _:Size/integer-unsigned-little, Rest/binary >> ->
            << Header:OffsetInBits, Value:Size/integer-unsigned-little, Rest/binary >>
    end.

% unsigned Get
uGet(Size, Buffer, Offset) -> 
    OffsetInBits = 8 * Offset, 
    case Buffer of
        << _:OffsetInBits, Value:Size/integer-unsigned-little, _/binary >> -> 
            Value
    end.

% signed Put
iPut(Size, Buffer, Offset, Value) ->
    OffsetInBits = 8 * Offset,
    case Buffer of
        << Header:OffsetInBits, _:Size/integer-signed-little, Rest/binary >> ->
            << Header:OffsetInBits, Value:Size/integer-signed-little, Rest/binary >>
    end.

iGet(Size, Buffer, Offset) -> 
    OffsetInBits = 8 * Offset,
    case Buffer of
        << _:OffsetInBits, Value:Size/integer-signed-little, _/binary>> ->
            Value
    end.

uint8Put(Buffer, Offset, Value) -> uPut(8, Buffer, Offset, Value).
uint16Put(Buffer, Offset, Value) -> uPut(16, Buffer, Offset, Value).
uint64Put(Buffer, Offset, Value) -> uPut(64, Buffer, Offset, Value).

uint8Get(Buffer, Offset) -> uGet(8, Buffer, Offset).
uint16Get(Buffer, Offset) -> uGet(16, Buffer, Offset).
uint64Get(Buffer, Offset) -> uGet(64, Buffer, Offset).

int32Put(Buffer, Offset, Value) -> iPut(32, Buffer, Offset, Value).
int32Get(Buffer, Offset) -> iGet(32, Buffer, Offset).

% char
charsPut(Buffer, Offset, Value, SrcOffset, Length) -> 
    OffsetInBits = 8 * Offset,
    SrcOffsetInBits = 8 * SrcOffset,
    LengthInBits = 8 * Length,
    case {Value, Buffer} of
        {<< _:SrcOffsetInBits, CopyValue:LengthInBits, _/binary >>,
         << Header:OffsetInBits, _:LengthInBits, Rest/binary >>} ->
            << Header:OffsetInBits, CopyValue:LengthInBits, Rest/binary>>
    end.

charsGet(Buffer, Offset, Length) -> 
    OffsetInBits = 8 * Offset,
    LengthInBits = 8 * Length,
    case Buffer of
        << _:OffsetInBits, Value: LengthInBits/bitstring, _/binary >> ->
            binary:bin_to_list(Value)
    end.

charGet(Buffer, Offset) -> 
    OffsetInBits = 8 * Offset, 
    case Buffer of
        <<_:OffsetInBits, Value:8/bitstring, _/binary>> ->
            binary:bin_to_list(Value)
    end.
