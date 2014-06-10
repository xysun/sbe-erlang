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

% signed Put
iPut(Size, Buffer, Offset, Value) ->
    OffsetInBits = 8 * Offset,
    case Buffer of
        << Header:OffsetInBits, _:Size/integer-signed-little, Rest/binary >> ->
            << Header:OffsetInBits, Value:Size/integer-signed-little, Rest/binary >>
    end.

uint8Put(Buffer, Offset, Value) -> uPut(8, Buffer, Offset, Value).
uint16Put(Buffer, Offset, Value) -> uPut(16, Buffer, Offset, Value).
uint64Put(Buffer, Offset, Value) -> uPut(64, Buffer, Offset, Value).

int32Put(Buffer, Offset, Value) -> iPut(32, Buffer, Offset, Value).

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
