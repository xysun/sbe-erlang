% buffer utilities
% these are not "real" buffers though..

-module(buffer).
-export([allocate/1, checkLimit/2,
         charGet/2, charPut/3, charsGet/3, charsPut/5,
         uint8Get/3, uint8Put/4, int8Get/3, int8Put/4,
         uint16Get/3, uint16Put/4, int16Get/3, int16Put/4,
         uint32Get/3, uint32Put/4, int32Get/3, int32Put/4,
         uint64Get/3, uint64Put/4, int64Get/3, int64Put/4]).

% helper functions
% allocate x bytes
allocate(Capacity) -> << <<0>> || _ <- lists:seq(1, Capacity) >>.

checkLimit(Buffer, Limit) -> 
    if Limit > size(Buffer) -> error(limit_beyond_capacity);
       true -> ok
    end.

% Size is in bits
% unsigned Put
uPut(Size, Buffer, Offset, Value, little) ->
    OffsetInBits = 8 * Offset,
    case Buffer of
        << Header:OffsetInBits, _:Size/integer-unsigned-little, Rest/binary >> ->
            << Header:OffsetInBits, Value:Size/integer-unsigned-little, Rest/binary >>
    end;
uPut(Size, Buffer, Offset, Value, big) ->
    OffsetInBits = 8 * Offset,
    case Buffer of
        << Header:OffsetInBits, _:Size/integer-unsigned-big, Rest/binary >> ->
            << Header:OffsetInBits, Value:Size/integer-unsigned-big, Rest/binary >>
    end.


% unsigned Get
uGet(Size, Buffer, Offset, little) -> 
    OffsetInBits = 8 * Offset, 
    case Buffer of
        << _:OffsetInBits, Value:Size/integer-unsigned-little, _/binary >> -> 
            Value
    end;
uGet(Size, Buffer, Offset, big) -> 
    OffsetInBits = 8 * Offset, 
    case Buffer of
        << _:OffsetInBits, Value:Size/integer-unsigned-big, _/binary >> -> 
            Value
    end.

% signed Put
iPut(Size, Buffer, Offset, Value, little) ->
    OffsetInBits = 8 * Offset,
    case Buffer of
        << Header:OffsetInBits, _:Size/integer-signed-little, Rest/binary >> ->
            << Header:OffsetInBits, Value:Size/integer-signed-little, Rest/binary >>
    end;
iPut(Size, Buffer, Offset, Value, big) ->
    OffsetInBits = 8 * Offset,
    case Buffer of
        << Header:OffsetInBits, _:Size/integer-signed-big, Rest/binary >> ->
            << Header:OffsetInBits, Value:Size/integer-signed-big, Rest/binary >>
    end.

% signed Get
iGet(Size, Buffer, Offset, little) -> 
    OffsetInBits = 8 * Offset,
    case Buffer of
        << _:OffsetInBits, Value:Size/integer-signed-little, _/binary>> ->
            Value
    end;
iGet(Size, Buffer, Offset, big) -> 
    OffsetInBits = 8 * Offset,
    case Buffer of
        << _:OffsetInBits, Value:Size/integer-signed-big, _/binary>> ->
            Value
    end.

uint8Put(Buffer, Offset, Value, Endian) -> uPut(8, Buffer, Offset, Value, Endian).
uint16Put(Buffer, Offset, Value, Endian) -> uPut(16, Buffer, Offset, Value, Endian).
uint32Put(Buffer, Offset, Value, Endian) -> uPut(32, Buffer, Offset, Value, Endian).
uint64Put(Buffer, Offset, Value, Endian) -> uPut(64, Buffer, Offset, Value, Endian).

uint8Get(Buffer, Offset, Endian) -> uGet(8, Buffer, Offset, Endian).
uint16Get(Buffer, Offset, Endian) -> uGet(16, Buffer, Offset, Endian).
uint32Get(Buffer, Offset, Endian) -> uGet(32, Buffer, Offset, Endian).
uint64Get(Buffer, Offset, Endian) -> uGet(64, Buffer, Offset, Endian).

int8Put(Buffer, Offset, Value, Endian) -> iPut(8, Buffer, Offset, Value, Endian).
int16Put(Buffer, Offset, Value, Endian) -> iPut(16, Buffer, Offset, Value, Endian).
int32Put(Buffer, Offset, Value, Endian) -> iPut(32, Buffer, Offset, Value, Endian).
int64Put(Buffer, Offset, Value, Endian) -> iPut(64, Buffer, Offset, Value, Endian).

int8Get(Buffer, Offset, Endian) -> iGet(8, Buffer, Offset, Endian).
int16Get(Buffer, Offset, Endian) -> iGet(16, Buffer, Offset, Endian).
int32Get(Buffer, Offset, Endian) -> iGet(32, Buffer, Offset, Endian).
int64Get(Buffer, Offset, Endian) -> iGet(64, Buffer, Offset, Endian).

% char, default us_ascii encoding
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
        <<_:OffsetInBits, Value/utf8, _/binary>> ->
            %binary:bin_to_list(Value)
            Value
    end.


charPut(Buffer, Offset, Value) ->
    OffsetInBits = 8 * Offset,
    case Buffer of
        <<Header:OffsetInBits, _/utf8, Rest/binary>> ->
            <<Header:OffsetInBits, Value/utf8, Rest/binary>>
    end.

