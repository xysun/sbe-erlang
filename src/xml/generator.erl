-module(generator).
-include("types.hrl").
-compile(export_all).

% define primitive types
uint8() -> #primitiveType{name="uint8",
                       size=1,
                       minValue=0,
                       maxValue=254,
                       nullValue=255}.

uint16() -> #primitiveType{name="uint16",
                        size=2,
                        minValue=0,
                        maxValue=65534,
                        nullValue=65535}.

uint32() -> #primitiveType{name="uint32",
                        size=4,
                        minValue=0,
                        maxValue=65534,
                        nullValue=65535}.

uint64() ->  #primitiveType{name="uint64",
                        size=8,
                        minValue=0,
                        maxValue=65534,
                        nullValue=65535}.

% generate messageHeader.erl
generateMessageHeader(Attributes) -> 
    Uint8 = uint8(),
    Uint16 = uint16(),
    Uint32 = uint32(),
    Uint64 = uint64(),
    FilePath = "../example/messageHeader.erl",
    % make sure we start fresh
    {ok, IoDevice_overwrite} = file:open(FilePath, [write]),
    
    % file header
    generateFileHeader(IoDevice_overwrite, "messageHeader"),
    file:close(IoDevice_overwrite),
    {ok, IoDevice} = file:open(FilePath, [append]),
    
    % wrap
    io:format(IoDevice,
        "~n~nwrap(Buffer, Offset, MessageTemplateVersion) ->"
         ++"~n    {Buffer, Offset, MessageTemplateVersion}.", []),

    % fields: blockLength, schemaId, templateId, version
    % must be unsigned integer type
    % following xml ordering
    Size = lists:foldl(
        fun(D, Acc) -> 
            Dname = capfirst(dict:fetch(name, D)),
            case dict:fetch(primitiveType, D) of
                "uint8" -> generatePrimitivePropertyMethods(IoDevice,Dname, Uint8, Acc, little), Acc + Uint8#primitiveType.size;
                "uint16" -> generatePrimitivePropertyMethods(IoDevice,Dname,Uint16, Acc, little), Acc + Uint16#primitiveType.size;
                "uint32" -> generatePrimitivePropertyMethods(IoDevice,Dname,Uint32, Acc, little), Acc + Uint32#primitiveType.size;
                "uint64" -> generatePrimitivePropertyMethods(IoDevice,Dname,Uint64, Acc, little), Acc + Uint64#primitiveType.size;
                _ -> error(must_be_unsigned_integer)
            end
        end, 0, Attributes),

    % size
    io:format(IoDevice, 
        "~n~nsize() -> ~w.", [Size]),

    % close
    file:close(IoDevice),
    lists:map(fun(D) -> dict:fetch(name, D) end, Attributes).

generatePrimitivePropertyMethods(IoDevice, Name, #primitiveType{} = P, Offset, Endian) ->
    % set methods
    io:format(IoDevice,
        "~n~nset~s({Buffer, Offset, MessageTemplateVersion}, ~s) ->"
        ++"~n    NewBuffer = buffer:~sPut(Buffer, Offset + ~w, ~s, ~w),"
        ++"~n    {NewBuffer, Offset, MessageTemplateVersion}.",
        [Name, Name, P#primitiveType.name, Offset, Name, Endian]),
    % get methods
    io:format(IoDevice,
        "~n~nget~s({Buffer, Offset, _}) ->"
        ++"~n    buffer:~sGet(Buffer, Offset + ~w, ~w).",
        [Name, P#primitiveType.name, Offset, Endian]).

generateFileHeader(IoDevice, ModuleName) -> 
    io:format(IoDevice, "-module(~s).~n-compile(export_all).", [ModuleName]).


% utility functions

capfirst([H|T]) when H >= $a, H =< $z ->
    [H + ($A - $a) | T];
capfirst(Other) -> Other.
