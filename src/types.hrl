% primitiveTypes
-record(primitiveType, {name,
                         size,
                         minValue,
                         maxValue,
                         nullValue
                        }).

-define(UINT8, #primitiveType{name="uint8",
                       size=1,
                       minValue=0,
                       maxValue=254,
                       nullValue=255}).

-define(INT8, #primitiveType{name="int8",
                       size=1,
                       minValue=-127,
                       maxValue=127,
                       nullValue=-128}).


-define(UINT16, #primitiveType{name="uint16",
                        size=2,
                        minValue=0,
                        maxValue=65534,
                        nullValue=65535}).

-define(INT16, #primitiveType{name="int16",
                        size=2,
                        minValue=-32767,
                        maxValue=32767,
                        nullValue=-32768}).

-define(UINT32, #primitiveType{name="uint32",
                        size=4,
                        minValue=0,
                        maxValue=round(math:pow(2,32))-2,
                        nullValue=round(math:pow(2,32))-1}).

-define(INT32, #primitiveType{name="int32",
                        size=4,
                        minValue=-round(math:pow(2,31))+1,
                        maxValue=round(math:pow(2,31))-1,
                        nullValue=-round(math:pow(2,31))}).


-define(UINT64, #primitiveType{name="uint64",
                        size=8,
                        minValue=0,
                        maxValue=round(math:pow(2,64))-2,
                        nullValue=round(math:pow(2,64))-1}).

-define(INT64, #primitiveType{name="int64",
                        size=8,
                        minValue=-round(math:pow(2,63))+1,
                        maxValue=round(math:pow(2,63))-1,
                        nullValue=-round(math:pow(2,63))}).

-define(CHAR, #primitiveType{name="char",
                             size = 1,
                             minValue = 16#20,
                             maxValue = 16#7e,
                             nullValue = 0}).

% simple, fixed-length data type
-record(simpleType, {name,
                     primitiveType,
                     length=1,
                     size,
                     characterEncoding=utf8}).

% composite variable length type
-record(varDataType, {name,
                      lengthPrimitiveType,
                      characterEncoding=utf8}).

