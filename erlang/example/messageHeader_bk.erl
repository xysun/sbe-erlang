-module(messageHeader_bk).
-compile(export_all).

wrap(Buffer, Offset, MessageTemplateVersion) ->
    {Buffer, Offset, MessageTemplateVersion}.

% below must be higher-order functions, taking wrap() as last argument, for chaining

% we already know the sequence:
% blockLength(uint16) -> templateId(uint16) -> schemaId(uint16) -> version(uint16)

setBlockLength({Buffer, Offset, MessageTemplateVersion}, BlockLength) -> 
    NewBuffer = buffer:uint16Put(Buffer, Offset+0, BlockLength, little),
    {NewBuffer, Offset, MessageTemplateVersion}.

setTemplateId({Buffer, Offset, MessageTemplateVersion}, TemplateId) ->
    NewBuffer = buffer:uint16Put(Buffer, Offset+2, TemplateId, little),
    {NewBuffer, Offset, MessageTemplateVersion}.

setSchemaId({Buffer, Offset, MessageTemplateVersion}, SchemaId) ->
    NewBuffer = buffer:uint16Put(Buffer, Offset+4, SchemaId, little),
    {NewBuffer, Offset, MessageTemplateVersion}.

setVersion({Buffer, Offset, MessageTemplateVersion}, Version) ->
    NewBuffer = buffer:uint16Put(Buffer, Offset+6, Version, little),
    {NewBuffer, Offset, MessageTemplateVersion}.

getBlockLength({Buffer, Offset, _}) ->
    buffer:uint16Get(Buffer, Offset, little).

getTemplateId({Buffer, Offset, _}) ->
    buffer:uint16Get(Buffer, Offset+2, little).

getSchemaId({Buffer, Offset, _}) ->
    buffer:uint16Get(Buffer, Offset+4, little).

getVersion({Buffer, Offset, _}) ->
    buffer:uint16Get(Buffer, Offset+6, little).

% we know size already
size() -> 8.
