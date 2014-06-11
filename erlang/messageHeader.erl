-module(messageHeader).
-compile(export_all).

wrap(Buffer, Offset, MessageTemplateVersion) ->
    {Buffer, Offset, MessageTemplateVersion}.

% below must be higher-order functions, taking wrap() as last argument, for chaining

% we already know the sequence:
% blockLength(uint16) -> templateId(uint16) -> schemaId(uint16) -> version(uint16)

blockLength({Buffer, Offset, MessageTemplateVersion}, BlockLength) -> 
    NewBuffer = buffer:uint16Put(Buffer, Offset+0, BlockLength),
    {NewBuffer, Offset, MessageTemplateVersion}.

templateId({Buffer, Offset, MessageTemplateVersion}, TemplateId) ->
    NewBuffer = buffer:uint16Put(Buffer, Offset+2, TemplateId),
    {NewBuffer, Offset, MessageTemplateVersion}.

schemaId({Buffer, Offset, MessageTemplateVersion}, SchemaId) ->
    NewBuffer = buffer:uint16Put(Buffer, Offset+4, SchemaId),
    {NewBuffer, Offset, MessageTemplateVersion}.

version({Buffer, Offset, MessageTemplateVersion}, Version) ->
    NewBuffer = buffer:uint16Put(Buffer, Offset+6, Version),
    {NewBuffer, Offset, MessageTemplateVersion}.

getBlockLength({Buffer, Offset, _}) ->
    buffer:uint16Get(Buffer, Offset).

getTemplateId({Buffer, Offset, _}) ->
    buffer:uint16Get(Buffer, Offset+2).

getSchemaId({Buffer, Offset, _}) ->
    buffer:uint16Get(Buffer, Offset+4).

getVersion({Buffer, Offset, _}) ->
    buffer:uint16Get(Buffer, Offset+6).

% we know size already
size() -> 8.
