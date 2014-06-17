-module(messageHeader).
-compile(export_all).

wrap(Buffer, Offset, MessageTemplateVersion) ->
    {Buffer, Offset, MessageTemplateVersion}.

setTemplateId({Buffer, Offset, MessageTemplateVersion}, TemplateId) ->
    NewBuffer = buffer:uint16Put(Buffer, Offset + 0, TemplateId, little),
    {NewBuffer, Offset, MessageTemplateVersion}.

getTemplateId({Buffer, Offset, _}) ->
    buffer:uint16Get(Buffer, Offset + 0, little).

setBlockLength({Buffer, Offset, MessageTemplateVersion}, BlockLength) ->
    NewBuffer = buffer:uint16Put(Buffer, Offset + 2, BlockLength, little),
    {NewBuffer, Offset, MessageTemplateVersion}.

getBlockLength({Buffer, Offset, _}) ->
    buffer:uint16Get(Buffer, Offset + 2, little).

setSchemaId({Buffer, Offset, MessageTemplateVersion}, SchemaId) ->
    NewBuffer = buffer:uint16Put(Buffer, Offset + 4, SchemaId, little),
    {NewBuffer, Offset, MessageTemplateVersion}.

getSchemaId({Buffer, Offset, _}) ->
    buffer:uint16Get(Buffer, Offset + 4, little).

setVersion({Buffer, Offset, MessageTemplateVersion}, Version) ->
    NewBuffer = buffer:uint16Put(Buffer, Offset + 6, Version, little),
    {NewBuffer, Offset, MessageTemplateVersion}.

getVersion({Buffer, Offset, _}) ->
    buffer:uint16Get(Buffer, Offset + 6, little).

size() -> 8.