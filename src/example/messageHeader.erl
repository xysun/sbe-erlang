-module(messageHeader).
-compile(export_all).

wrap(Buffer, Offset, MessageTemplateVersion) ->
    {Buffer, Offset, MessageTemplateVersion}.

setTemplateId(TemplateId) ->
    fun({Buffer, Offset, MessageTemplateVersion}) ->
        NewBuffer = buffer:uint16Put(Buffer, Offset + 0, TemplateId, little),
        {NewBuffer, Offset, MessageTemplateVersion}
    end.

getTemplateId({Buffer, Offset, _}) ->
    buffer:uint16Get(Buffer, Offset + 0, little).

setBlockLength(BlockLength) ->
    fun({Buffer, Offset, MessageTemplateVersion}) ->
        NewBuffer = buffer:uint16Put(Buffer, Offset + 2, BlockLength, little),
        {NewBuffer, Offset, MessageTemplateVersion}
    end.

getBlockLength({Buffer, Offset, _}) ->
    buffer:uint16Get(Buffer, Offset + 2, little).

setSchemaId(SchemaId) ->
    fun({Buffer, Offset, MessageTemplateVersion}) ->
        NewBuffer = buffer:uint16Put(Buffer, Offset + 4, SchemaId, little),
        {NewBuffer, Offset, MessageTemplateVersion}
    end.

getSchemaId({Buffer, Offset, _}) ->
    buffer:uint16Get(Buffer, Offset + 4, little).

setVersion(Version) ->
    fun({Buffer, Offset, MessageTemplateVersion}) ->
        NewBuffer = buffer:uint16Put(Buffer, Offset + 6, Version, little),
        {NewBuffer, Offset, MessageTemplateVersion}
    end.

getVersion({Buffer, Offset, _}) ->
    buffer:uint16Get(Buffer, Offset + 6, little).

size() -> 8.