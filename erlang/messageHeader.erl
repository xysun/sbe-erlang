-module(messageHeader).
-compile(export_all).

wrap(Buffer, Offset, MessageTemplateVersion) ->
    {Buffer, Offset, MessageTemplateVersion}.

% below must be higher-order functions, taking wrap() as last argument, for chaining

% we already know the sequence:
% blockLength(uint16) -> templateId(uint16) -> schemaId(uint16) -> version(uint16)

blockLength(BlockLength) -> 
    fun({Buffer, Offset, MessageTemplateVersion}) ->
        NewBuffer = buffer:uint16Put(Buffer, Offset+0, BlockLength),
        {NewBuffer, Offset, MessageTemplateVersion}
    end.

templateId(TemplateId) ->
    fun({Buffer, Offset, MessageTemplateVersion}) ->
        NewBuffer = buffer:uint16Put(Buffer, Offset+2, TemplateId),
        {NewBuffer, Offset, MessageTemplateVersion}
    end.

schemaId(SchemaId) ->
    fun({Buffer, Offset, MessageTemplateVersion}) ->
        NewBuffer = buffer:uint16Put(Buffer, Offset+4, SchemaId),
        {NewBuffer, Offset, MessageTemplateVersion}
    end.

version(Version) ->
    fun({Buffer, Offset, MessageTemplateVersion}) ->
        NewBuffer = buffer:uint16Put(Buffer, Offset+6, Version),
        {NewBuffer, Offset, MessageTemplateVersion}
    end.
   
% we know size already
size() -> 8.
