-module(utils).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

% utility functions

% capitilize first letter
capfirst([H|T]) when H >= $a, H =< $z ->
    [H + ($A - $a) | T];
capfirst(Other) -> Other.


% force first letter to be lowercase
lowerfirst([H|T]) when H >= $A, H =< $Z ->
    [H - ($A - $a) | T];
lowerfirst(Other) -> Other.

getAttributesDict(Node) -> 
    lists:foldl(
        fun(#xmlAttribute{} = T, Dict) -> dict:store(T#xmlAttribute.name, T#xmlAttribute.value, Dict) end,
        dict:new(), Node#xmlElement.attributes).

% fetch key from dict, use default value if not exist
fetchWithDefault(Key, Dict, Default) ->
    Find = dict:find(Key, Dict),
    case Find of
        {ok, Value} -> Value;
        error -> Default
    end.
