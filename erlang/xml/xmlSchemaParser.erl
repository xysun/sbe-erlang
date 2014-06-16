-module(xmlSchemaParser).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

findTypes(Node, Xpath) -> ok.


parse(Filename) -> 
    MessageHeader_XPATH = "/messageSchema/types/composite[@name='messageHeader']/type",
    {Root, _} = xmerl_scan:file(Filename),
    MessageHeaderAttributes = getAttributesFromXpath(Root, MessageHeader_XPATH),
    generator:generateMessageHeader(MessageHeaderAttributes).

test() -> 
    {Root, _} = xmerl_scan:file("../example-schema-simple.xml"),
    getAttributesFromXpath(Root, "/messageSchema/types/composite[@name='messageHeader']/type").

main() ->
    parse("../example-schema-simple.xml").

% return a {name:value} attribute dicts for each matching element
getAttributesFromXpath(Node, Xpath) -> 
    lists:reverse(
        lists:foldl(
            fun(#xmlElement{} = E, Acc) -> 
                [lists:foldl(
                    fun(#xmlAttribute{} = A, Dict) ->
                        dict:store(A#xmlAttribute.name, A#xmlAttribute.value, Dict)
                    end,
                    dict:new(), E#xmlElement.attributes
                )|Acc]
            end,
            [], xmerl_xpath:string(Xpath, Node)
        )
    ).
