
% This file is part of ejsonld released under the MIT license.
% See the LICENSE file for more information.

-module(ej_compact).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([compact/1]).

-include("jsonld.hrl").


compact(Json) when is_list(Json) ->
    lists:foldl(
        fun(Item, Acc) ->
            CompactItem = compact(Item),
            Acc ++ [CompactItem]
        end,
        [],
        Json
    );
compact(Json) ->
    % expand the Json first
    ExpandedJson = ej_expand:expand(Json),

    % create the default context
    DefaultContext = ej_context:create_default(),

    {CompactedObject, Context} = compact_object(ExpandedJson, DefaultContext),

    ContextValue = convert_context_to_json_value(Context),

    {Props} = CompactedObject,

    ContextProperty = { ?LOCAL_CONTEXT_KEY, ContextValue },

    { Props ++ [ContextProperty] }.

%
% Internal API
%

convert_context_to_json_value(Context) ->
    case ej_context:get_default(Context) of
        undefined ->
            PrefixProplist = lists:foldl(
                fun(Prefix, Proplist) ->
                    URI = ej_context:get_prefix(Context, Prefix),
                    Property = {Prefix, URI},
                    Proplist ++ [Property]
                end,
                [],
                ej_context:get_all_prefixes(Context)
            ),

            CoerceCtx = ej_context:get_coerce_context(Context),
            CoerceProplist = lists:foldl(
                fun(Type, Acc) ->
                    Property = { Type, dict:fetch(Type, CoerceCtx) },
                    Acc ++ [Property]
                end,
                [],
                dict:fetch_keys(CoerceCtx)
            ),

            case CoerceProplist of
                [] ->
                    {PrefixProplist};
                _ ->
                    {PrefixProplist ++ [{?COERCE_KEY, {CoerceProplist}}]}
            end;
        Default ->
            Default
    end.

compact_object(Json, DefaultContext) when is_binary(Json) ->
    {Json, DefaultContext};
compact_object(Json, DefaultContext) when is_list(Json) ->
    lists:foldl(
        fun(Obj, {Acc, CurrentCtx}) ->
            {CompactedObjectItem, ContextFromObjectItem} = compact_object(Obj, CurrentCtx),
            {Acc ++ [CompactedObjectItem], ContextFromObjectItem}
        end,
        {[], DefaultContext},
        Json
    );
compact_object(Json, DefaultContext) ->
    {Proplist} = Json,
    {CompactedProplist, FinalContext} = lists:foldl(
        fun({Key, Value}, {Props, Context}) ->
            {KeyName, CompactedValue, UpdatedContext} = compact_key_value(Key, Value, Context),
            Property = {KeyName, CompactedValue},
            {Props ++ [Property], UpdatedContext}
        end,
        {[], DefaultContext},
        Proplist
    ),
    {{CompactedProplist}, FinalContext}.

compact_key_value(Key, Value, Context) ->
    KeyName = extract_key_name(Key),
    ContextWithKey = ej_context:register_prefix(KeyName, Key, Context),
    {FinalValue, ContextWithKeyAndValue} = case Value of
        {[ {?IRI_KEY, IRI} ]} ->
            ContextWithIRI = ej_context:register_coerce(KeyName, ?IRI_KEY, ContextWithKey),
            {IRI, ContextWithIRI};
        {[ {?DATATYPE_KEY, Datatype}, {?LITERAL_KEY, Literal} ]} ->
            ContextWithDatatype = ej_context:register_coerce(KeyName, Datatype, ContextWithKey),
            {Literal, ContextWithDatatype};
        _ ->
            compact_object(Value, ContextWithKey)
    end,
    {KeyName, FinalValue, ContextWithKeyAndValue}.

extract_key_name(Key) ->
    Value = binary_to_list(Key),
    iolist_to_binary([lists:last(string:tokens(Value, "/"))]).
