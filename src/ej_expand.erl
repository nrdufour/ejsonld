
% This file is part of ejsonld released under the MIT license.
% See the LICENSE file for more information.

-module(ej_expand).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([expand/1]).

-include("jsonld.hrl").

% Patterns to recognize an IRI (absolute or not, curie, bnode)
-define(ABSOLUTE_IRI, "^(?<iri>(\\w+)\\://([^>\\s]+))$").
-define(ABSOLUTE_CURIE, "^(?<iri>(?<prefix>\\w+)\\:(?<name>\\w+))$").
-define(RELATIVE_CURIE, "^(?<iri>(\\:)?(?<name>\\w+))$").
-define(BNODE, "^_\\:(?<id>\\w+)$").

expand(Json) ->
    Context = ej_context:create_default(),
    expand(Json, Context).

expand([], _Context) ->
    [];
expand({[]}, _Context) ->
    {[]};
expand(Json, Context) when is_list(Json) ->
    lists:foldl(
        fun(Item, Acc) ->
            ExpandedObject = expand(Item, Context),
            Acc ++ [ExpandedObject]
        end,
        [],
        Json
    );
expand(Json, Context) ->
    case Json of
        {[_|_]} ->
            % First let's see if there is a context local to that object
            CurrentContext = ej_context:process_local_context(Json, Context),

            {Proplist} = Json,
            % Then process each property by expanding the key and the value
            ExpandedProplist = lists:foldl(
                fun(Item, Acc) ->
                    {Key, Value} = Item,
                    case Key of
                        % Ignoring the context property now
                        ?LOCAL_CONTEXT_KEY ->
                            Acc;
                        ?TYPE_KEY ->
                            ExpandedProp = {
                                ?TYPE_IRI,
                                expand_object_value(Value, CurrentContext, ?IRI_KEY)
                            },
                            Acc ++ [ExpandedProp];
                        % Processing all the others
                        _ ->
                            ExpandedProp = case ej_context:has_coerce(CurrentContext, Key) of
                                false ->
                                    {
                                        expand_object_key(Key, CurrentContext),
                                        expand(Value, CurrentContext)
                                    };
                                true  ->
                                    CoerceType = ej_context:get_coerce(CurrentContext, Key),
                                    {
                                        expand_object_key(Key, CurrentContext),
                                        expand_object_value(Value, CurrentContext, CoerceType)
                                    }
                            end,
                            Acc ++ [ExpandedProp]
                    end
                end,
                [],
                Proplist
            ),

            {ExpandedProplist};
        _ ->
            Json
    end.

%
% Internal API
%

% Process a triple property.
% In JSON-LD, it's the key.
%
% It can be:
% - a JSON-LD keyword
% - an absolute IRI such as 'http://xmlns.com/foaf/0.1/name'
% - a CURIE such as 'foaf:name'
% - a relative CURIE such as ':name' or 'name'
%
% returns the expanded property IRI
expand_object_key(Key, Context) ->
    case ej_context:is_keyword(Key, Context) of
        true  ->
            Key;
        false ->
            expand_iri(Key, Context)
    end.

expand_object_value(Value, Context, CoerceType) ->
    case CoerceType of
        ?IRI_KEY ->
            case is_list(Value) of
                true  ->
                    lists:foldl(
                        fun(Item, Acc) ->
                            ExpandedIRI = expand_iri(Item, Context),
                            Acc ++ [{[ { ?IRI_KEY, ExpandedIRI } ]}]
                        end,
                        [],
                        Value
                    );
                false ->
                    ExpandedIRI = expand_iri(Value, Context),
                    {[ { ?IRI_KEY, ExpandedIRI } ]}
            end;
        _ ->
            ExpandedCoerce = expand_iri(CoerceType, Context),
            {[ { ?DATATYPE_KEY, ExpandedCoerce }, { ?LITERAL_KEY, Value } ]}
    end.

expand_iri(Key, Context) when is_list(Key) ->
    lists:foldl(
        fun(Item, Acc) ->
            IRI = expand_iri(Item, Context),
            Acc ++ [IRI]
        end,
        [],
        Key
    );
expand_iri(Key, Context) ->
    % regexp to identify which type of property we have
    AbsoluteIri = re:run(Key, ?ABSOLUTE_IRI, [{capture, ['iri'], binary}]),
    AbsoluteCurie = re:run(Key, ?ABSOLUTE_CURIE, [{capture, ['iri', 'prefix', 'name'], binary}]),
    RelativeCurie = re:run(Key, ?RELATIVE_CURIE, [{capture, ['iri', 'name'], binary}]),

    % time to see which one we have here
    case {AbsoluteIri, AbsoluteCurie, RelativeCurie} of
        % we have an absolute IRI
        {{match, [_IRI]}, _,  _} -> Key;
        % we have an absolute CURIE
        {_, {match, [_IRI, Prefix, Name]}, _} ->
            case ej_context:has_prefix(Context, Prefix) of
                true ->
                    URI = ej_context:get_prefix(Context, Prefix),
                    iolist_to_binary([URI, Name]);
                % this case is a bit odd.
                % it's like having 'foo:bar' without knowing what is 'foo'
                false -> throw([unknown_prefix, Key, Prefix])
            end;
        % we have a relative CURIE
        {_, _, {match, [_IRI, Name]}} ->
            % let's grab the @vocab value
            % or the default context URI
            Vocab = ej_context:get_vocab(Context),
            case Vocab of
                % no vocab is defined!
                % let's see if there is a default context
                undefined ->
                    case ej_context:has_prefix(Context, Name) of
                        false ->
                            % see if a default namespace exists in the context
                            DefaultNamespace = ej_context:get_default(Context),
                            case DefaultNamespace of
                                undefined ->
                                    % throwing an error for now
                                    throw([bad_property, Key]);
                                _ ->
                                    iolist_to_binary([DefaultNamespace, Key])
                            end;
                        true ->
                            % the relative curie is registered in the context
                            ej_context:get_prefix(Context, Name)
                    end;
                % we have a vocab: prepend the name with it
                _ -> iolist_to_binary([Vocab, Name])
            end;
        % anything else: error
        _ ->
            throw([bad_property, Key])
    end.
