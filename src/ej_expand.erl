
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

expand(Json) when is_list(Json) ->
    % Grab a default context
    Context = ej_context:create_default(),

    % Process if it's an object or a list
    case ?IS_OBJECT(Json) of
        % It's an object, just expand it
        true  ->
            expand_object(Json, Context);
        % it's a list: go through each element
        false ->
            lists:foldl(
                fun(Item, Acc) ->
                    ExpandedObject = expand_object(Item, Context),
                    Acc ++ [ExpandedObject]
                end,
                [],
                Json
            )
    end;
expand(Json) ->
    Json.

%
% Internal API
%

expand_object(Json, Context) ->
    % First let's see if there is a context local to that object
    CurrentContext = ej_context:process_local_context(Json, Context),

    % Then process each property by expanding the key and the value
    lists:foldl(
        fun(Item, Acc) ->
            {Key, Value} = Item,
            case Key of
                % Ignoring the context property now
                ?LOCAL_CONTEXT_KEY ->
                    Acc;
                % Processing all the others
                _ ->
                    ExpandedProp = {
                        expand_object_key(Key, CurrentContext),
                        expand_object_value(Value, CurrentContext)
                    },
                    Acc ++ [ExpandedProp]
            end
        end,
        [],
        Json
    ).

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
            % TODO need to add that default context
            Vocab = ej_context:get_vocab(Context),
            case Vocab of
                % no vocab is defined!
                % let's see if there is a default context
                undefined ->
                    case ej_context:has_prefix(Context, Name) of
                        false ->
                            % TODO define a default context in jsonld_context.erl
                            % In case somebody would do:
                            % @context: "http://...."

                            % throwing an error for now
                            throw([bad_property, Key]);
                        true ->
                            ej_context:get_prefix(Context, Name)
                    end;
                % we have a vocab: prepend the name with it
                _ -> iolist_to_binary([Vocab, Name])
            end;
        % anything else: error
        _ ->
            throw([bad_property, Key])
    end.

expand_object_value(Value, Context) when is_list(Value) ->
    case ?IS_OBJECT(Value) of
        true  ->
            expand_object(Value, Context);
        false ->
            lists:foldl(
                fun(Item, Acc) ->
                    ExpandedValue = case ?IS_OBJECT(Item) of
                        true  -> expand_object(Item, Context);
                        false -> expand_object_value(Item, Context)
                    end,
                    Acc ++ [ExpandedValue]
                end,
                [],
                Value
            )
    end;
expand_object_value(Value, _Context) ->
    Value.
