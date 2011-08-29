
% This file is part of ejsonld released under the MIT license.
% See the LICENSE file for more information.

-module(ej_context).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-include("jsonld.hrl").

-export([create_default/0, process_local_context/2]).
-export([has_prefix/2, get_prefix/2]).
-export([get_base/1, get_vocab/1, get_default/1]).
-export([is_keyword/2]).

-record(context, {
        % 'names' is a simple dict that maps a set of names to a set of IRI
        % a prefix can be an element name (for example 'name' in foaf:name)
        % or a namespace name (for example 'foaf' in foaf:name)
        names,

        % base is an IRI which prefix all object IRIs
        base = undefined,

        % vocab is an IRI which prefix all property IRIs
        vocab = undefined,

        % coerce is a proplist which maps a type to an element name
        coerce,

        % 'keywords' is a simple dict that maps names to json-ld keywords
        % such as @type or @subject, allowing you to override them for any reason
        % Only '@context' can't be overriden for obvious reason.
        keywords,

        % default context if it's a unique IRI
        default = undefined
    }
).

create_default() ->
    DefaultNames = create_default_names(),
    DefaultCoerce = create_default_coerce(),
    DefaultKeywords = create_default_keywords(),
    #context{ names = DefaultNames, coerce = DefaultCoerce, keywords = DefaultKeywords }.

process_local_context(JsonObject, Context) ->
    {Proplist} = JsonObject,

    % Local Context: merge if exists
    LocalContextProp = ?HAS_VALUE(Proplist, ?LOCAL_CONTEXT_KEY),

    case LocalContextProp of
        false -> Context;
        {_, Value} ->
            merge(Context, Value)
    end.

has_prefix(Context, Prefix) ->
    dict:is_key(Prefix, Context#context.names).

get_prefix(Context, Prefix) ->
    dict:fetch(Prefix, Context#context.names).

get_base(Context) ->
    Context#context.base.

get_vocab(Context) ->
    Context#context.vocab.

get_default(Context) ->
    Context#context.default.

is_keyword(Key, Context) ->
    dict:is_key(Key, Context#context.keywords).

%
% Internal API
%

% NewNames has to be a proplist here
merge(Context, ContextValues) ->
    case ContextValues of
        {[_|_]} ->
            {Proplist} = ContextValues,

            Fun = fun({Key, Value}, Ctx) ->
                case Key of
                    ?VOCAB_KEY  -> Ctx#context{ vocab = Value };
                    ?BASE_KEY   -> Ctx#context{ base  = Value };
                    ?COERCE_KEY ->
                        CoerceValue = ?HAS_VALUE(Proplist, ?COERCE_KEY),
                        case CoerceValue of
                            false ->
                                Ctx;
                            _     ->
                                {?COERCE_KEY, {CoerceValues}} = CoerceValue,
                                UpdatedCoerceDict = lists:foldl(
                                    fun({Type, Keys}, Dict) ->
                                        case is_list(Keys) of
                                            true  ->
                                                lists:foldl(
                                                    fun(Item, Acc) ->
                                                        dict:store(Item, Type, Acc)
                                                    end,
                                                    Dict,
                                                    Keys
                                                );
                                            false ->
                                                dict:store(Keys, Type, Dict)
                                        end
                                    end,
                                    Ctx#context.coerce,
                                    CoerceValues
                                ),

                                Ctx#context{coerce = UpdatedCoerceDict}
                        end;
                    _ ->
                        UpdatedNames = dict:store(Key, Value, Ctx#context.names),
                        Ctx#context{ names = UpdatedNames }
                end
            end,
            lists:foldl(Fun, Context, Proplist);
        Binary when is_binary(Binary) ->
            Context#context{ default = ContextValues };
        _ ->
            Context
    end.

%
% ---
%

create_default_names() ->
    InitialDict = dict:new(),
    lists:foldl(
        fun({Key, Value}, Dict) ->
            dict:store(Key, Value, Dict)
        end,
        InitialDict,
        ?DEFAULT_NAMES).

create_default_keywords() ->
    InitialDict = dict:new(),
    lists:foldl(
        fun(Element, Dict) ->
            dict:store(Element, Element, Dict)
        end,
        InitialDict,
        ?DEFAULT_KEYWORDS).

create_default_coerce() ->
    dict:new().
