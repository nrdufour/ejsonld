
% This file is part of ejsonld released under the MIT license.
% See the LICENSE file for more information.

-module(ej_context).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-include("jsonld.hrl").

-export([create_default/0, process_local_context/2]).
-export([has_prefix/2, get_prefix/2]).
-export([get_base/1, get_vocab/1, get_default/1]).
-export([is_keyword/2]).
-export([has_coerce/2, get_coerce/2]).
-export([register_prefix/3]).
-export([get_all_prefixes/1]).
-export([register_coerce/3]).
-export([get_coerce_context/1]).

-record(context, {
        % 'names' is a simple dict that maps a set of names to a set of IRI
        % a prefix can be an element name (for example 'name' in foaf:name)
        % or a namespace name (for example 'foaf' in foaf:name)
        names,

        % base is an IRI which prefix all object IRIs
        base = undefined,

        % vocab is an IRI which prefix all property IRIs
        vocab = undefined,

        % coerce is a proplist which maps a element name to an type
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

    UpdatedContext = case LocalContextProp of
        false -> Context;
        {_, Value} ->
            merge(Context, Value)
    end,

    % see if @vocab exists
    % TODO need to really fully support keyword override
    VocabKey = ?VOCAB_KEY,
    VocabProp = ?HAS_VALUE(Proplist, VocabKey),
    CtxWithVocab = case VocabProp of
        false -> UpdatedContext;
        {_, VocabValue} -> UpdatedContext#context{ vocab = VocabValue }
    end,

    % see if @base exists
    % TODO need to really fully support keyword override
    BaseKey = ?BASE_KEY,
    BaseProp = ?HAS_VALUE(Proplist, BaseKey),
    case BaseProp of
        false -> CtxWithVocab;
        {_, BaseValue} -> CtxWithVocab#context{ base = BaseValue}
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

is_keyword(Key, _Context) ->
    lists:member(Key, ?DEFAULT_KEYWORDS).

has_coerce(Context, Key) ->
    dict:is_key(Key, Context#context.coerce).

get_coerce(Context, Key) ->
    dict:fetch(Key, Context#context.coerce).

register_prefix(Prefix, URI, Context) ->
    Names = Context#context.names,

    UpdatedNames = dict:store(Prefix, URI, Names),

    Context#context{ names = UpdatedNames}.

get_all_prefixes(Context) ->
    dict:fetch_keys(Context#context.names).

register_coerce(KeyName, Type, Context) ->
    Coerce = Context#context.coerce,

    UpdatedCoerce = dict:store(KeyName, Type, Coerce),

    Context#context{ coerce = UpdatedCoerce }.

get_coerce_context(Context) ->
    lists:foldl(
        fun(KeyName, Acc) ->
            Type = dict:fetch(KeyName, Context#context.coerce),
            StoredValue = case dict:is_key(Type, Acc) of
                true  ->
                    Value = dict:fetch(Type, Acc),
                    case is_list(Value) of
                        true ->
                            Value ++ [KeyName];
                        false ->
                            [Value, KeyName]
                    end;
                false ->
                    KeyName
            end,
            dict:store(Type, StoredValue, Acc)
        end,
        dict:new(),
        dict:fetch_keys(Context#context.coerce)
    ).

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
    dict:new().

% only useful once the override feature is supported
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
