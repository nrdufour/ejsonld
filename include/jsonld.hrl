
% This file is part of ejsonldp released under the MIT license.
% See the LICENSE file for more information.

% Keywords
-define(LOCAL_CONTEXT_KEY, <<"@context">>).
-define(BASE_KEY, <<"@base">>).
-define(REMOTE_CONTEXT_KEY, <<"@profile">>).
-define(VOCAB_KEY, <<"@vocab">>).
-define(COERCE_KEY, <<"@coerce">>).
-define(LITERAL_KEY, <<"@literal">>).
-define(IRI_KEY, <<"@iri">>).
-define(LANGUAGE_KEY, <<"@language">>).
-define(DATATYPE_KEY, <<"@datatype">>).
-define(SUBJECT_KEY, <<"@subject">>).
-define(TYPE_KEY, <<"@type">>).

-define(TYPE_IRI, <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#type">>).

-define(DEFAULT_KEYWORDS,
[
        %?LOCAL_CONTEXT_KEY, % The is obviously not overridable ;-)
        ?BASE_KEY,
        ?REMOTE_CONTEXT_KEY,
        ?VOCAB_KEY,
        %?COERCE_KEY, % XXX This can't be seriously overriden!
        ?LITERAL_KEY,
        ?IRI_KEY,
        ?LANGUAGE_KEY,
        ?DATATYPE_KEY,
        ?SUBJECT_KEY,
        ?TYPE_KEY
]
).

-define(IS_OBJECT(Obj), ej_util:is_proplist(Obj)).

-define(HAS_VALUE(Proplist, Key), lists:keyfind(Key, 1, Proplist)).
