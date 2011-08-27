
% This file is part of ejsonld released under the MIT license.
% See the LICENSE file for more information.

-module(ej_util).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([is_proplist/1]).

is_proplist(Object) when is_list(Object) ->
    Fun = fun(X, Acc) ->
        IsProp = case X of
            {_, _} -> true;
            _      -> false
        end,
        Acc and IsProp
    end,
    lists:foldl(Fun, true, Object);
is_proplist(_) ->
    false.

%
% Internal API
%
