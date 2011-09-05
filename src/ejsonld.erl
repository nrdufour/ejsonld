

% This file is part of ejsonld released under the MIT license.
% See the LICENSE file for more information.

-module(ejsonld).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([expand/1, compact/1, frame/3, normalize/1]).

% Process JSON expressed in EEP018

expand(Json) ->
    ej_expand:expand(Json).

compact(Json) ->
    ej_compact:compact(Json).

frame(Json, _Frame, _Options) ->
    Json.

normalize(Json) ->
    Json.
