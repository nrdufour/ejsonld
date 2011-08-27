

% This file is part of ejsonld released under the MIT license.
% See the LICENSE file for more information.

-module(ejsonld).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-export([start/0, expand/1, compact/1, frame/3, normalize/1]).

start() ->
    application:start(ejsonld).

% Process JSON expressed in EEP018

expand(Json) ->
    ej_expand:expand(Json).

compact(Json) ->
    Json.

frame(Json, _Frame, _Options) ->
    Json.

normalize(Json) ->
    Json.
