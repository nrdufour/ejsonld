
% This file is part of ejsonld released under the MIT license.
% See the LICENSE file for more information.

-module(ej_app).
-author("Nicolas R Dufour <nicolas.dufour@nemoworld.info>").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ej_sup:start_link().

stop(_State) ->
    ok.
