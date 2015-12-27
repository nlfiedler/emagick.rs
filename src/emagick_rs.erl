%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Nathan Fiedler
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(emagick_rs).
-export([image_fit/3]).
-on_load(init/0).

-define(APPNAME, emagick_rs).
-define(LIBNAME, libemagick_rs).

init() ->
    % tried using code:priv_dir/1 but it never seemed to work
    PrivDir = filename:join([filename:dirname(code:which(?APPNAME)), "..", "priv"]),
    SoName = filename:join(PrivDir, ?LIBNAME),
    ok = erlang:load_nif(SoName, 0).

%
% @doc Returns {ok, Binary} if successful, and {error, Reason} otherwise.
%
image_fit(_Bin, _Width, _Height) ->
    exit(nif_library_not_loaded).
