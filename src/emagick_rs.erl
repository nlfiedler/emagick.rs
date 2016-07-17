%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015-2016 Nathan Fiedler
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
-export([image_fit/3, image_get_property/2, auto_orient/1, requires_orientation/1]).
-on_load(init/0).

-define(APPNAME, emagick_rs).
-define(LIBNAME, libemagick_rs).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join("..", priv)) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join(priv, ?LIBNAME)
            end;
        Dir ->
            case filelib:is_dir(Dir) of
                true ->
                    filename:join(Dir, ?LIBNAME);
                _ ->
                    % Special case for use with escripts, since it seems
                    % rather difficult to inform erl where the priv_dir for
                    % this application should be.
                    case os:getenv("NIF_DIR") of
                        false -> filename:join(priv, ?LIBNAME);
                        Path -> filename:join(Path, ?LIBNAME)
                    end
            end
    end,
    ok = erlang:load_nif(SoName, 0).

%
% @doc Fits the image to the given dimensions, maintaining aspect ratio.
%      Returns {ok, Binary} if successful, and {error, Reason} otherwise.
%
image_fit(_Bin, _Width, _Height) ->
    exit(nif_library_not_loaded).

%
% @doc Retrieves the named property from the given image data.
%      Returns {ok, Value} if successful, and {error, Reason} otherwise.
%
image_get_property(_Bin, _Name) ->
    exit(nif_library_not_loaded).

%
% @doc Automatically orient the image so it is suitable for viewing.
%      Returns {ok, Binary} if successful, and {error, Reason} otherwise.
%
auto_orient(_Bin) ->
    exit(nif_library_not_loaded).

%
% @doc Return true if the image requires correction of the orientation,
%      and false otherwise. If correction is required, use auto_orient/1.
%
requires_orientation(_Bin) ->
    exit(nif_library_not_loaded).
