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

-module(emagick_rs_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

init_per_suite(Config) ->
    ok = application:load(emagick_rs),
    Config.

all() ->
    [
        test_image_fit
    ].

test_image_fit(Config) ->
    DataDir = ?config(data_dir, Config),
    ImagePath = filename:join([DataDir, "IMG_5745.JPG"]),
    {ok, ImageData} = file:read_file(ImagePath),
    {ok, Resized} = emagick_rs:image_fit(ImageData, 240, 240),
    ?assert(is_binary(Resized)),
    ?assertEqual(length(binary_to_list(Resized)), 31907),
    ok.
