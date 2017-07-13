%% -*- coding: utf-8 -*-
%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015-2017 Nathan Fiedler
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

% So this has to be defined now?
end_per_suite(Config) ->
    Config.

all() ->
    [
        test_image_fit,
        test_image_get_property,
        test_image_get_format,
        test_auto_orient,
        test_requires_orientation
    ].

test_image_fit(Config) ->
    DataDir = ?config(data_dir, Config),
    ImagePath = filename:join([DataDir, "IMG_5745.JPG"]),
    {ok, ImageData} = file:read_file(ImagePath),
    {ok, Resized} = emagick_rs:image_fit(ImageData, 240, 240),
    ?assert(is_binary(Resized)),
    Length = length(binary_to_list(Resized)),
    % have to be flexible as size differs from one platform to another
    ?assert(31000 =< Length),
    ?assert(Length =< 32000),
    ok.

test_image_get_property(Config) ->
    DataDir = ?config(data_dir, Config),
    ImagePath = filename:join([DataDir, "IMG_5745.JPG"]),
    {ok, ImageData} = file:read_file(ImagePath),
    {ok, Value} = emagick_rs:image_get_property(ImageData, "exif:DateTimeOriginal"),
    ?assert(is_list(Value)),
    ?assertEqual("2014:04:23 13:33:08", Value),
    ok.

test_image_get_format(Config) ->
    DataDir = ?config(data_dir, Config),
    ImagePath = filename:join([DataDir, "IMG_5745.JPG"]),
    {ok, ImageData} = file:read_file(ImagePath),
    {ok, Value} = emagick_rs:image_get_format(ImageData),
    ?assert(is_list(Value)),
    ?assertEqual("JPEG", Value),
    ok.

test_auto_orient(Config) ->
    DataDir = ?config(data_dir, Config),
    ImagePath = filename:join([DataDir, "IMG_5745_rotl.JPG"]),
    {ok, ImageData} = file:read_file(ImagePath),
    {ok, Oriented} = emagick_rs:auto_orient(ImageData),
    ?assert(is_binary(Oriented)),
    Length = length(binary_to_list(Oriented)),
    % have to be flexible as size differs from one platform to another
    ?assert(100000 =< Length),
    ?assert(Length =< 110000),
    ok.

test_requires_orientation(Config) ->
    DataDir = ?config(data_dir, Config),
    ImagePath = filename:join([DataDir, "IMG_5745.JPG"]),
    {ok, ImageData} = file:read_file(ImagePath),
    ?assertNot(emagick_rs:requires_orientation(ImageData)),
    ImagePathR = filename:join([DataDir, "IMG_5745_rotl.JPG"]),
    {ok, ImageDataR} = file:read_file(ImagePathR),
    ?assert(emagick_rs:requires_orientation(ImageDataR)),
    ok.
