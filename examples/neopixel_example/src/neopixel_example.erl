%%
%% Copyright (c) 2021 dushin.net
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-module(neopixel_example).

-export([start/0]).

-define(NEOPIXEL_PIN, 18).
-define(NUM_PIXELS, 9).

-define(SATURATION, 100).
-define(VALUE, 15).

start() ->
    {ok, NeoPixel} = neopixel:start(?NEOPIXEL_PIN, ?NUM_PIXELS),
    ok = neopixel:clear(NeoPixel),
    lists:foreach(
        fun(I) ->
            spawn(fun() -> loop(NeoPixel, I, 0, atomvm_lib:random(100, 500)) end)
        end,
        lists:seq(0, ?NUM_PIXELS - 1)
    ),
    atomvm_lib:sleep_forever().

loop(NeoPixel, I, Hue, SleepMs) ->
    ok = neopixel:set_pixel_hsv(NeoPixel, I, Hue, ?SATURATION, ?VALUE),
    ok = neopixel:refresh(NeoPixel),
    timer:sleep(SleepMs),
    loop(NeoPixel, I, (Hue + 1) rem 360, SleepMs).
