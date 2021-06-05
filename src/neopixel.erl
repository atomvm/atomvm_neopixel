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
%%-----------------------------------------------------------------------------
%% @doc WS2812 ("Neopixel") support.
%%
%% Use this module to drive a strip of WS2812 "noepixel" LED strips.
%%
%% Each LED in a strip is individually addressable and can be configured in
%% 24-bit color, using either a Red-Green-Blue (RGB) or Hue-Saturation-Value (HSV)
%% color space.
%% @end
%%-----------------------------------------------------------------------------
-module(neopixel).

-export([
    start/2, start/3, stop/1, clear/1, set_pixel_rgb/5, set_pixel_hsv/5, refresh/1
]).
-export([nif_init/3, nif_clear/2, nif_refresh/2, nif_set_pixel_hsv/5, nif_set_pixel_rgb/5, nif_tini/2]). %% internal nif APIs
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_server).

-type neopixel() :: term().
-type pin() :: non_neg_integer().
-type options() :: [option()].
-type option() :: #{timeout => non_neg_integer(), channel => channel()}.
-type channel() :: channel_0 | channel_1 | channel_2 | channel_3.

-type color() :: 0..255.
-type hue() :: 0..359.
-type saturation() :: 0..100.
-type value() :: 0..100.

-define(DEFAULT_OPTIONS, #{timeout => 100, channel => channel_0}).

-record(state, {
    pin :: pin(),
    num_pixels :: non_neg_integer(),
    nif_handle :: binary(),
    options :: options()
}).


%%-----------------------------------------------------------------------------
%% @param   Pin     pin connected to neopixel strip.
%% @returns ok | {error, Reason}
%% @doc     Start a neopixel driver.
%% @end
%%-----------------------------------------------------------------------------
-spec start(Pin::pin(), NumPixels::non_neg_integer()) -> {ok, neopixel()} | {error, Reason::term()}.
start(Pin, NumPixels) ->
    start(Pin, NumPixels, maps:new()).

%%-----------------------------------------------------------------------------
%% @param   Pin         pin connected to neopixel strip
%% @param   Options     extra options
%% @returns ok | {error, Reason}
%% @doc     Start a neopixel driver.
%%
%% Use the returned reference in subsequent neopixel operations.
%% @end
%%-----------------------------------------------------------------------------
-spec start(Pin::pin(), NumPixels::non_neg_integer(), Options::options()) -> {ok, neopixel()} | {error, Reason::term()}.
start(Pin, NumPixels, Options) ->
    gen_server:start(?MODULE, [Pin, NumPixels, validate_options(maps:merge(Options, ?DEFAULT_OPTIONS))], []).

%%-----------------------------------------------------------------------------
%% @returns ok
%% @doc     Stop the specified neopixel driver.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(Neopixel::neopixel()) -> ok.
stop(Neopixel) ->
    gen_server:call(Neopixel, stop).

%%-----------------------------------------------------------------------------
%% @param   Neopixel        Neopixel instance
%% @returns ok | {error, Reason}
%% @doc     Clear the neopixel strip.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec clear(Neopixel::neopixel()) -> ok | {error, Reason::term()}.
clear(Neopixel) ->
    gen_server:call(Neopixel, clear).

%%-----------------------------------------------------------------------------
%% @param   Neopixel        Neopixel instance
%% @returns ok | {error, Reason}
%% @doc     Refresh the neopixel strip.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec refresh(Neopixel::neopixel()) -> ok | {error, Reason::term()}.
refresh(Neopixel) ->
    gen_server:call(Neopixel, refresh).

%%-----------------------------------------------------------------------------
%% @param   Neopixel        Neopixel instance
%% @param   I               pixel index (`0..NumPixels - 1')
%% @param   R               Red value (`0..255')
%% @param   G               Green value (`0..255')
%% @param   B               Blue value (`0..255')
%% @returns ok | {error, Reason}
%% @doc     Set a pixel value in the RGB color space.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec set_pixel_rgb(Neopixel::neopixel(), I::non_neg_integer(), R::color(), G::color(), B::color()) -> ok | {error, Reason::term()}.
set_pixel_rgb(Neopixel, I, R, G, B) when is_pid(Neopixel), 0 =< R, R =< 255, 0 =< G, G =< 255, 0 =< B, B =< 255 ->
    gen_server:call(Neopixel, {set_pixel_rgb, I, R, G, B});
set_pixel_rgb(_Neopixel, _I, _R, _G, _B) ->
    throw(badarg).

%%-----------------------------------------------------------------------------
%% @param   Neopixel        Neopixel instance
%% @param   I               pixel index (`0..NumPixels - 1')
%% @param   H               Hue value (`0..359')
%% @param   S               Saturation value (`0..100')
%% @param   V               Value (`0..100')
%% @returns ok | {error, Reason}
%% @doc     Set a pixel value in the HSV color space.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec set_pixel_hsv(Neopixel::neopixel(), I::non_neg_integer(), H::hue(), S::saturation(), V::value()) -> ok | {error, Reason::term()}.
set_pixel_hsv(Neopixel, I, H, S, V) when is_pid(Neopixel), 0 =< H, H < 360, 0 =< S, S =< 100, 0 =< V, V =< 100 ->
    gen_server:call(Neopixel, {set_pixel_hsv, I, H, S, V});
set_pixel_hsv(_Neopixel, _I, _R, _G, _B) ->
    throw(badarg).

%%
%% gen_server API
%%

%% @hidden
init([Pin, NumPixels, Options]) ->
    Handle = ?MODULE:nif_init(Pin, NumPixels, maps:get(channel, Options)),
    {ok, #state{
        pin=Pin,
        num_pixels=NumPixels,
        options=Options,
        nif_handle=Handle
    }}.

%% @hidden
handle_call(stop, _From, State) ->
    {stop, normal, ?MODULE:nif_tini(State#state.nif_handle, maps:get(channel, State#state.options)), State};
handle_call(clear, _From, State) ->
    {reply, ?MODULE:nif_clear(State#state.nif_handle, maps:get(timeout, State#state.options)), State};
handle_call(refresh, _From, State) ->
    {reply, ?MODULE:nif_refresh(State#state.nif_handle, maps:get(timeout, State#state.options)), State};
handle_call({set_pixel_rgb, I, R, G, B}, _From, State) ->
    {reply, ?MODULE:nif_set_pixel_hsv(State#state.nif_handle, I, R, G, B), State};
handle_call({set_pixel_hsv, I, H, S, V}, _From, State) ->
    {reply, ?MODULE:nif_set_pixel_hsv(State#state.nif_handle, I, H, S, V), State};
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% internal operations
%%

%% @private
validate_options(Options) ->
    validate_timeout_option(maps:get(timeout, Options, undefined)),
    validate_channel_option(maps:get(channel, Options, undefined)),
    Options.

%% @private
validate_timeout_option(Timeout) when is_integer(Timeout), 0 =< Timeout ->
    ok;
validate_timeout_option(_Timeout) ->
    throw(badarg).

%% @private
validate_channel_option(channel_0) ->
    ok;
validate_channel_option(channel_1) ->
    ok;
validate_channel_option(channel_2) ->
    ok;
validate_channel_option(channel_3) ->
    ok;
validate_channel_option(_Timeout) ->
    throw(badarg).


%%
%% Nifs
%%

%% @hidden
nif_init(_Pin, _NumPixels, _Channel) ->
    throw(nif_error).

%% @hidden
nif_clear(_NifHandle, _Timeout) ->
    throw(nif_error).

%% @hidden
nif_refresh(_NifHandle, _Timeout) ->
    throw(nif_error).

%% @hidden
nif_set_pixel_rgb(_NifHandle, _Index, _Red, _Green, _Blue) ->
    throw(nif_error).

%% @hidden
nif_set_pixel_hsv(_NifHandle, _Index, _Hue, _Saturation, _Value) ->
    throw(nif_error).

%% @hidden
nif_tini(_NifHandle, _Channel) ->
    throw(nif_error).
