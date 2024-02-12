# Neopixel

This AtomVM Erlang library and Nif can be used to control WS2812 LED strips using the ESP32 SoC for any Erlang/Elixir programs targeted for AtomVM on the ESP32 platform.

The AtomVM NeoPixel library is only supported on the ESP32 platform.

## Build Instructions

The AtomVM NeoPixel library is implemented as an AtomVM component, which includes some native C code that must be linked into the ESP32 AtomVM image.  In order to build and deploy this client code, you must build an AtomVM binary image with this component compiled and linked into the image.

For general instructions about how to build AtomVM and include third-party components into an AtomVM image, see the [AtomVM Build Instructions](https://doc.atomvm.net/build-instructions.html).

Once the AtomVM image including this component has been built, you can flash the image to your ESP32 device.  For instructions about how to flash AtomVM images to your ESP32 device, see the AtomVM [Getting Started Guide](https://doc.atomvm.net/getting-started-guide.html).

Once the AtomVM image including this component has been flashed to your ESP32 device, you can then include this project into your [`rebar3`](https://www.rebar3.org) project using the [`atomvm_rebar3_plugin`](https://github.com/atomvm/atomvm_rebar3_plugin), which provides targets for building AtomVM packbeam files and flashing them to your device.

## Programmer's Guide

The `atomvm_neopixel` library can be used to drive a strip of [WS2812](https://cdn-shop.adafruit.com/datasheets/WS2812.pdf) "Neopixel" LEDs.

Neopixel LED strips are individually addressable sets of "pixels", where each pixel contains 3 LEDs (red, blue, green).  Each pixel can be configured using three 8-bit (`0..255`) values for red, green, and blue respectively, using an RGB color space.  Alternatively, users may specify a hue (`0..359`), saturation (`0..100`), and value (`0..100`), using an HSV color space.

AtomVM programmers interface with the `atomvm_neopixel` API via the `neopixel` module, which provides operations for starting and stopping an Erlang process associated with a specified LED strip, and for setting values on each pixel in the strip.

This guide provides an overview of the interfaces provided by this module.

### Lifecyle

Use the `neopixel:start/2` function to initialize a neopixel instance.  Specify the ESP32 pin to which the LED strip is connected, as well as the number of LEDs in the strip.  This function will return a reference to a neopixel process, which is used in subsequent operations.

    %% erlang
    Pin = 18,
    NumPixels = 4,
    {ok NeoPixel} = neopixel:start(Pin, NumPixels),
    ...

Use the `neopixel:start/3` function to initialize a neopixel instance with non-default options.  Options are represented in an Erlang map.  The permissible entries are encapsulated in the following table:

| Key | Type | Default | Description |
| ----- | ----- | ------| ----|
| `timeout`   | `non_neg_integer()` | 100 | Timeout (in milliseconds) used internally when communicating with the LED strip |
| `channel`   | `channel_0\|channel_1\|channel_2\|channel_3` | `channel_0` | ESP RTC transmit channel to use.  Use a different channel for each `neopixel` instance created. |

For example,

    %% erlang
    Pin = 18,
    NumPixels = 4,
    {ok NeoPixel} = neopixel:start(Pin, NumPixels, #{channel => channel_1}),
    ...

The returned `NeoPixel` instance should be used for subsequent operations.

Use the `neopixel:stop/1` function to stop a neopixel instance and free any resources in use by it.

    %% erlang
    neopixel:stop(NeoPixel).

### Clearing pixels

Use the `neopixel:clear/1` function to clear all the pixels in the LED strip to an "off" value.

    %% erlang
    ok = neopixel:clear(NeoPixel).

Clearing the LED strip will set all RGB values to 0.

### Setting Pixel values

Pixel colors can be set using and RGB or HSV color space.

Note that setting values will not change the state of the LED strip until the NeoPixel instance is refreshed (see below).

#### RGB Color Space

Pixel colors can be set using the RGB color space via the `neopixel:set_pixel_rgb/5` function.  Pixel indices are in the range `[0..NumPixels-1]`.

For example, to set the second pixel in the strip (index 1) to all red, use:

    %% erlang
    ok = neopixel:set_pixel_rgb(NeoPixel, 1, 255, 0, 0).

RGB values and their ranges are summarized in the following table:

| Parameter | Range | Description |
| ----- | ----- | ------|
| red   | `0..255` | Value of red LED |
| green | `0..255` | Value of green LED |
| blue | `0..255` | Value of blue LED |

#### HSV color space

Pixel colors can be set using the HSV color space via the `neopixel:set_pixel_hsv/5` function.  Pixel indices are in the range `[0..NumPixels-1]`.

Information about the HSV color space is available via [this article](https://en.wikipedia.org/wiki/HSL_and_HSV).

For example, to set the second pixel in the strip (index 1) to all red, use:

    %% erlang
    ok = neopixel:set_pixel_hsv(NeoPixel, 1, 0, 100, 100).

HSV values and their ranges are summarized in the following table:

| Parameter | Range | Description |
| ----- | ----- | ------|
| hue   | `0..259` | Pixel hue, expressed as degrees of rotation around a circle, starting with red at 0 degrees, green at 120 degrees, and blue at 240 degrees. |
| saturation | `0..100` | Color saturation, as a percentage, with 0 being all white, and 100 maximum color saturation. |
| value | `0..100` | Value, as a percentage, with 0 being all dark, and 100 maximum brightness. |

### Refreshing pixels

Use the `neopixel:refresh/1` function to ref refresh all the pixels in the LED strip.

    %% erlang
    ok = neopixel:refresh(NeoPixel).

Refreshing the LED strip will manifest any changes made via any previous `set_pixel_*` operations (see above).

### API Reference

To generate Reference API documentation in HTML, issue the rebar3 target

    shell$ rebar3 edoc

from the top level of the `atomvm_neopixel` source tree.  Output is written to the `doc` directory.
