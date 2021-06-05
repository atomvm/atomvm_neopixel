# AtomVM NeoPixel Example Program

The `neopixel_example` program illustrates use of the neopixel API by rotating each pixel through the HSV color space at different rates, making the LED strip slowly morph through different colors.

> Note.  Building and flashing the `neopixel_example` program requires installation of the [`rebar3`](https://www.rebar3.org) Erlang build tool.

To run this example program, you will need to connect your ESP32 device to a WS2812 LED strip.  Connect the negative lead on your LED strip to the ground pin on your ESP32, the data lead on your LED strip to pin 18 on your ESP32, and the positive lead on the LED strip to a +5v power source (e.g., on an ESP32 dev board or power supply on the same ground plane as the ESP32).

                                      +-----------+   +-----------+       +-----------+
    +-----------+ gnd             neg |           |   |           |       |           |
    |           +---------------------+           +---+           +- ... -+           |
    |   ESP32   |                     |           |   |           |       |           |
    |           | 18             data |           |   |           |       |           |
    |           +---------------------+           +---+           +- ... -+           |
    |           |                     |           |   |           |       |           |
    |           | +5v             pos |           |   |           |       |           |
    |           +---------------------+           +---+           +- ... -+           |
    |           |                     +-----------+   +-----------+       +-----------+
    |           |                     WS2812 LED strip
    |           |
    |           |
    |           |
    +-----------+

Build the example program and flash to your device:

    shell$ rebar3 esp32_flash -p /dev/ttyUSB0

> Note.  This build step makes use of the [`atomvm_rebar3_plugin`](https://github.com/fadushin/atomvm_rebar3_plugin).  See the `README.md` for information about parameters for setting the serial port and baud rate for your platform.

Attach to the console using the `monitor` Make target in the AtomVM ESP32 build:

    shell$ cd .../AtomVM/src/platform/esp32
    shell$ make monitor
    Toolchain path: /work/opt/xtensa-esp32-elf/bin/xtensa-esp32-elf-gcc
    WARNING: Toolchain version is not supported: crosstool-ng-1.22.0-95-ge082013a
    ...
    Found AVM partition: size: 1048576, address: 0x210000
    Booting file mapped at: 0x3f420000, size: 1048576
    I (243) atomvm_neopixel: eFuse Two Point: NOT supported
    I (243) atomvm_neopixel: eFuse Vref: Supported
    Found AVM partition: size: 1048576, address: 0x110000
    Starting: neopixel_example.beam...
    ---
    I (779) atomvm_neopixel: Installed WS2812 driver.

The pixels in your LED strip should start red and gradually change color.
