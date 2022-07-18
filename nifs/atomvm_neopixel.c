//
// Copyright (c) 2021 dushin.net
// All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include <stdlib.h>

#include <esp_log.h>
#include <esp32_sys.h>
#include <driver/rmt.h>

#include <context.h>
#include <defaultatoms.h>
#include <nifs.h>
#include <term.h>

#include "atomvm_neopixel.h"
#include "led_strip.h"

// #define ENABLE_TRACE
#include "trace.h"

#define TAG "atomvm_neopixel"
#define NO_ALLOC_FLAGS 0

// References
// https://docs.espressif.com/projects/esp-idf/en/v3.3.4/api-reference/peripherals/rmt.html
//

static const char *const led_strip_atom       = "\x9"  "led_strip";
static const char *const channel_0_atom       = "\x9"  "channel_0";
static const char *const channel_1_atom       = "\x9"  "channel_1";
static const char *const channel_2_atom       = "\x9"  "channel_2";
static const char *const channel_3_atom       = "\x9"  "channel_3";
//                                                      123456789ABCDEF01


static inline term ptr_to_binary(void *ptr, Context* ctx)
{
    return term_from_literal_binary(&ptr, sizeof(int), ctx);
}


static inline void *binary_to_ptr(term binary)
{
    if (term_binary_size(binary) != sizeof(int)) {
        return NULL;
    }
    const char *ptr = term_binary_data(binary);
    return *((void **) ptr);
}


static rmt_channel_t get_rmt_channel(Context *ctx, term channel)
{
    if (channel == context_make_atom(ctx, channel_0_atom)) {
        return RMT_CHANNEL_1;
    } else if (channel == context_make_atom(ctx, channel_1_atom)) {
        return RMT_CHANNEL_2;
    } else if (channel == context_make_atom(ctx, channel_2_atom)) {
        return RMT_CHANNEL_3;
    } else if (channel == context_make_atom(ctx, channel_3_atom)) {
        return RMT_CHANNEL_4;
    } else {
        return RMT_CHANNEL_MAX;
    }
}


static term nif_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term pin = argv[0];
    VALIDATE_VALUE(pin, term_is_integer);
    term num_pixels = argv[1];
    VALIDATE_VALUE(num_pixels, term_is_integer);
    term channel = argv[2];
    VALIDATE_VALUE(channel, term_is_atom);

    rmt_channel_t rmt_channel = get_rmt_channel(ctx, channel);

    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    } else {
        rmt_config_t config = RMT_DEFAULT_CONFIG_TX(term_to_int(pin), rmt_channel);
        // set counter clock to 40MHz
        config.clk_div = 2;

        esp_err_t err = rmt_config(&config);
        if (err != ESP_OK) {
            TRACE("Failed to initialize rmt config.  err=%i\n", err);
            term error_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, term_from_int(err));
            return error_tuple;
        }
        err = rmt_driver_install(config.channel, 0, NO_ALLOC_FLAGS);
        if (err != ESP_OK) {
            TRACE("Failed to install rmt driver.  err=%i\n", err);
            term error_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, term_from_int(err));
            return error_tuple;
        }
        led_strip_config_t strip_config = LED_STRIP_DEFAULT_CONFIG(term_to_int(num_pixels), (led_strip_dev_t) config.channel);
        led_strip_t *strip = led_strip_new_rmt_ws2812(&strip_config);
        if (!strip) {
            TRACE("Failed to install WS2812 driver.\n");
            term error_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, context_make_atom(ctx, led_strip_atom));
            return error_tuple;
        }
        ESP_LOGI(TAG, "Installed WS2812 driver.");
        return ptr_to_binary(strip, ctx);
    }
}


static term nif_clear(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term handle = argv[0];
    VALIDATE_VALUE(handle, term_is_binary);
    term timeout = argv[1];
    VALIDATE_VALUE(timeout, term_is_integer);

    led_strip_t *strip = (led_strip_t *) binary_to_ptr(handle);

    esp_err_t err = strip->clear(strip, term_to_int(timeout));
    if (err != ESP_OK) {
        TRACE("Failed to clear led strip.  err=%i\n", err);
        if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        } else {
            term error_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, term_from_int(err));
            return error_tuple;
        }
    }
    TRACE("Cleared led strip.\n");
    return OK_ATOM;
}


static term nif_refresh(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term handle = argv[0];
    VALIDATE_VALUE(handle, term_is_binary);
    term timeout = argv[1];
    VALIDATE_VALUE(timeout, term_is_integer);

    led_strip_t *strip = (led_strip_t *) binary_to_ptr(handle);

    esp_err_t err = strip->refresh(strip, term_to_int(timeout));
    if (err != ESP_OK) {
        TRACE("Failed to refresh led strip.  err=%i\n", err);
        if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        } else {
            term error_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, term_from_int(err));
            return error_tuple;
        }
    }
    TRACE("Refreshed led strip.\n");
    return OK_ATOM;
}


static term nif_set_pixel_rgb(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term handle = argv[0];
    VALIDATE_VALUE(handle, term_is_binary);
    term index = argv[1];
    VALIDATE_VALUE(index, term_is_integer);
    term red = argv[2];
    VALIDATE_VALUE(red, term_is_integer);
    term green = argv[3];
    VALIDATE_VALUE(green, term_is_integer);
    term blue = argv[4];
    VALIDATE_VALUE(blue, term_is_integer);

    led_strip_t *strip = (led_strip_t *) binary_to_ptr(handle);

    avm_int_t i = term_to_int(index);
    esp_err_t err = strip->set_pixel(strip, i, term_to_int(red), term_to_int(green), term_to_int(blue));
    if (err != ESP_OK) {
        TRACE("Failed to set pixel value on index %i (r=%i g=%i b=%i).  err=%i\n", i, red, green, blue, err);
        if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        } else {
            term error_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, term_from_int(err));
            return error_tuple;
        }
    }
    TRACE("Set pixel %i to r=%i g=%i b=%i\n", i, term_to_int(red), term_to_int(green), term_to_int(blue));
    return OK_ATOM;
}


static term nif_set_pixel_hsv(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term handle = argv[0];
    VALIDATE_VALUE(handle, term_is_binary);
    term index = argv[1];
    VALIDATE_VALUE(index, term_is_integer);
    term hue = argv[2];
    VALIDATE_VALUE(hue, term_is_integer);
    term saturation = argv[3];
    VALIDATE_VALUE(saturation, term_is_integer);
    term value = argv[4];
    VALIDATE_VALUE(value, term_is_integer);

    led_strip_t *strip = (led_strip_t *) binary_to_ptr(handle);

    uint32_t red = 0;
    uint32_t green = 0;
    uint32_t blue = 0;
    led_strip_hsv2rgb(term_to_int(hue), term_to_int(saturation), term_to_int(value), &red, &green, &blue);

    avm_int_t i = term_to_int(index);
    esp_err_t err = strip->set_pixel(strip, i, red, green, blue);
    if (err != ESP_OK) {
        TRACE("Failed to set pixel value on index %i (r=%i g=%i b=%i).  err=%i\n", i, red, green, blue, err);
        if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        } else {
            term error_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, term_from_int(err));
            return error_tuple;
        }
    }
    TRACE("Set pixel %i to r=%i g=%i b=%i\n", i, red, green, blue);
    return OK_ATOM;
}


static term nif_tini(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term handle = argv[0];
    VALIDATE_VALUE(handle, term_is_binary);
    term channel = argv[1];
    VALIDATE_VALUE(channel, term_is_atom);

    led_strip_t *strip = (led_strip_t *) binary_to_ptr(handle);

    esp_err_t err = strip->del(strip);
    if (err != ESP_OK) {
        TRACE("Failed to delete led strip.  err=%i\n", err);
        if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        } else {
            term error_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, term_from_int(err));
            return error_tuple;
        }
    }

    rmt_channel_t rmt_channel = get_rmt_channel(ctx, channel);
    err = rmt_driver_uninstall(term_to_int(rmt_channel));
    if (err != ESP_OK) {
        TRACE("Failed to uninstall rmt driver.  err=%i\n", err);
        if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        } else {
            term error_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, term_from_int(err));
            return error_tuple;
        }
    }
    TRACE("LED strip niti'd\n");
    return OK_ATOM;
}


static const struct Nif init_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_init
};
static const struct Nif clear_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_clear
};
static const struct Nif refresh_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_refresh
};
static const struct Nif set_pixel_hsv_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_set_pixel_hsv
};
static const struct Nif set_pixel_rgb_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_set_pixel_rgb
};
static const struct Nif tini_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_tini
};


//
// Component Nif Entrypoints
//

void atomvm_neopixel_init(GlobalContext *global)
{
    // no-op
}

const struct Nif *atomvm_neopixel_get_nif(const char *nifname)
{
    TRACE("Locating nif %s ...", nifname);
    if (strcmp("neopixel:nif_init/3", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &init_nif;
    }
    if (strcmp("neopixel:nif_clear/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &clear_nif;
    }
    if (strcmp("neopixel:nif_refresh/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &refresh_nif;
    }
    if (strcmp("neopixel:nif_set_pixel_rgb/5", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &set_pixel_rgb_nif;
    }
    if (strcmp("neopixel:nif_set_pixel_hsv/5", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &set_pixel_hsv_nif;
    }
    if (strcmp("neopixel:nif_tini/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &tini_nif;
    }
    return NULL;
}

#include <sdkconfig.h>
#ifdef CONFIG_AVM_NEOPIXEL_ENABLE
REGISTER_NIF_COLLECTION(atomvm_neopixel, atomvm_neopixel_init, atomvm_neopixel_get_nif)
#endif
