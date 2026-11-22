#!/usr/bin/env python3
"""Check SameBoy's Ada frontend (userspace/ports/sameboy) against the core
headers it mirrors: GB_key_t's order, the GB_model_t values, GB_sample_t's
layout and the core functions' argument types."""
import os
import re
import sys

here = os.path.dirname(os.path.abspath(__file__))
port = os.path.join(here, "../../userspace/ports/sameboy")
core = os.path.join(os.environ["SAMEBOY_SRC"], "Core")
failures = 0


def fail(message):
    global failures
    failures += 1
    print("FAIL: " + message)


def read(path):
    with open(path) as f:
        return re.sub(r"//[^\n]*|/\*.*?\*/", "", f.read(), flags=re.S)


keys = read(f"{port}/cubit-sameboy_keys.ads")
frontend = read(f"{port}/cubit-sameboy_frontend.adb")

joypad = read(f"{core}/joypad.h")
c_keys = re.search(r"typedef enum\s*{([^}]*)}\s*GB_key_t;", joypad).group(1)
c_keys = [k.strip() for k in c_keys.split(",") if k.strip() and k.strip() != "GB_KEY_MAX"]
ada_keys = re.search(r"type Pad_Key is \(([^)]*)\);", keys).group(1)
ada_keys = [k.strip() for k in ada_keys.split(",")]
names = {"Right": "GB_KEY_RIGHT", "Left": "GB_KEY_LEFT", "Up": "GB_KEY_UP",
         "Down": "GB_KEY_DOWN", "A": "GB_KEY_A", "B": "GB_KEY_B",
         "Select_Button": "GB_KEY_SELECT", "Start": "GB_KEY_START"}
if [names.get(k) for k in ada_keys] != c_keys:
    fail(f"Pad_Key {ada_keys} does not follow GB_key_t {c_keys}")
for position, key in enumerate(ada_keys):
    if not re.search(rf"\b{key} => {position}\b", keys):
        fail(f"Pad_Key {key} is not represented as {position}")

model = read(f"{core}/model.h")
for ada_name, c_name in (("Model_Dmg_B", "GB_MODEL_DMG_B"), ("Model_Cgb_E", "GB_MODEL_CGB_E")):
    theirs = int(re.search(rf"\b{c_name}\s*=\s*(0x[0-9a-fA-F]+)", model).group(1), 16)
    ours = int(re.search(rf"{ada_name}\s*: constant C.unsigned := 16#([0-9A-F]+)#", frontend).group(1), 16)
    if ours != theirs:
        fail(f"{ada_name} = {ours:#x}, {c_name} = {theirs:#x}")

apu = read(f"{core}/apu.h")
plain = re.search(r"#else\s*typedef struct\s*{([^}]*)}\s*GB_sample_t;", apu)
if not plain or [line.split() for line in plain.group(1).strip().split(";") if line.strip()] != \
        [["int16_t", "left"], ["int16_t", "right"]]:
    fail("GB_sample_t is no longer int16_t left, right (one 32-bit Stereo_Frame)")

# Argument types of the imported functions: pointers are System.Address
# (Game_Boy), bool is C_Bool, enums are C.unsigned/C.int.
gb = read(f"{core}/gb.h") + joypad + apu + read(f"{core}/display.h")
expected = {
    "GB_set_key_state": ["GB_gameboy_t *gb", "GB_key_t index", "bool pressed"],
    "GB_set_turbo_mode": ["GB_gameboy_t *gb", "bool on", "bool no_frame_skip"],
    "GB_load_rom_from_buffer": ["GB_gameboy_t *gb", "const uint8_t *buffer", "size_t size"],
    "GB_load_boot_rom_from_buffer": ["GB_gameboy_t *gb", "const unsigned char *buffer", "size_t size"],
    "GB_set_sample_rate": ["GB_gameboy_t *gb", "unsigned sample_rate"],
    "GB_get_clock_rate": ["GB_gameboy_t *gb"],
    "GB_init": ["GB_gameboy_t *gb", "GB_model_t model"],
}
for function, arguments in expected.items():
    found = re.search(rf"\b{function}\s*\(([^)]*)\)\s*;", gb)
    if not found:
        fail(f"{function} not declared")
        continue
    theirs = [" ".join(a.split()) for a in found.group(1).split(",")]
    if theirs != arguments:
        fail(f"{function}({', '.join(theirs)}) changed; expected ({', '.join(arguments)})")
if not re.search(r"\bbool GB_is_cgb\(const GB_gameboy_t \*gb\);", gb):
    fail("GB_is_cgb no longer returns bool")
if not re.search(r"\bunsigned GB_run\(GB_gameboy_t \*gb\);", gb):
    fail("GB_run no longer returns unsigned")
if not re.search(r"typedef uint32_t \(\*GB_rgb_encode_callback_t\)\(GB_gameboy_t \*gb, uint8_t r, uint8_t g, uint8_t b\);", gb):
    fail("GB_rgb_encode_callback_t changed")

if failures:
    sys.exit(1)
print("sameboy core: key order, models, sample layout and imported signatures match")
