#!/usr/bin/env python3
"""Check DOOM's Ada platform layer (userspace/ports/doom) against the
doomgeneric headers it mirrors: key codes, screen size, zone tag, sound
device, and the field order and types of sfxinfo_t, sound_module_t and
music_module_t."""
import os
import re
import sys

here = os.path.dirname(os.path.abspath(__file__))
port = os.path.join(here, "../../userspace/ports/doom")
doom = os.path.join(os.environ["DOOMGENERIC_SRC"], "doomgeneric")
failures = 0


def fail(message):
    global failures
    failures += 1
    print("FAIL: " + message)


def read(path):
    with open(path) as f:
        return f.read()


def strip_c(text):
    return re.sub(r"//[^\n]*|/\*.*?\*/", "", text, flags=re.S)


def c_value(expression, defines):
    expression = expression.strip()
    expression = re.sub(r"'(\\?.)'", lambda m: str(ord(m.group(1)[-1])), expression)
    expression = re.sub(r"\b[A-Z_][A-Z0-9_]*\b",
                        lambda m: str(c_value(defines[m.group(0)], defines)), expression)
    return eval(expression, {})


def ada_value(expression, constants):
    expression = re.sub(r"(\d+)#([0-9A-Fa-f_]+)#",
                        lambda m: str(int(m.group(2).replace("_", ""), int(m.group(1)))),
                        expression)
    expression = re.sub(r"\b[A-Z][A-Za-z0-9_]*\b",
                        lambda m: str(ada_value(constants[m.group(0)], constants)), expression)
    return eval(expression.replace("_", ""), {})


def ada_constants(text):
    return dict(re.findall(r"^\s*(\w+)\s*:\s*constant(?:\s+\w+)?\s*:=\s*([^;]+);", text, re.M))


# Key codes.
defines = dict(re.findall(r"^#define[ \t]+(\w+)[ \t]+(.+?)[ \t]*$", strip_c(read(f"{doom}/doomkeys.h")), re.M))
keys_ada = read(f"{port}/cubit-doom_keys.ads")
key_constants = ada_constants(keys_ada)
key_names = {
    "Key_Right_Arrow": "KEY_RIGHTARROW", "Key_Left_Arrow": "KEY_LEFTARROW",
    "Key_Up_Arrow": "KEY_UPARROW", "Key_Down_Arrow": "KEY_DOWNARROW",
    "Key_Use": "KEY_USE", "Key_Fire": "KEY_FIRE", "Key_Escape": "KEY_ESCAPE",
    "Key_Enter": "KEY_ENTER", "Key_Tab": "KEY_TAB", "Key_Backspace": "KEY_BACKSPACE",
    "Key_Equals": "KEY_EQUALS", "Key_Minus": "KEY_MINUS",
    "Key_Right_Shift": "KEY_RSHIFT", "Key_Right_Alt": "KEY_RALT",
    "Key_Caps_Lock": "KEY_CAPSLOCK", "Key_Num_Lock": "KEY_NUMLOCK",
    "Key_Scroll_Lock": "KEY_SCRLCK", "Key_Home": "KEY_HOME", "Key_End": "KEY_END",
    "Key_Page_Up": "KEY_PGUP", "Key_Page_Down": "KEY_PGDN", "Key_Insert": "KEY_INS",
    "Key_Delete": "KEY_DEL",
}
key_names.update({f"Key_F{i}": f"KEY_F{i}" for i in range(1, 13)})
for ada_name, c_name in key_names.items():
    if ada_name not in key_constants:
        fail(f"{ada_name} missing")
        continue
    ours = ada_value(key_constants[ada_name], key_constants)
    theirs = c_value(defines[c_name], defines)
    if ours != theirs:
        fail(f"{ada_name} = {ours}, doomkeys.h {c_name} = {theirs}")
unmatched = [n for n in key_constants if n.startswith("Key_") and n not in key_names]
if unmatched:
    fail(f"key constants without a doomkeys.h counterpart: {unmatched}")

# Screen size, zone tag, sound device.
generic = read(f"{doom}/doomgeneric.h")
platform = ada_constants(read(f"{port}/cubit-doom_platform.adb"))
for ada_name, c_name in (("Screen_Width", "DOOMGENERIC_RESX"), ("Screen_Height", "DOOMGENERIC_RESY")):
    theirs = int(re.search(rf"#define {c_name} (\d+)", generic).group(1))
    if ada_value(platform[ada_name], platform) != theirs:
        fail(f"{ada_name} differs from {c_name} = {theirs}")
zone = strip_c(read(f"{doom}/z_zone.h"))
if not re.search(r"\bPU_STATIC\s*=\s*1\b", zone):
    fail("PU_STATIC is no longer 1")
module_body = read(f"{port}/cubit-doom_sound_module.adb")
if not re.search(r"Zone_Static : constant int := 1;", module_body):
    fail("Zone_Static is not PU_STATIC (1)")
sound_h = strip_c(read(f"{doom}/i_sound.h"))
if not re.search(r"\bSNDDEVICE_SB\s*=\s*3\b", sound_h):
    fail("SNDDEVICE_SB is no longer 3")
module_spec = read(f"{port}/cubit-doom_sound_module.ads")
if "Sound_Blaster : constant Sound_Device := 3;" not in module_spec:
    fail("Sound_Blaster is not SNDDEVICE_SB (3)")
if not re.search(r"typedef unsigned int boolean;", read(f"{doom}/doomtype.h")) and \
        "undef\t= 0xFFFFFFFF" not in read(f"{doom}/doomtype.h"):
    fail("doomtype.h's boolean is no longer 32 bits")


def c_struct(text, opening, closing):
    body = text[text.index(opening):]
    body = body[body.index("{") + 1:body.index(closing)]
    fields = []
    for declaration in body.split(";"):
        declaration = " ".join(declaration.split())
        if not declaration:
            continue
        pointer_function = re.match(r".*\(\s*\*\s*(\w+)\s*\)\s*\(", declaration)
        if pointer_function:
            fields.append((pointer_function.group(1), "function"))
            continue
        array = re.match(r"(.*?)\s*(\w+)\s*\[(\d+)\]$", declaration)
        if array:
            fields.append((array.group(2), f"array {array.group(1)} {array.group(3)}"))
            continue
        plain = re.match(r"(.*?)\s*(\*?)\s*(\w+)$", declaration)
        kind = "pointer" if plain.group(2) or "*" in plain.group(1) else plain.group(1)
        fields.append((plain.group(3), kind))
    return fields


def ada_record(text, name):
    body = text[text.index(f"type {name} is record"):]
    body = body[body.index("record") + len("record"):body.index("end record")]
    fields = []
    for names, kind in re.findall(r"^\s*([\w ,]+?)\s*:\s*([\w.]+)\s*;", body, re.M):
        for field in names.split(","):
            fields.append((field.strip(), kind))
    return fields


ada_kind = {"pointer": {"System.Address", "Device_List_Access"}}
structures = (
    ("sfxinfo_t", "struct sfxinfo_struct", "};", "Sfx_Info"),
    ("sound_module_t", "Interface for sound modules", "} sound_module_t", "Sound_Module"),
    ("music_module_t", "Interface for music modules", "} music_module_t", "Music_Module"),
)
for c_name, opening, closing, ada_name in structures:
    text = read(f"{doom}/i_sound.h")
    start = text.index(opening)
    theirs = c_struct(strip_c(text[start:]), "{" if "struct" not in opening else opening, closing)
    ours = ada_record(module_spec, ada_name)
    if len(theirs) != len(ours):
        fail(f"{ada_name} has {len(ours)} fields, {c_name} {len(theirs)}: {theirs}")
        continue
    for (c_field, c_kind), (ada_field, kind) in zip(theirs, ours):
        if c_kind.startswith("array"):
            length = int(c_kind.split()[-1])
            if kind != "Sfx_Name" or ada_value(ada_constants(module_spec)["Sfx_Name_Length"], {}) != length:
                fail(f"{ada_name}.{ada_field} is not char[{length}] like {c_field}")
        elif c_kind == "function":
            if kind in ("System.Address", "int"):
                fail(f"{ada_name}.{ada_field} should be a subprogram access like {c_field}")
        elif c_kind == "pointer":
            if kind not in ada_kind["pointer"]:
                fail(f"{ada_name}.{ada_field} ({kind}) should be a pointer like {c_field}")
        elif c_kind == "int":
            if kind != "int":
                fail(f"{ada_name}.{ada_field} ({kind}) should be int like {c_field}")
        else:
            fail(f"{c_name}.{c_field}: unexpected C type {c_kind}")

if failures:
    sys.exit(1)
print(f"doom layout: {len(key_names)} key codes, screen, zone tag, sound device, "
      f"{len(structures)} structures match doomgeneric")
