#!/usr/bin/env python3
"""One-time translation of native CuBit Ada program projects (.gpr) into CCL
build declarations (docs/ccl-build.md, "Migration from make").

The user decided (2026-10-19) that .gpr files are translated once and then
deleted: CCL is the source of truth, and gprbuild is never ported. This tool
handles exactly the GPR subset native programs use; anything else is reported
and left alone, never guessed at:

  for Languages use ("Ada");          for Object_Dir use "build";
  for Source_Dirs use (...);          for Main use ("<one unit>");
  for Runtime ("Ada") use "<user runtime>";   for Library_Dir use "";
  package Builder:  for Executable ("<main>") use "<name>";
                    for Default_Switches ("Ada") use (...);
  package Compiler, Binder, Linker: for Default_Switches ("Ada") use (...);
  -- comments

Usage:
  gpr_to_ccl.py survey                 classify every project in the tree
  gpr_to_ccl.py convert GPR [--write]  print (or write beside it) component.ccl

The output is typed CCL: `ccl-build check` runs it through the type checker.
"""
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
MAKEFILE = ROOT / "kernel/Makefile"
RUNTIME = "userspace/runtime"
GENERATED = "build/generated"
OBJECTS = "build"
CONVENTIONAL_MAIN = "main.adb"
MANIFEST_OBJECT = "build/manifest.o"
# Builder switches that are gprbuild's own, not the compiler's.
BUILDER_ONLY = {"-s", "-v", "-p", "-nostdlib"}
SUFFIXES = {".app", ".svc", ".drv"}


class Unsupported(Exception):
    pass


def strip_comments(text):
    return "\n".join(line.split("--", 1)[0] for line in text.splitlines())


def string_list(text):
    return re.findall(r'"((?:[^"]|"")*)"', text)


# with'ed projects CCL build models: an externally built library (its
# archive is linked), an abstract project of per-file switches (a library
# of unit switches in build-params.ccl), or one that only supplies
# variables (nothing to model).
LIBRARY_NAMES = {"fonts_native.gpr": "fonts-native", "toolkit_flags.gpr": "toolkit-flags"}
VARIABLE_ONLY = {"calc_flags.gpr"}


def expression(text, variables):
    """A list expression: a parenthesised list of strings or a variable,
    possibly qualified (Toolkit_Flags.Pixel_Loop_Switches)."""
    text = text.strip()
    if text.startswith("("):
        return string_list(text)
    key = text.lower()
    if key in variables:
        return variables[key]
    raise Unsupported(f"refers to {text}")


def project_variables(path):
    """Variables of a with'ed abstract project, by qualified name."""
    text = strip_comments(path.read_text())
    name = re.search(r"project\s+(\w+)\s+is", text, re.I).group(1).lower()
    found = {}
    for m in re.finditer(r"^\s*(\w+)\s*:=\s*(\(.*?\))\s*;", text, re.S | re.M):
        found[f"{name}.{m.group(1).lower()}"] = string_list(m.group(2))
    return found


def parse(path):
    """The supported subset as a dict; Unsupported names the first misfit."""
    text = strip_comments(path.read_text())
    project = {"packages": {}, "libraries": [], "units": {}}
    variables = {}
    for m in re.finditer(r'^\s*with\s+"([^"]+)"\s*;', text, re.M):
        name = Path(m.group(1)).name
        if name in LIBRARY_NAMES:
            project["libraries"].append(LIBRARY_NAMES[name])
        elif name not in VARIABLE_ONLY:
            raise Unsupported(f"imports {name}")
        variables.update(project_variables((path.parent / m.group(1)).resolve()))
    text = re.sub(r'^\s*with\s+"[^"]+"\s*;', "", text, flags=re.M)
    for word, why in (("case", "has a case statement"),
                      ("extends", "extends another project"), ("external", "reads an external"),
                      ("library", "is a library project")):
        if word == "extends":
            probe = re.sub(r"package\s+Compiler\s+extends\s+Toolkit_Flags\.Compiler", "", text, flags=re.I)
        else:
            probe = text
        if re.search(rf"\b{word}\b", probe, re.I) and not (word == "library" and
                                                          re.search(r'Library_Dir\s+use\s+""', text)
                                                          and not re.search(r"\blibrary\s+project|Library_Name", text, re.I)):
            raise Unsupported(why)
    body = re.sub(r"package\s+(\w+)(?:\s+extends\s+[\w.]+)?\s+is(.*?)end\s+\1\s*;",
                  lambda m: project["packages"].__setitem__(m.group(1).lower(), m.group(2)) or "",
                  text, flags=re.S | re.I)
    if re.search(r"^\s*\w+\s*:=", body, re.M):
        raise Unsupported("declares project-level variables")
    allowed = {"languages", "object_dir", "source_dirs", "main", "runtime", "library_dir"}
    for m in re.finditer(r"for\s+(\w+)\s*(\([^)]*\))?\s+use\s+(.*?);", body, re.S | re.I):
        name = m.group(1).lower()
        if name not in allowed:
            raise Unsupported(f"sets {m.group(1)}")
        project[name] = string_list(m.group(3))
    for package, content in project["packages"].items():
        if package not in {"builder", "compiler", "binder", "linker"}:
            raise Unsupported(f"has package {package}")
        local = dict(variables)
        for m in re.finditer(r"^\s*(\w+)\s*:=\s*(\(.*?\))\s*;", content, re.S | re.M):
            local[m.group(1).lower()] = string_list(m.group(2))
        for m in re.finditer(r"for\s+(\w+)\s*(\([^)]*\))?\s+use\s+(.*?);", content, re.S | re.I):
            attribute, index = m.group(1).lower(), string_list(m.group(2) or "")
            if attribute == "default_switches" and index == ["Ada"]:
                project[f"{package}_switches"] = expression(m.group(3), local)
            elif package == "builder" and attribute == "executable":
                project.setdefault("executables", {})[index[0]] = string_list(m.group(3))[0]
            elif package == "compiler" and attribute == "switches" and len(index) == 1:
                project["units"][index[0]] = expression(m.group(3), local)
            else:
                raise Unsupported(f"sets {package}.{m.group(1)}")
    if project.get("languages") != ["Ada"]:
        raise Unsupported(f"languages {project.get('languages')}")
    if project.get("object_dir") != [OBJECTS]:
        raise Unsupported(f"object directory {project.get('object_dir')}")
    if len(project.get("main", [])) != 1:
        raise Unsupported(f"{len(project.get('main', []))} mains")
    if resolve(path.parent, project.get("runtime", [""])[0]) != RUNTIME:
        raise Unsupported("does not use the user runtime")
    return project


def resolve(directory, name):
    """A project-relative name as a path from the tree's root."""
    full = (directory / name).resolve()
    try:
        return str(full.relative_to(ROOT))
    except ValueError:
        raise Unsupported(f"{name} is outside the tree")


def ccl_string(text):
    if '"' in text or "\\" in text:
        raise Unsupported(f"string {text!r} needs escaping")
    return f'"{text}"'


def ccl_strings(values):
    return "[" + " ".join(ccl_string(v) for v in values) + "]"


# What build-params.ccl and build-catalog.ccl hold, which a translated
# component must not repeat.
PROFILE_BINDER = ["-d_C", "-nostdlib"]
PROFILE_LINK = ['(Link_Argument.Text "-nostdlib")', '(Link_Argument.Text "-static")',
                '(Link_Argument.Text "-no-pie")', '(Link_Argument.Text "-Wl,-z,stack-size=16777216")',
                '(Link_Argument.Text "-lgnat-user")', '(Link_Argument.Text "-T")',
                '(Link_Argument.File "userspace/c/link.ld")', '(Link_Argument.File "userspace/c/build/crt0.o")',
                "Link_Argument.Manifest_Object"]
TOOLKIT_SOURCES = ["userspace/lib/ui", "userspace/lib/theme", "userspace/lib/compositor",
                   "userspace/lib/display", "userspace/allocator/src"]
DEBUG_SWITCH = "-g"
# Switches that change no object code (warnings, stack-usage reports) or
# that build-params.ccl's target list supplies.
NEUTRAL = {"-gnatwa", "-gnatw.X", "-fstack-usage", "-mno-red-zone", "-fno-pic"}
# build-catalog.ccl's code profiles, by the facets that change object code.
PROFILES = {"proved": dict(language=True, maximum=True, suppressed=True, kept=False, no_vrp=True),
            "pixel-loop": dict(language=True, maximum=True, suppressed=False, kept=False, no_vrp=True)}


def facets(switches):
    """The intent behind a switch list; Unsupported for anything else."""
    known = NEUTRAL | {DEBUG_SWITCH, "-O2", "-O3", "-gnat2022", "-gnatp", "-fomit-frame-pointer", "-fno-tree-vrp"}
    if set(switches) - known:
        raise Unsupported(f"switches {sorted(set(switches) - known)} have no intent in the schema")
    return dict(language="-gnat2022" in switches, maximum="-O3" in switches, suppressed="-gnatp" in switches,
                kept="-fomit-frame-pointer" not in switches, no_vrp="-fno-tree-vrp" in switches)


def code_fields(switches):
    f = facets(switches)
    return ([s for flag, s in ((f["language"], "language => Ada_Language.Ada_2022"),
                                (f["maximum"], "optimization => Optimization.Maximum"),
                                (f["suppressed"], "checks => Checks.Suppressed"),
                                (f["kept"], "frame_pointer => Frame_Pointer.Kept"),
                                (f["no_vrp"], "value_range_propagation => false")) if flag])


def profile_named(switches, debug):
    if (DEBUG_SWITCH in switches) != debug:
        raise Unsupported("a unit's debug information differs from its program's")
    f = facets(switches)
    for name, wanted in PROFILES.items():
        if f == wanted:
            return name
    raise Unsupported(f"no code profile in build-catalog.ccl matches {switches}")


def catalog(path):
    """Native_Runtime, Driver or No_Manifest: the manifest macro the make
    target building this project calls for its directory."""
    gpr = "../" + str(path.relative_to(ROOT))
    directory = "../" + str(path.parent.relative_to(ROOT))
    lines = MAKEFILE.read_text().splitlines()
    for i, line in enumerate(lines):
        if gpr in line:
            first = i
            while first > 0 and lines[first - 1].startswith("\t"):
                first -= 1
            recipe = lines[first:i]
            for r in recipe:
                if "generate_" in r and directory in r:
                    raise Unsupported("make generates sources for it (" + r.strip().split(",")[0] + ")")
            if any(f"compile_driver_manifest,{directory})" in r for r in recipe):
                return "Driver"
            if any(f"compile_ccl_manifest,{directory})" in r for r in recipe):
                return "Native_Runtime"
            return "No_Manifest"
    raise Unsupported("no make target builds it")


def convert(path):
    project = parse(path)
    directory = str(path.parent.relative_to(ROOT))
    main = project["main"][0]
    executable = project.get("executables", {}).get(main)
    if not executable or Path(executable).suffix not in SUFFIXES:
        raise Unsupported(f"executable {executable!r}")
    builder = project.get("builder_switches", [])
    builder_compile = [s for s in builder if s not in BUILDER_ONLY]
    compiler = builder_compile + project.get("compiler_switches", [])
    # Units with their own switches, grouped by identical lists, in order.
    groups = []
    for unit, switches in project["units"].items():
        full = builder_compile + switches
        for group in groups:
            if group[1] == full:
                group[0].append(unit)
                break
        else:
            groups.append(([unit], full))
    binder = project.get("binder_switches", []) + (["-nostdlib"] if "-nostdlib" in builder else [])
    if binder != PROFILE_BINDER:
        raise Unsupported(f"binder switches {binder} differ from build-params.ccl's")
    sources = []
    for name in project.get("source_dirs", []):
        if name in (".", GENERATED):
            continue
        source = resolve(path.parent, name)
        if "/build/" in source + "/":
            raise Unsupported(f"reads make-generated sources from {source}")
        sources.append(source)
    link = []
    for s in project.get("linker_switches", []):
        if s == MANIFEST_OBJECT:
            link.append("Link_Argument.Manifest_Object")
        elif "/" in s:
            link.append(f"(Link_Argument.File {ccl_string(resolve(path.parent, s))})")
        else:
            link.append(f"(Link_Argument.Text {ccl_string(s)})")
    launcher = {"Native_Runtime": "Process_Manager", "Driver": "Device_Manager",
                "No_Manifest": "Kernel"}[catalog(path)]
    suffix = Path(executable).suffix
    kind = {".app": "Kind.Application", ".drv": "Kind.Driver", ".svc": f"(Kind.Service Launcher.{launcher})"}[suffix]
    if (suffix == ".app" and launcher != "Process_Manager") or (suffix == ".drv" and launcher != "Device_Manager"):
        raise Unsupported(f"{executable} is checked against the {catalog(path)} catalog")
    base_link = [a for a in PROFILE_LINK if not (launcher == "Kernel" and a == "Link_Argument.Manifest_Object")]
    if link[:len(base_link)] != base_link:
        raise Unsupported("link switches differ from build-params.ccl's")
    debug = DEBUG_SWITCH in compiler
    fields = []
    if main != CONVENTIONAL_MAIN:
        fields.append(f"main => (Main.File {ccl_string(main)})")
    libraries = []
    if project["libraries"]:
        if sorted(project["libraries"]) != ["fonts-native", "toolkit-flags"]:
            raise Unsupported(f"libraries {project['libraries']}")
        libraries = ["ui-toolkit"]
        sources = [d for d in sources if d not in TOOLKIT_SOURCES]
    if sources:
        fields.append(f"sources => {ccl_strings(sources)}")
    if libraries:
        fields.append(f"libraries => {ccl_strings(libraries)}")
    fields += code_fields(compiler)
    if not debug:
        fields.append("debug_info => false")
    if groups:
        units = []
        for files, switches in groups:
            units.append(f"(Unit_Code {ccl_strings(files)} {ccl_string(profile_named(switches, debug))})")
        fields.append("units => [" + " ".join(units) + "]")
    if link[len(base_link):]:
        fields.append("link_extra => [" + " ".join(link[len(base_link):]) + "]")
    name = Path(executable).stem
    return (f'(Component name => {ccl_string(name)} kind => {kind}\n'
            f'  source => (Source.Ada (Ada_Source' + "".join("\n    " + f for f in fields) + ")))\n")


def projects():
    for path in sorted(ROOT.rglob("*.gpr")):
        parts = path.relative_to(ROOT).parts
        if "build" in parts or parts[0] in {".build-workspaces", "tests"} or "tests" in parts:
            continue
        yield path


def main(argv):
    if argv[:1] == ["survey"]:
        fits = 0
        for path in projects():
            try:
                convert(path)
                fits += 1
                print(f"fits      {path.relative_to(ROOT)}")
            except Unsupported as why:
                print(f"no        {path.relative_to(ROOT)}: {why}")
        print(f"{fits} projects fit the subset")
        return 0
    if argv[:1] == ["convert"] and len(argv) in (2, 3):
        path = Path(argv[1]).resolve()
        text = convert(path)
        if argv[2:] == ["--write"]:
            (path.parent / "component.ccl").write_text(text)
        else:
            sys.stdout.write(text)
        return 0
    sys.stderr.write(__doc__)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
