"""Compare bounded SPARK shadow coverage with the actual Desktop pixel loop.

Run in Nix; outputs are private. No GPU or native execution is claimed.
"""
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
assert os.environ.get("IN_NIX_SHELL"), "Use Nix"
main = (ROOT / "userspace/services/desktop/main.adb").read_text()
shadow = re.search(r"   procedure drawDappledShadow .*?   end drawDappledShadow;", main, re.S)
depth = re.search(r"   DROP_SHADOW_DEPTH : constant Positive := \d+;", main)
assert shadow and depth
work = Path(tempfile.mkdtemp(prefix="shadow-geometry-", dir=ROOT / "tests/compositor/build"))
print(work, flush=True)
inputs = {}
for relative in ("userspace/lib/compositor/compositor_shadow.ads", "userspace/lib/compositor/compositor_shadow.adb",
                 "userspace/lib/display/cubit-display_geometry.ads", "userspace/lib/display/cubit-display_geometry.adb",
                 "userspace/runtime/gnat/cubit.ads"):
    path = ROOT / relative
    data = path.read_bytes()
    (work / path.name).write_bytes(data)
    inputs[relative] = hashlib.sha256(data).hexdigest()
inputs["main.adb"] = hashlib.sha256(main.encode()).hexdigest()
program = '''with Compositor_Shadow; with Ada.Text_IO;
procedure Shadow_Tests is
   package S renames Compositor_Shadow; package G renames S.G;
   use type G.Pixel_Edge, G.Logical_Coordinate;
   type Pixels is array (0 .. 23, 0 .. 31) of Boolean;
   Reference : Pixels;
   Screen : G.Output := (32, 24, G.Unrotated, (1, 1), 0, 0);
   C_BLACK : constant Natural := 0;
   Checks : Natural := 0;
   procedure putPixel (X, Y, Color : Natural) is
      pragma Unreferenced (Color);
      R : constant G.Physical_Rectangle := G.Damage (Screen,
        (G.Logical_Coordinate (X), G.Logical_Coordinate (Y),
         G.Logical_Coordinate (X + 1), G.Logical_Coordinate (Y + 1)));
   begin
      for PY in Integer (R.Top) .. Integer (R.Bottom) - 1 loop
         for PX in Integer (R.Left) .. Integer (R.Right) - 1 loop
            Reference (PY, PX) := True;
         end loop;
      end loop;
   end putPixel;
@DEPTH@
@SHADOW@
begin
   for Rotation in G.Orientation loop
      for N in G.Scale_Component loop
         for D in G.Scale_Component loop
            for Origin in -1 .. 1 loop
               for Width in 1 .. 8 loop
                  Screen := (32, 24, Rotation, (N, D),
                    G.Output_Origin (Origin * 5), G.Output_Origin (-Origin * 5));
                  Reference := (others => (others => False));
                  drawDappledShadow (3, 3, Width, 7);
                  declare
                     P : constant S.Plan := S.Build ((3, 3, 3 + G.Logical_Coordinate (Width), 10), DROP_SHADOW_DEPTH);
                  begin
                     pragma Assert (P.Valid);
                     for Y in Reference'Range (1) loop
                        for X in Reference'Range (2) loop
                           pragma Assert (Reference (Y, X) =
                             (S.Paints (Screen, P.Areas (1), (G.Pixel_Index (X), G.Pixel_Index (Y))) or
                              S.Paints (Screen, P.Areas (2), (G.Pixel_Index (X), G.Pixel_Index (Y)))));
                           Checks := Checks + 1;
                        end loop;
                     end loop;
                  end;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   declare
      P : S.Plan;
      L : constant G.Logical_Coordinate := G.Logical_Coordinate'Last;
      F : constant G.Logical_Coordinate := G.Logical_Coordinate'First;
   begin
      P := S.Build ((0, 0, L, 10), 3); pragma Assert (not P.Valid);
      P := S.Build ((0, 0, 10, L), 3); pragma Assert (not P.Valid);
      P := S.Build ((F, F, L - 16, L - 16), 16); pragma Assert (P.Valid);
      P := S.Build ((0, 0, 10, 10), 0); pragma Assert (P.Valid);
      pragma Assert (not S.Paints (Screen, P.Areas (1), (0, 0)));
      P := S.Build ((10, 10, 0, 0), 3); pragma Assert (P.Valid);
      pragma Assert (not S.Paints (Screen, P.Areas (1), (0, 0)));
      pragma Assert (not S.Paints (Screen, (0, 0, L, L), (32, 24)));
   end;
   Ada.Text_IO.Put_Line ("PASS bounded shadow: actual Desktop pixel loop, all scales/rotations/signed output origins, pixel checks=" & Checks'Image);
end Shadow_Tests;
'''.replace("@SHADOW@", shadow[0]).replace("@DEPTH@", depth[0])
(work / "shadow_tests.adb").write_text(program)
(work / "shadow.gpr").write_text('''project Shadow is
   for Source_Dirs use (".");
   for Object_Dir use "obj";
   for Exec_Dir use ".";
   for Main use ("shadow_tests.adb");
   package Compiler is
      for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
   end Compiler;
end Shadow;
''')
inputs["generated_test"] = hashlib.sha256(program.encode()).hexdigest()
(work / "inputs.json").write_text(json.dumps(inputs, indent=2) + "\n")
for command in (["gprbuild", "-q", "-p", "-P", str(work / "shadow.gpr")],
                ["gnatprove", "-P", str(work / "shadow.gpr"), "-u", "compositor_shadow.adb", "--level=2", "--timeout=30", "-j2"]):
    subprocess.run(["alr", "exec", "--", *command], cwd=ROOT / "kernel", check=True)
report = (work / "obj/gnatprove/gnatprove.out").read_text()
total = next(line for line in report.splitlines() if line.startswith("Total "))
assert total.split()[-2:] == [".", "."], total
result = subprocess.run([str(work / "shadow_tests")], text=True, capture_output=True)
(work / "tests.log").write_text(result.stdout + result.stderr)
print(result.stdout + result.stderr, end="", flush=True)
result.check_returncode()
for relative, expected in inputs.items():
    if relative.startswith("userspace/"):
        assert hashlib.sha256((ROOT / relative).read_bytes()).hexdigest() == expected, relative
assert hashlib.sha256((ROOT / "userspace/services/desktop/main.adb").read_bytes()).hexdigest() == inputs["main.adb"]
(work / "result.json").write_text(json.dumps({"status": "PASS", "proof": total.strip(),
    "scope": "hosted shadow geometry and actual Desktop traversal oracle; no Vulkan execution"}, indent=2) + "\n")
