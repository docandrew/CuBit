"""Measure naive GPU lowering of the actual Desktop shadow traversal.

Run in Nix. Uses real SPARK scene/drawing code and hosted device mocks; this
is a capture-capacity diagnostic, not native execution or GPU performance.
The extracted loop is intentionally unchanged. It establishes why the live
facade must use a bounded shadow primitive instead of one GPU fill per pixel.
"""
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
assert os.environ.get("IN_NIX_SHELL"), "Run inside the Nix development shell"
main_path = ROOT / "userspace/services/desktop/main.adb"
main = main_path.read_text()
match = re.search(r"   procedure drawDappledShadow .*?   end drawDappledShadow;", main, re.S)
assert match, "production shadow traversal missing"
depth = re.search(r"   DROP_SHADOW_DEPTH : constant Positive := \d+;", main)
assert depth, "production shadow depth missing"
out = ROOT / "tests/compositor/build"
out.mkdir(exist_ok=True)
work = Path(tempfile.mkdtemp(prefix="shadow-capture-", dir=out))
print(work, flush=True)
unit = "desktop_gpu_scene-drawing-shadow_probe.adb"
source = '''with Ada.Text_IO; with Interfaces; with Vulkan_Device_Mock;
procedure Desktop_GPU_Scene.Drawing.Shadow_Probe is
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32) with Import, Convention => C, External_Name => "image_mock";
   procedure Pipeline_Set (Create, Close : U32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   S : State; OK : Boolean; Result : Outcome;
   Calls, First_Reject : Natural := 0;
   C_BLACK : constant U32 := 0;
   Screen : constant V.A.G.Output := (1920, 1080, V.A.G.Unrotated, (1, 1), 0, 0);
   procedure putPixel (X, Y : Natural; Color : U32) is
   begin
      Calls := Calls + 1;
      Logical_Fill (S, (V.A.G.Logical_Coordinate (X), V.A.G.Logical_Coordinate (Y),
        V.A.G.Logical_Coordinate (X + 1), V.A.G.Logical_Coordinate (Y + 1)),
        (0, 0, 1920, 1080), Color, OK);
      if not OK and First_Reject = 0 then First_Reject := Calls; end if;
   end putPixel;
@DEPTH@
@SHADOW@
   procedure Check (W, H : Natural; Fits : Boolean) is
   begin
      Calls := 0; First_Reject := 0;
      Begin_Frame (S, Screen, 0, OK); pragma Assert (OK);
      drawDappledShadow (100, 100, W, H);
      pragma Assert (Calls > 0 and ((First_Reject = 0) = Fits));
      Ada.Text_IO.Put_Line ("SHADOW width=" & W'Image & " height=" & H'Image &
        " pixel_calls=" & Calls'Image & " layers=" & Layer_Count (S)'Image &
        " first_rejected_pixel=" & First_Reject'Image);
      if Fits then
         Discard (S, Result); pragma Assert (Result = Retry);
      else
         Finish (S, Result); pragma Assert (Result = Rejected);
      end if;
      pragma Assert (Current (S) = Idle and Layer_Count (S) = 0 and
        Reader_Count (S) = 0 and Image_Reader_Count (S) = 0);
   end Check;
begin
   Reset; Context_Set (0, 0); Pipeline_Set (0, 0);
   Vulkan_Device_Mock.Set (True, True, 0); Image_Set (8388608, 1, 0, 0, 0);
   D.Initialize (25); D.Configure_Targets (1920, 1080, 1, 33554432, OK); pragma Assert (OK);
   D.Prepare_Pipeline (OK); pragma Assert (OK);
   Check (64, 64, True);
   Check (320, 200, False);
   Check (640, 480, False);
   Check (800, 600, False);
   Close (S, OK); pragma Assert (OK); D.Stop;
   pragma Assert (D.Charged_Bytes = 0);
   Ada.Text_IO.Put_Line ("PASS production shadow traversal: naive pixel lowering exceeds GPU scene capacity; rejection retires the complete scene");
end Desktop_GPU_Scene.Drawing.Shadow_Probe;
'''.replace("@DEPTH@", depth[0]).replace("@SHADOW@", match[0])
(work / unit).write_text(source)
# Only the hosted metadata boundary changes dimensions; no real allocation or
# GPU execution is claimed. Retain the original mock and its adapted hash.
mock_path = ROOT / "tests/compositor/vulkan_device_targets_metadata_mock.c"
mock_original = mock_path.read_text()
assert mock_original.count("width==32 && (height==24 || height==0)") == 1
mock = mock_original.replace("width==32 && (height==24 || height==0)",
                             "width==1920 && (height==1080 || height==0)")
(work / mock_path.name).write_text(mock)
(work / "probe.gpr").write_text(f'''project Probe extends "{ROOT / 'tests/compositor/desktop_gpu_drawing.gpr'}" is
   for Source_Dirs use (".");
   for Source_Files use ("{unit}", "{mock_path.name}");
   for Main use ("{unit}");
   for Object_Dir use "obj";
   for Exec_Dir use ".";
end Probe;
''')
inputs = {str(main_path): hashlib.sha256(main_path.read_bytes()).hexdigest(),
          str(mock_path): hashlib.sha256(mock_original.encode()).hexdigest(),
          "adapted_metadata_mock": hashlib.sha256(mock.encode()).hexdigest(),
          "extracted_shadow": hashlib.sha256(match[0].encode()).hexdigest(),
          "generated_probe": hashlib.sha256(source.encode()).hexdigest()}
(work / "inputs.json").write_text(json.dumps(inputs, indent=2) + "\n")
subprocess.run(["alr", "exec", "--", "gprbuild", "-q", "-p", "-P", str(work / "probe.gpr")],
               cwd=ROOT / "kernel", check=True)
assert hashlib.sha256(main_path.read_bytes()).hexdigest() == inputs[str(main_path)]
result = subprocess.run([str(work / unit.removesuffix(".adb"))], text=True, capture_output=True)
(work / "probe.log").write_text(result.stdout + result.stderr)
print(result.stdout + result.stderr, end="", flush=True)
result.check_returncode()
