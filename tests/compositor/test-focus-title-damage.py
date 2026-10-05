"""Exercise actual Desktop focus/raise/restore damage routing; Nix required."""
from pathlib import Path
import hashlib, json, os, subprocess, tempfile
assert os.environ.get("IN_NIX_SHELL")
root = Path(__file__).resolve().parents[2]
source = (root / "userspace/services/desktop/main.adb").read_text()
def routine(name):
    start = source.rindex("   procedure " + name)
    end = source.index("   end " + name + ";", start) + len("   end " + name + ";")
    return source[start:end]
body = "\n".join(routine(n) for n in ("raiseSurface", "damagePreviousFocus", "focusAndRaiseSurface", "restoreSurface"))
out = Path(tempfile.mkdtemp(prefix="focus-title-damage-", dir=root / "tests/compositor/build"))
print(out, flush=True)
prefix = r"""with Interfaces; use Interfaces;
with Ada.Text_IO;
procedure Check is
 type Rect is record x,y,w,h : Natural := 0; end record;
 subtype SurfaceIndex is Natural range 0..7;
 type Surface is record
  id, flags : Unsigned_64 := 0;
  used, minimized, dirty : Boolean := False;
  x,y,w,h : Natural := 0;
 end record;
 surfaces : array(SurfaceIndex) of Surface;
 focusSurface : Unsigned_64 := 0;
 SURFACE_FLAG_WINDOW : constant Unsigned_64 := 1;
 CLIENT_INSET_TOP : constant := 30;
 function findSurface(id:Unsigned_64) return Integer is
 begin
  for I in surfaces'Range loop
   if surfaces(I).used and then surfaces(I).id=id then return I; end if;
  end loop; return -1;
 end;
 function surfaceRect(s:Surface) return Rect is ((s.x,s.y,s.w,s.h));
 function taskButtonRect(i:SurfaceIndex) return Rect is ((10+Natural(i)*60,700,55,24));
 function unionRect(a,b:Rect) return Rect is
  x : constant Natural:=Natural'Min(a.x,b.x);
  y : constant Natural:=Natural'Min(a.y,b.y);
 begin
  if a.w=0 or a.h=0 then return b; end if;
  if b.w=0 or b.h=0 then return a; end if;
  return (x,y,Natural'Max(a.x+a.w,b.x+b.w)-x,Natural'Max(a.y+a.h,b.y+b.h)-y);
 end;
 function inflateRect(r:Rect;n:Natural) return Rect is
  x : constant Natural:=(if r.x<n then 0 else r.x-n);
  y : constant Natural:=(if r.y<n then 0 else r.y-n);
 begin return (x,y,r.x+r.w+n-x,r.y+r.h+n-y); end;
 function Covers(d,r:Rect) return Boolean is
  (d.w>0 and d.h>0 and d.x<=r.x and d.y<=r.y and
   d.x+d.w>=r.x+r.w and d.y+d.h>=r.y+r.h);
"""
suffix = r"""
 D, Old_Title : Rect;
 procedure Setup(New_X,New_Y:Natural) is
 begin
  surfaces := (others=>(others=><>));
  surfaces(0):=(1,1,True,False,False,New_X,New_Y,240,200);
  surfaces(1):=(2,1,True,False,False,100,100,240,200);
  focusSurface:=2; D:=(others=>0); Old_Title:=(100,100,240,30);
 end;
begin
 -- Partially intersecting damage, fully separate windows, and raised reordering.
 for X in 0..2 loop
  Setup(200+X*200,112+X*80);
  focusAndRaiseSurface(0,D);
  pragma Assert(focusSurface=1 and surfaces(1).id=1);
  pragma Assert(Covers(D,Old_Title));
  pragma Assert(Covers(D,(200+X*200,112+X*80,240,30)));
  -- Stable focus avoids repainting.
  D:=(others=>0); focusAndRaiseSurface(1,D);
  pragma Assert(D.w=0 and D.h=0);
 end loop;
 Setup(500,300); surfaces(0).minimized:=True;
 restoreSurface(0,D);
 pragma Assert(Covers(D,Old_Title) and focusSurface=1);
 Setup(500,300); focusSurface:=99; damagePreviousFocus(1,D);
 pragma Assert(D.w=0);
 Setup(500,300); surfaces(1).minimized:=True; damagePreviousFocus(1,D);
 pragma Assert(D.w=0);
 Ada.Text_IO.Put_Line("PASS partial/separate focus, raise, restore, stable/missing/minimized focus");
end Check;
"""
(out / "test.gpr").write_text('project Test is for Main use ("check.adb"); for Object_Dir use "obj"; for Exec_Dir use "."; package Compiler is for Default_Switches("Ada") use ("-gnat2022", "-gnata"); end Compiler; end Test;')
report = {"source_sha256": hashlib.sha256(source.encode()).hexdigest(), "variants": {}}
for name, text in (("fixed", body), ("missing-old-focus-damage", body.replace("      damagePreviousFocus (id, damage);", ""))):
    (out / "check.adb").write_text(prefix + text + suffix)
    with (out / (name + ".log")).open("w") as log:
        subprocess.run(["gprbuild", "-q", "-p", "-P", str(out / "test.gpr")], stdout=log, stderr=subprocess.STDOUT, check=True)
        rc = subprocess.run([str(out / "check")], stdout=log, stderr=subprocess.STDOUT).returncode
    report["variants"][name] = rc
    assert (rc == 0) == (name == "fixed"), name
report["status"] = "PASS"
(out / "result.json").write_text(json.dumps(report, indent=2)+"\n")
print("PASS actual Desktop damage routing; original-behavior negative control rejected")
