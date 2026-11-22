#!/usr/bin/env python3
"""Compile actual native allocation routing with modeled supervisor progress."""
from pathlib import Path
import hashlib
import json
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = root / "userspace/services/intel-gpu/main.adb"
original = source.read_bytes()
text = original.decode()
start = text.index("      if Update_Pending = 0 then return; end if;", text.index("   procedure Finish_VM_Update"))
end = text.index("      if Application_State.Has_Update", start)
dispatch = text[start:end]
start = text.index("         if not Update_Image_Pending and then", text.index("   procedure Finish_VM_Update"))
end = text.index("         Advance_In_Place;", start)
loop = text[start:end]

prefix = r'''with Ada.Text_IO;
procedure Allocation_Routing is
   Application_Pending, Private_Pending, Update_Pending,
     Buffer_Retirement_Pending : Natural := 0;
   Update_Image_Pending, Recycle_In_Progress : Boolean := False;
   type Offline_Phase is (No_Offline_Bind, Register_Offline);
   Offline_Bind_State : Offline_Phase := No_Offline_Bind;
   type Directory_Phase is (No_Directory_Update, Register_Directories);
   Directory_Update : Directory_Phase := No_Directory_Update;
   Buffer_Pool : Natural := 0;
   Waiting, Complete_On_Tick, Recycle_Done : Boolean := False;
   Ticks, Recycles, Offline_Calls, Directory_Calls, Replacement_Calls,
     Private_Calls, App_Calls, Retire_Calls : Natural := 0;
   package Buffer_Memory is
      procedure Tick (Pool : Natural);
      function Pending (Pool : Natural) return Boolean;
      function Result (Pool : Natural) return Natural;
   end Buffer_Memory;
   package body Buffer_Memory is
      procedure Tick (Pool : Natural) is
      begin Ticks := Ticks + 1;
         if Complete_On_Tick then Waiting := False; end if;
      end Tick;
      function Pending (Pool : Natural) return Boolean is (Waiting);
      function Result (Pool : Natural) return Natural is
      begin pragma Assert (not Waiting); return 123; end Result;
   end Buffer_Memory;
   procedure Advance_Table_Recycling is
   begin Recycles := Recycles + 1;
      if Recycle_Done then Recycle_In_Progress := False; end if;
   end Advance_Table_Recycling;
   procedure Finish_Offline_Bind (Backing : Natural) is
   begin pragma Assert (Backing = 123); Offline_Calls := Offline_Calls + 1; end;
   procedure Finish_Directory_Update (Backing : Natural) is
   begin pragma Assert (Backing = 123); Directory_Calls := Directory_Calls + 1; end;
   procedure Finish_Private_Context (Backing : Natural) is
   begin pragma Assert (Backing = 123); Private_Calls := Private_Calls + 1; end;
   procedure Finish_Application_Buffer (Backing : Natural) is
   begin pragma Assert (Backing = 123); App_Calls := App_Calls + 1; end;
   procedure Finish_Buffer_Retirement is
   begin Retire_Calls := Retire_Calls + 1; end;
   procedure Finish_VM_Update (Backing : Natural) is
   begin
'''
middle = r'''
      Replacement_Calls := Replacement_Calls + 1;
   end Finish_VM_Update;
   procedure Turn is
   begin
'''
suffix = r'''
   end Turn;
   function Calls return Natural is
     (Offline_Calls + Directory_Calls + Replacement_Calls + Private_Calls + App_Calls + Retire_Calls);
begin
   Turn; pragma Assert (Ticks = 0 and Recycles = 0 and Calls = 0);
   Update_Pending := 7; Offline_Bind_State := Register_Offline; Waiting := True;
   Turn; pragma Assert (Ticks = 1 and Recycles = 1 and Calls = 0);
   Update_Image_Pending := True; Complete_On_Tick := True;
   Turn; pragma Assert (Ticks = 1 and Waiting and Calls = 0);
   Update_Image_Pending := False;
   Turn; pragma Assert (Ticks = 2 and not Waiting and Offline_Calls = 1 and Calls = 1);
   -- A completed allocation remains available across multiple bounded append turns.
   Turn; pragma Assert (Offline_Calls = 2 and Calls = 2);
   Recycle_In_Progress := True;
   Turn; pragma Assert (Offline_Calls = 2 and Calls = 2);
   Recycle_Done := True;
   Turn; pragma Assert (Offline_Calls = 3 and Calls = 3);
   -- Even a stale live-directory state must not steal an offline continuation.
   Directory_Update := Register_Directories;
   Turn; pragma Assert (Offline_Calls = 4 and Directory_Calls = 0 and Calls = 4);
   Offline_Bind_State := No_Offline_Bind;
   Turn; pragma Assert (Directory_Calls = 1 and Calls = 5);
   Directory_Update := No_Directory_Update;
   Turn; pragma Assert (Replacement_Calls = 1 and Calls = 6);
   Buffer_Retirement_Pending := 9; Private_Pending := 8; Application_Pending := 6;
   Turn; pragma Assert (Retire_Calls = 1 and Calls = 7);
   Buffer_Retirement_Pending := 0; Update_Pending := 0;
   Turn; pragma Assert (Private_Calls = 1 and Calls = 8);
   Private_Pending := 0;
   Turn; pragma Assert (App_Calls = 1 and Calls = 9);
   Application_Pending := 0;
   Turn; pragma Assert (Calls = 9);
   -- A stale phase is not authority without a pending ticket.
   Offline_Bind_State := Register_Offline;
   Finish_VM_Update (123); pragma Assert (Calls = 9);
   Ada.Text_IO.Put_Line ("Allocation routing PASS15: pending, multi-turn offline, recycling, priority, stale ticket");
end Allocation_Routing;
'''
variants = {
    "native": (dispatch, loop),
    "omit-pending-check": (dispatch, loop.replace("not Buffer_Memory.Pending (Buffer_Pool) and then ", "")),
    "omit-offline-route": (dispatch.replace("         Finish_Offline_Bind (Backing); return;", "         null;"), loop),
    "omit-ticket-check": (dispatch.replace("      if Update_Pending = 0 then return; end if;", ""), loop),
}
out = Path(tempfile.mkdtemp(prefix="cubit-allocation-routing."))
(out / "test.gpr").write_text('''project Test is
for Source_Dirs use (".");
for Object_Dir use "obj";
for Main use ("allocation_routing.adb");
package Compiler is
for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato");
end Compiler;
end Test;
''')
for name, (route, event) in variants.items():
    if name != "native":
        assert (route, event) != (dispatch, loop), name
    (out / "allocation_routing.adb").write_text(prefix + route + middle + event + suffix)
    build = subprocess.run(["gprbuild", "-f", "-p", "-P", str(out / "test.gpr")], capture_output=True, text=True)
    (out / f"{name}-build.log").write_text(build.stdout + build.stderr)
    assert build.returncode == 0, (name, build.stderr, out)
    run = subprocess.run([str(out / "obj/allocation_routing")], capture_output=True, text=True)
    (out / f"{name}-run.log").write_text(run.stdout + run.stderr)
    assert (run.returncode == 0) == (name == "native"), (name, run.stdout, run.stderr, out)
    print(name, run.returncode, run.stdout.strip())
assert source.read_bytes() == original
(out / "evidence.json").write_text(json.dumps({"source_sha256": hashlib.sha256(original).hexdigest(), "variants": list(variants)}, indent=2))
print("Evidence:", out)
