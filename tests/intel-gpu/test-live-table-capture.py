#!/usr/bin/env python3
"""Compile native live-table capture with real VM metadata and modeled authority."""
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = root / "userspace/services/intel-gpu/main.adb"
original = source.read_text()
start = original.index("      for P in 1 .. Application_VM.Used", original.index("   procedure Capture_Removal"))
end = original.index("      Removal_Backing := Backing;", start)
body = original[start:end]
prefix = '''with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_Table_Provenance;
with Intel_GPU_ADLN_PPGTT;
procedure Capture_Test is
   package Application_VM is new Intel_GPU_VM_Image (64);
   type Context is limited record Source : Application_VM.Image; end record;
   Private_Contexts : array (1 .. 1) of Context;
   Update_Index : constant := 1;
   Update_Session : constant Unsigned_64 := 42;
   type Capture_State is record Ready : Boolean := True; end record;
   Removal_Capture : Capture_State;
   Ticket : Unsigned_64 := 0;
   Calls, Fault : Natural := 0;
   procedure Capture;
   function Current_Table_Ticket (Index : Positive) return Unsigned_64 is (Ticket);
   package Application_Buffers is
      function Ticket_Slot (ID : Unsigned_64) return Positive is (1);
   end Application_Buffers;
   function Initial_Table_Mapping (Index : Positive; Session : Unsigned_64;
                                   P : Positive) return Intel_GPU_Table_Provenance.Mapping is
   begin
      Calls := Calls + 1;
      if P > Application_VM.Used (Private_Contexts (1).Source) or P = Fault then
         return (others => 0);
      end if;
      return (Ticket => 9, Offset => 0, CPU => Unsigned_64 (P) * 4096,
              DMA => Unsigned_64 (P) * 4096);
   end Initial_Table_Mapping;
   function Replacement_Table_Mapping (Index : Positive; Session : Unsigned_64;
                                       P : Positive) return Intel_GPU_Table_Provenance.Mapping is
     (Initial_Table_Mapping (Index, Session, P));
   function Captured_Table_Mapping (P : Positive)
     return Intel_GPU_Table_Provenance.Mapping is
     (if Ticket = 0 then Initial_Table_Mapping (Update_Index, Update_Session, P)
      else Replacement_Table_Mapping (1, Update_Session, P));
   Accepted : Boolean;
   procedure Capture is
      Table_Pages : Application_VM.Backing_Pages := [others => 0];
      Resolved : Boolean;
   begin
      Accepted := False;
'''
suffix = '''
      Accepted := True;
   end Capture;
   Backing : Application_VM.Backing_Pages;
   OK : Boolean;
begin
   for P in Backing'Range loop Backing (P) := Unsigned_64 (P) * 4096; end loop;
   Application_VM.Initialize (Private_Contexts (1).Source, Backing, OK);
   pragma Assert (OK);
   Application_VM.Map_Page (Private_Contexts (1).Source, 4096, 16#100000#,
     Intel_GPU_ADLN_PPGTT.Write_Back, Intel_GPU_ADLN_PPGTT.Read_Write, OK);
   pragma Assert (OK);
   Application_VM.Seal (Private_Contexts (1).Source, OK); pragma Assert (OK);
   pragma Assert (Application_VM.Used (Private_Contexts (1).Source) = 4);
   for Replacement in Boolean loop
      Ticket := (if Replacement then 9 else 0);
      for Missing in 0 .. 4 loop
         Fault := Missing; Calls := 0;
         Removal_Capture.Ready := True;
         Capture;
         pragma Assert (Accepted = (Missing = 0));
         pragma Assert (Calls = (if Missing = 0 then 4 else Missing));
         pragma Assert (Removal_Capture.Ready = (Missing = 0));
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Sparse native capture PASS10");
end Capture_Test;
'''
work = Path(tempfile.mkdtemp(prefix="cubit-live-table-capture."))
(work / "test.gpr").write_text(f'''project Test is
 for Source_Dirs use (".", "{root}/userspace/services/intel-gpu");
 for Main use ("capture_test.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato");
 end Compiler;
end Test;
''')
mutants = {
    "actual": body,
    "all_reserved": body.replace(
        "1 .. Application_VM.Used (Private_Contexts (Update_Index).Source)",
        "Application_VM.Page_Number", 1),
    "stale_ready": body.replace("Removal_Capture.Ready := False;", "null;", 1),
}
for name, fragment in mutants.items():
    assert name == "actual" or fragment != body
    (work / "capture_test.adb").write_text(prefix + fragment + suffix)
    subprocess.run(["gprbuild", "-f", "-P", "test.gpr"], cwd=work, check=True)
    result = subprocess.run([str(work / "capture_test")], capture_output=True, text=True)
    if name == "actual":
        assert result.returncode == 0, result.stderr
    else:
        assert result.returncode != 0 and "ASSERTION_ERROR" in result.stderr, result
assert source.read_text() == original
print("Sparse native capture PASS10; both negative controls rejected; evidence", work)
