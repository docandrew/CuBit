#!/usr/bin/env python3
"""Check the actual grouped teardown alias scans, including the current-root exception."""
from pathlib import Path
import hashlib
import json
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = root / "userspace/services/intel-gpu/main.adb"
original = source.read_bytes()
text = original.decode()
start = text.index("      for I in Private_Contexts'Range loop", text.index("   procedure Poll_Closed_Table_Retirement is"))
end = text.index("      Image_Retirement_Index := Index;", start)
scan = text[start:end]
prefix = '''with Ada.Text_IO;
with Interfaces; use Interfaces;
procedure Grouped_Scan_Test is
   type Image is record
      Retired, Sealed, Disjoint : Boolean;
      Root, Epoch : Unsigned_64;
   end record;
   type Context is record Attempted : Boolean; Source : Image; end record;
   Private_Contexts : array (1 .. 2) of Context;
   Index, Slot : Positive := 1;
   Current_Table_Ticket : array (1 .. 2) of Unsigned_64;
   type Receipt is record Ticket, Root : Unsigned_64; end record;
   Saved : constant Receipt := (55, 4096);
   Source_Revision : Unsigned_64;
   package Live_Snapshots is
      function Retired (Object : Image) return Boolean is (Object.Retired);
   end Live_Snapshots;
   package Application_VM is
      function Sealed (Object : Image) return Boolean is (Object.Sealed);
      function Root_DMA (Object : Image) return Unsigned_64 is (Object.Root);
      function Revision (Object : Image) return Unsigned_64 is (Object.Epoch);
   end Application_VM;
   function Disjoint (Object : Image) return Boolean is (Object.Disjoint);
   Replacement_Tables : Boolean := True;
   package Replacement_Records is
      function Capacity (Object : Boolean) return Positive is (2);
   end Replacement_Records;
   type Backing is record Ready : Boolean; end record;
   type Update_Record is record Tables : Backing; Candidate : Image; end record;
   Update : Update_Record;
   Has_Update : Boolean;
   package Application_State is
      function Has_Update (Other : Positive) return Boolean;
      function Updates (Other : Positive) return Update_Record;
   end Application_State;
   package body Application_State is
      function Has_Update (Other : Positive) return Boolean is (Grouped_Scan_Test.Has_Update);
      function Updates (Other : Positive) return Update_Record is (Update);
   end Application_State;
   Allowed, Expected : Boolean;
   Expected_Revision : Unsigned_64;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      if not Condition then raise Program_Error with "GROUPED_SCAN_ORACLE"; end if;
   end Check;
   procedure Scan is
   begin
'''
suffix = '''
      Allowed := True;
   end Scan;
   function Flag (Bits, Position : Natural) return Boolean is
     ((Bits / 2 ** Position) mod 2 = 1);
begin
   for Owner_Index in 1 .. 2 loop
      Index := Owner_Index;
      for Retiring_Slot in 1 .. 2 loop
         Slot := Retiring_Slot;
         for Bits in 0 .. 32767 loop
            Expected := True; Expected_Revision := 0;
            Current_Table_Ticket := [others => (if Flag (Bits, 10) then 55 else 66)];
            for I in Private_Contexts'Range loop
               Private_Contexts (I) :=
                 (Flag (Bits, (I - 1) * 5),
                  (Flag (Bits, (I - 1) * 5 + 1), Flag (Bits, (I - 1) * 5 + 2),
                   Flag (Bits, (I - 1) * 5 + 3),
                   (if Flag (Bits, (I - 1) * 5 + 4) then 4096 else 8192),
                   Unsigned_64 (I + 7)));
               if I = Owner_Index and Flag (Bits, 10) then
                  Expected := Expected and Private_Contexts (I).Source.Sealed and
                    Private_Contexts (I).Source.Root = 4096;
                  Expected_Revision := Unsigned_64 (I + 7);
               elsif Private_Contexts (I).Attempted and not Private_Contexts (I).Source.Retired then
                  Expected := Expected and Private_Contexts (I).Source.Sealed and
                    Private_Contexts (I).Source.Disjoint;
               end if;
            end loop;
            Has_Update := Flag (Bits, 11);
            Update := ((Ready => Flag (Bits, 12)), (False, Flag (Bits, 13), Flag (Bits, 14), 0, 0));
            -- There is exactly one OTHER slot for either chosen retiring slot.
            if Has_Update and Update.Tables.Ready then
               Expected := Expected and Update.Candidate.Sealed and Update.Candidate.Disjoint;
            end if;
            Source_Revision := 0; Allowed := False;
            Scan;
            Check (Allowed = Expected);
            if Allowed then Check (Source_Revision = Expected_Revision); end if;
            Checks := Checks + 1;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Grouped alias scan PASS" & Checks'Image);
end Grouped_Scan_Test;
'''
work = Path(tempfile.mkdtemp(prefix="grouped-alias-scan.", dir=root / "tests/intel-gpu/build"))
(work / "test.gpr").write_text('''project Test is
 for Source_Dirs use (".");
 for Main use ("grouped_scan_test.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnatp", "-O2");
 end Compiler;
end Test;
''')
mutations = {
    "actual": scan,
    "foreign_current_exception": scan.replace("I = Index and then ", "", 1),
    "wrong_current_ticket": scan.replace("Current_Table_Ticket (Index) = Saved.Ticket", "True", 1),
    "wrong_root": scan.replace("Application_VM.Root_DMA (Private_Contexts (I).Source) /= Saved.Root", "False", 1),
    "ignore_other_candidate": scan.replace("Other /= Slot and then", "False and then", 1),
}
assert all(value != scan for name, value in mutations.items() if name != "actual")
results = {}
for name, fragment in mutations.items():
    (work / "grouped_scan_test.adb").write_text(prefix + fragment + suffix)
    subprocess.run(["gprbuild", "-f", "-P", "test.gpr"], cwd=work, check=True)
    result = subprocess.run([str(work / "grouped_scan_test")], capture_output=True, text=True)
    results[name] = {"returncode": result.returncode, "stdout": result.stdout, "stderr": result.stderr}
    if name == "actual":
        assert result.returncode == 0 and "PASS 131072" in result.stdout, results[name]
    else:
        assert result.returncode != 0 and "GROUPED_SCAN_ORACLE" in result.stderr, results[name]
assert source.read_bytes() == original
(work / "result.json").write_text(json.dumps({"source_sha256": hashlib.sha256(original).hexdigest(),
    "scan_sha256": hashlib.sha256(scan.encode()).hexdigest(), "results": results}, indent=2) + "\n")
print("Grouped native alias scan PASS131072; four negative controls rejected; evidence", work)
