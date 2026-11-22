#!/usr/bin/env python3
"""Extract startup demand: actual backing size, not maximum VM quota."""
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = (root / "userspace/services/intel-gpu/main.adb").read_text()
start = source.index("   procedure Start_Table_Ledger (Index : Positive; Session : Unsigned_64) is")
body = source[start:source.index("   procedure Grow_Table_Ledger is", start)]
prefix = """
with Interfaces; use Interfaces;
procedure Startup_Metadata_Test is
   Private_Table_Pages : constant := 4;
   package Application_State is Table_Pages : constant := 64; end;
   package Application_Lifetime is type Phase is (Offline, Retired); end;
   use type Application_Lifetime.Phase;
   type Context is record Life : Application_Lifetime.Phase := Application_Lifetime.Offline; end record;
   Private_Contexts : array (1 .. 1) of Context;
   Ledger_Index : Natural := 0;
   Ledger_Session : Unsigned_64 := 0;
   Runtime_Fault : Boolean := False;
   Mode, Calls, Stops : Natural := 0;
   function Table_Ledger_Busy return Boolean is (Ledger_Index /= 0);
   function Table_Ledger_Owner return Boolean is
     (Mode /= 3 and Ledger_Index = 1 and Ledger_Session = 99);
   procedure Publish_Snapshot (Text : String) is begin null; end;
   procedure Stop_Table_Ledger (Reason : String) is
   begin
      Stops := Stops + 1; Private_Contexts (1).Life := Application_Lifetime.Retired;
      Ledger_Index := 0; Ledger_Session := 0;
   end;
   procedure Request_Context_Metadata (Index, Tables, Records : Positive; OK : out Boolean) is
   begin
      pragma Assert (Table_Ledger_Owner and Index = 1 and Tables = 4 and Records = 4);
      Calls := Calls + 1; OK := Mode /= 4;
   end;
"""
suffix = """
begin
   for M in 0 .. 4 loop
      Mode := M; Calls := 0; Stops := 0; Runtime_Fault := False;
      Ledger_Index := (if M = 1 then 1 else 0); Ledger_Session := 0;
      Private_Contexts (1).Life := Application_Lifetime.Offline;
      Start_Table_Ledger ((if M = 2 then 2 else 1), 99);
      pragma Assert (Calls = (if M in 0 | 4 then 1 else 0));
      pragma Assert (Runtime_Fault = (M in 1 | 2));
      pragma Assert (Stops = (if M = 3 then 1 else 0));
      pragma Assert ((Private_Contexts (1).Life = Application_Lifetime.Retired) = (M in 3 | 4));
      if M = 0 then pragma Assert (Ledger_Index = 1 and Ledger_Session = 99);
      elsif M in 3 | 4 then pragma Assert (Ledger_Index = 0 and Ledger_Session = 0); end if;
   end loop;
end Startup_Metadata_Test;
"""
variants = {
    "actual": body,
    "eager-quota": body.replace("Index, Private_Table_Pages, Private_Table_Pages", "Index, Application_State.Table_Pages, Application_State.Table_Pages"),
    "omit-owner": body.replace("if not Table_Ledger_Owner then", "if False then"),
}
out = Path(tempfile.mkdtemp(prefix="cubit-startup-metadata."))
for name, code in variants.items():
    assert name == "actual" or code != body
    case = out / name
    case.mkdir()
    (case / "startup_metadata_test.adb").write_text(prefix + code + suffix)
    with (case / "build.log").open("w") as log:
        subprocess.run(["gnatmake", "-q", "-gnat2022", "-gnata", "-gnato", "startup_metadata_test.adb"],
                       cwd=case, stdout=log, stderr=log, check=True)
    with (case / "run.log").open("w") as log:
        run = subprocess.run([str(case / "startup_metadata_test")], stdout=log, stderr=log)
    assert (run.returncode == 0) == (name == "actual"), (name, out)
print("Native startup demand PASS5 plus two negative controls:", out)
