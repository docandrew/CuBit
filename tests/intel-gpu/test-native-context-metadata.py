#!/usr/bin/env python3
"""Extract native context metadata dispatch/admission; hosted, not GPU validation."""
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = (root / "userspace/services/intel-gpu/main.adb").read_text()
start = source.index("   type Context_Metadata_Table is")
body = source[start:source.index("   package Context_Metadata_Growth is", start)]
prefix = """
with Interfaces; use Interfaces;
procedure Context_Metadata_Test is
   package Application_State is
      Table_Pages : constant := 64;
   end Application_State;
   Owned : Boolean := True;
   function Context_Metadata_Owner return Boolean is (Owned);
   function Context_Metadata_Index return Natural is (1);
   Private_Contexts : array (1 .. 1) of Boolean := [False];
   Caps : array (1 .. 4) of Positive := [64, 64, 64, 64];
   Seen : Natural := 0;
   function Context_Reference_Capacity return Positive is (Caps (1));
   function Context_Descriptor_Capacity return Positive is (Caps (2));
   function Context_Mirror_Capacity return Positive is (Caps (3));
   function Ledger_Capacity return Positive is (Caps (4));
   procedure Record_Extend (Id : Positive; Base, Bytes : Unsigned_64;
                            Accepted : out Boolean) is
   begin
      pragma Assert (Base = 16#100000# and Bytes = 8192);
      Seen := Id;
      Accepted := Owned;
   end Record_Extend;
   procedure Extend_Context_References (Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin Record_Extend (1, Base, Bytes, Accepted); end;
   procedure Extend_Context_Descriptors (Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin Record_Extend (2, Base, Bytes, Accepted); end;
   procedure Extend_Context_Mirror (Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin Record_Extend (3, Base, Bytes, Accepted); end;
   procedure Extend_Ledger (Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin Record_Extend (4, Base, Bytes, Accepted); end;
"""
suffix = """
   OK : Boolean;
begin
   Context_Metadata_Targets (1) := [others => 64];
   -- Each independent capacity is required; no accidental min/other-store dispatch.
   for T in Context_Metadata_Table loop
      Caps := [11, 22, 33, 44];
      pragma Assert (Context_Metadata_Capacity (T) = 11 * (Context_Metadata_Table'Pos (T) + 1));
      Extend_Context_Metadata (T, 16#100000#, 8192, OK);
      pragma Assert (OK and Seen = Context_Metadata_Table'Pos (T) + 1);
   end loop;
   for Mask in Unsigned_64 range 0 .. 15 loop
      for I in Caps'Range loop
         Caps (I) := (if (Mask and 2 ** (I - 1)) /= 0 then 63 else 64);
      end loop;
      Admit_Context_Metadata (64, OK);
      pragma Assert (OK = (Mask = 0));
   end loop;
   Caps := [others => 128];
   Admit_Context_Metadata (65, OK); pragma Assert (not OK);
   Admit_Context_Metadata (64, OK); pragma Assert (OK);
   Context_Metadata_Targets (1) (Provenance) := 129;
   Admit_Context_Metadata (64, OK); pragma Assert (not OK);
   Context_Metadata_Targets (1) (Provenance) := 64;
   Owned := False;
   Admit_Context_Metadata (64, OK); pragma Assert (not OK);
   for T in Context_Metadata_Table loop
      Extend_Context_Metadata (T, 16#100000#, 8192, OK);
      pragma Assert (not OK);
   end loop;
end Context_Metadata_Test;
"""
variants = {
    "actual": body,
    "missing-owner": body.replace("Context_Metadata_Owner and then", "True and then"),
    "missing-capacity": body.replace("Context_Metadata_Capacity (T) >=\n          Context_Metadata_Targets (Context_Metadata_Index) (T)", "True"),
    "wrong-mirror": body.replace("when Mirrors => Context_Mirror_Capacity", "when Mirrors => Context_Descriptor_Capacity"),
}
out = Path(tempfile.mkdtemp(prefix="cubit-native-context-metadata."))
for name, code in variants.items():
    assert name == "actual" or code != body
    case = out / name
    case.mkdir()
    (case / "context_metadata_test.adb").write_text(prefix + code + suffix)
    with (case / "build.log").open("w") as log:
        subprocess.run(["gnatmake", "-q", "-gnat2022", "-gnata", "-gnato",
                        "context_metadata_test.adb"], cwd=case, stdout=log, stderr=log, check=True)
    with (case / "run.log").open("w") as log:
        run = subprocess.run([str(case / "context_metadata_test")], stdout=log, stderr=log)
    assert (run.returncode == 0) == (name == "actual"), (name, out)
print("Native metadata dispatch/admission PASS, three negative controls:", out)
