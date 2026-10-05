#!/usr/bin/env python3
"""Execute the native bounded census with real allocation/ledger metadata.

Closed-ticket authority is modeled; this does not establish GPU retirement.
"""
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = (root / "userspace/services/intel-gpu/main.adb").read_text()
start = source.index("   type Table_Census_Record is record")
end = source.index("   end Advance_Context_Table_Census;", start) + len("   end Advance_Context_Table_Census;")
body = source[start:end]
prefix = '''with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Table_Allocations;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Buffer_Reply;
procedure Membership_Test is
   package P renames Intel_GPU_Table_Provenance;
begin
 for Fault in 0 .. 13 loop
  declare
   type Context is limited record
      Table_Owners : P.Ledger;
      Table_Generation : Unsigned_64 := 1;
   end record;
   Private_Contexts : array (1 .. 2) of Context;
   type Bytes is array (Positive range <>) of Unsigned_8;
   Ledger_Memory : Bytes (1 .. 4096) := [others => 0] with Alignment => 4096;
   Registry_Memory : Bytes (1 .. 65536) := [others => 0] with Alignment => 4096;
   function Admitted (Owner, ID : Unsigned_64; Slot : Positive) return Boolean is
     (Owner in 42 .. 43 and ID /= 0);
   function Receipt (Owner, ID : Unsigned_64) return Boolean is (False);
   package Table_Allocations is new Intel_GPU_Table_Allocations (Admitted, Receipt);
   Table_Backing_Registry : Table_Allocations.Registry;
   Application_Buffer_State : Natural := 0;
   Checks : Natural := 0;
   package Application_Buffers is
      function Ticket_Slot (ID : Unsigned_64) return Positive is (Positive (ID mod 256));
   end Application_Buffers;
   package Closed_Table_Tickets is
      function Can_Retire (Ignored : Natural; Owner, ID : Unsigned_64) return Boolean;
   end Closed_Table_Tickets;
   package body Closed_Table_Tickets is
      function Can_Retire (Ignored : Natural; Owner, ID : Unsigned_64) return Boolean is
      begin Checks := Checks + 1; return Owner = 42 and ID in 325 .. 327 and Fault /= 2; end;
   end Closed_Table_Tickets;
   procedure Resolve (Owner, ID, Offset : Unsigned_64;
     CPU, DMA : out Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := Owner in 42 .. 43 and ID /= 0;
      CPU := 16#100000# + Offset; DMA := 16#200000# + Offset;
   end;
   package Authority is new P.Authority (Resolve);
'''
suffix = '''
   Backing : constant Intel_GPU_Buffer_Reply.Backing := Intel_GPU_Buffer_Reply.From_Linear
     (16#300000#, Intel_GPU_Buffer_Reply.Layout.CPU_Base, 4096, 16#300000#);
   OK, Complete : Boolean;
   Turns, Before, Before_Checks : Natural := 0;
   Crossed, Restarted : Boolean := False;
   Owner : constant Unsigned_64 := (if Fault = 7 then 43 else 42);
  begin
   P.Extend (Private_Contexts (1).Table_Owners,
     Unsigned_64 (To_Integer (Ledger_Memory'Address)), 4096, OK); pragma Assert (OK);
   Table_Allocations.Extend (Table_Backing_Registry,
     Unsigned_64 (To_Integer (Registry_Memory'Address)), 32768, OK); pragma Assert (OK);
   pragma Assert (Table_Allocations.Capacity (Table_Backing_Registry) > 71);
   for I in 1 .. 64 loop
      Authority.Install (Private_Contexts (1).Table_Owners, Owner, 1, I, 257,
        Unsigned_64 (I - 1) * 4096, OK); pragma Assert (OK);
   end loop;
   if Fault /= 1 then
      Authority.Install (Private_Contexts (1).Table_Owners, Owner, 1, 65, 326, 0, OK);
      pragma Assert (OK);
   end if;
   if Fault = 8 then Private_Contexts (1).Table_Generation := 2; end if;
   Table_Allocations.Install (Table_Backing_Registry, 70,
     (if Fault = 5 then 43 else 42), (if Fault = 4 then 325 else 326),
     (if Fault = 3 then Table_Allocations.Replacement_Image else Table_Allocations.Incremental_Tables),
     Backing, OK); pragma Assert (OK);
   if Fault = 6 then
      Table_Allocations.Revoke (Table_Backing_Registry, 70, 42, 326, OK); pragma Assert (OK);
   end if;
   loop
      Turns := Turns + 1;
      Before := Parent_Table_Census (1).Cursor; Before_Checks := Checks;
      if Turns = 71 then
         if Fault = 9 then
            Authority.Install (Private_Contexts (1).Table_Owners, 42, 1, 66, 326, 4096, OK);
            pragma Assert (OK);
         elsif Fault = 10 then
            Table_Allocations.Revoke (Table_Backing_Registry, 70, 42, 326, OK); pragma Assert (OK);
         elsif Fault = 11 then
            Table_Allocations.Extend (Table_Backing_Registry,
              Unsigned_64 (To_Integer (Registry_Memory'Address)), 65536, OK); pragma Assert (OK);
         elsif Fault = 12 then
            Table_Allocations.Install (Table_Backing_Registry, 71, 42, 327,
              Table_Allocations.Incremental_Tables, Backing, OK); pragma Assert (OK);
         elsif Fault = 13 then
            Private_Contexts (1).Table_Generation := 2;
         end if;
      end if;
      Advance_Context_Table_Census (1, 42, Complete, OK);
      pragma Assert (Checks - Before_Checks <= 1);
      Crossed := Crossed or Parent_Table_Census (1).Ledger_Cursor = 65;
      if Turns = 71 and OK and Parent_Table_Census (1).Cursor < Before then Restarted := True; end if;
      exit when Complete or not OK;
      pragma Assert (Turns < 4096);
   end loop;
   pragma Assert (Complete = (Fault in 0 | 5 | 6 | 9 .. 11));
   if Complete and Fault /= 5 then pragma Assert (Crossed); end if;
   if Fault in 9 .. 11 then pragma Assert (Restarted); end if;
   -- Observation never erases ownership, including rejected/orphan groups.
   pragma Assert (Table_Allocations.Retained_At (Table_Backing_Registry, 70).Present);
   pragma Assert (P.Count (Private_Contexts (1).Table_Owners) >= 64);
  end;
 end loop;
 Ada.Text_IO.Put_Line ("Native context membership PASS14: closed incremental identities, revoked rows, cross64 ledger scans, missing/foreign membership, stale generation, mutation restarts; no releases");
end Membership_Test;
'''
work = Path(tempfile.mkdtemp(prefix="cubit-context-membership."))
(work / "test.gpr").write_text(f'''project Test is
 for Source_Dirs use (".", "{root}/userspace/services/intel-gpu");
 for Main use ("membership_test.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato");
 end Compiler;
end Test;
''')
variants = {
    "actual": body,
    "ignore_membership": body.replace("not OK or else not Current or else (not Found and then Next_Record = 0)", "not Current")
                             .replace("if not Found then", "if False then"),
    "ignore_generation": body.replace("Private_Contexts (Index).Table_Generation /= Intel_GPU_Table_Provenance.Generation (Ledger)", "False")
                             .replace("Census.Generation = Intel_GPU_Table_Provenance.Generation (Ledger)", "True"),
}
for name, code in variants.items():
    if name != "actual":
        assert code != body
    (work / "membership_test.adb").write_text(prefix + code + suffix)
    subprocess.run(["alr", "exec", "--", "gprbuild", "-f", "-q", "-p", "-P", str(work / "test.gpr")],
                   cwd=root / "kernel", check=True)
    result = subprocess.run([str(work / "membership_test")], capture_output=True, text=True)
    (work / f"{name}.log").write_text(result.stdout + result.stderr)
    if name == "actual":
        assert result.returncode == 0, result.stdout + result.stderr
        print(result.stdout.strip())
    else:
        assert result.returncode != 0 and "ASSERTION_ERROR" in result.stderr, result.stdout + result.stderr
        print(f"{name}: negative control rejected")
assert (root / "userspace/services/intel-gpu/main.adb").read_text() == source
print(f"Hosted census evidence (NOT hardware): {work}")
