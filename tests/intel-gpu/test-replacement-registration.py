#!/usr/bin/env python3
"""Compile the native registration block with real ledger/registry authority.

No GPU, IPC or supervisor is present. Exercise the exact resumption/early-return
code that precedes native publication, with modeled allocation ownership.
"""
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = (root / "userspace/services/intel-gpu/main.adb").read_text()
begin = source.index("            -- BEGIN STEPPED REPLACEMENT REGISTRATION")
end = source.index("            -- END STEPPED REPLACEMENT REGISTRATION", begin)
body = source[begin:end]
read_begin = source.index("   function Read_Replacement_Page (")
read_end = source.index("   end Read_Replacement_Page;", read_begin) + len("   end Read_Replacement_Page;")
reader = source[read_begin:read_end]
prefix = '''with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Table_Provenance;
with Intel_GPU_Table_References;
with Intel_GPU_Table_Allocations;
with Intel_GPU_Buffer_Reply;
procedure Registration_Test is
   package B renames Intel_GPU_Buffer_Reply;
   package Application_Buffers is
      function Ticket_Slot (ID : Unsigned_64) return Positive is (1);
   end Application_Buffers;
   package Application_VM is subtype Page_Number is Positive range 1 .. 64; end;
   package Application_State is
      package Table_References is new Intel_GPU_Table_References (64);
   end;
   package R renames Application_State.Table_References;
begin
   for Size in 1 .. 64 loop
      for Fault in 0 .. Size + 2 + (if Size > 4 then 1 else 0) loop
         declare
            Update_Table_Pages : constant Positive := Size;
            Update_Pending : constant Unsigned_64 := 7;
            Update_Session : constant Unsigned_64 := 42;
            Backing : constant B.Backing := B.From_Linear
              (16#200000#, B.Layout.CPU_Base, Unsigned_64 (Size) * 4096, 16#200000#);
            type Item_Record is limited record
               Table_Owners : Intel_GPU_Table_Provenance.Ledger;
               Table_Generation : Unsigned_64 := 1;
               Table_IDs : R.Map;
            end record;
            Item : Item_Record;
            type Metadata_Bytes is array (1 .. 4096) of Unsigned_8;
            Metadata : Metadata_Bytes := [others => 0] with Alignment => 4096;
            References : Metadata_Bytes := [others => 0] with Alignment => 4096;
            Extended : Boolean;
            Registry_Calls, Resolve_Calls, Turns : Natural := 0;
            Finished, Succeeded, Withdrawn : Boolean := False;
            function Update_Exclusive return Boolean is (not Withdrawn);
            function Admitted (Session, Ticket : Unsigned_64; Slot : Positive) return Boolean is
            begin
               Registry_Calls := Registry_Calls + 1;
               return not Withdrawn and Session = 42 and Ticket = 7 and Slot = 1;
            end Admitted;
            function Retired (Session, Ticket : Unsigned_64) return Boolean is (False);
            package Table_Allocations is new Intel_GPU_Table_Allocations (Admitted, Retired);
            Table_Backing_Registry : Table_Allocations.Registry;
            procedure Resolve (Session, Ticket, Offset : Unsigned_64;
                               CPU, DMA : out Unsigned_64; OK : out Boolean) is
               Selected : B.Backing;
            begin
               Resolve_Calls := Resolve_Calls + 1;
               CPU := 0; DMA := 0;
               Table_Allocations.Lookup (Table_Backing_Registry, 1, Session, Ticket,
                 Table_Allocations.Replacement_Image, Selected, OK);
               if not OK or else Offset / 4096 + 1 = Unsigned_64 (Fault) then
                  OK := False; return;
               end if;
               CPU := Selected.CPU_Address + Offset;
               DMA := B.Page_Address (Selected, Offset);
               OK := DMA /= 0;
            end Resolve;
            package Table_Authority is new Intel_GPU_Table_Provenance.Authority (Resolve);
            function Replacement_Table_Mapping (Slot : Positive; Session : Unsigned_64;
                                                P : Positive) return Intel_GPU_Table_Provenance.Mapping is
              (Table_Authority.Lookup (Item.Table_Owners, Session, Item.Table_Generation,
                                      R.Get (Item.Table_IDs, Item.Table_Generation, P)));
''' + reader + '''
            procedure Advance is
               OK : Boolean;
            begin
'''
suffix = '''
               if OK then
                  for P in 1 .. Size loop
                     declare
                        DMA : constant Unsigned_64 := Read_Replacement_Page (P);
                     begin
                        if DMA = 0 then OK := False; exit; end if;
                        pragma Assert (DMA = 16#200000# + Unsigned_64 (P - 1) * 4096);
                     end;
                  end loop;
               end if;
               Finished := True; Succeeded := OK;
            end Advance;
         begin
            Intel_GPU_Table_Provenance.Extend (Item.Table_Owners,
              Unsigned_64 (To_Integer (Metadata'Address)), 4096, Extended);
            pragma Assert (Extended);
            if Fault /= Size + 3 then
               R.Extend (Item.Table_IDs,
                 Unsigned_64 (To_Integer (References'Address)), 4096, Extended);
               pragma Assert (Extended);
            end if;
            if Fault = Size + 2 then
               R.Reopen (Item.Table_IDs, 1, 2, Extended);
               pragma Assert (Extended);
            end if;
            loop
               Turns := Turns + 1; Resolve_Calls := 0;
               -- Withdraw backing after the last successful registration but
               -- before the final publication preflight resolves the group.
               Withdrawn := Fault = Size + 1 and Turns = Size + 1;
               Advance;
               if not Finished then
                  pragma Assert (Resolve_Calls = 1);
                  pragma Assert (Intel_GPU_Table_Provenance.Count (Item.Table_Owners) = Turns);
                  for P in 1 .. Turns loop
                     pragma Assert (R.Get (Item.Table_IDs, 1, P) = P);
                  end loop;
                  for P in Turns + 1 .. 64 loop
                     pragma Assert (R.Get (Item.Table_IDs, 1, P) = 0);
                  end loop;
               end if;
               exit when Finished;
               pragma Assert (Turns <= Size);
            end loop;
            pragma Assert (Succeeded = (Fault = 0));
            pragma Assert (Turns = (if Fault = Size + 2 then 1
              elsif Fault = Size + 3 then 5
              elsif Fault = 0 or Fault = Size + 1 then Size + 1 else Fault));
            pragma Assert (Intel_GPU_Table_Provenance.Count (Item.Table_Owners) =
              (if Fault = Size + 2 then 1 elsif Fault = Size + 3 then 5
               elsif Fault = 0 or Fault = Size + 1 then Size else Fault - 1));
            if Fault >= Size + 2 then
               pragma Assert (R.Get (Item.Table_IDs, 1, Turns) = 0);
            end if;
            pragma Assert (Table_Allocations.Retained_At (Table_Backing_Registry, 1).Present);
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Native replacement registration PASS2332: real reference map, one page/turn, failed capacity/generation, no early publication, all failure offsets retain backing, final owner-loss rejects");
end Registration_Test;
'''
work = Path(tempfile.mkdtemp(prefix="cubit-replacement-registration."))
(work / "test.gpr").write_text(f'''project Test is
 for Source_Dirs use (".", "{root}/userspace/services/intel-gpu");
 for Main use ("registration_test.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato");
 end Compiler;
end Test;
''')
variants = {"actual": body,
            "early_publication": body.replace("if OK then return; end if;", "null;"),
            "ignore-put-failure": body.replace("if OK then return; end if;", "OK := True; return;")}
assert all(block != body for name, block in variants.items() if name != "actual")
for name, block in variants.items():
    (work / "registration_test.adb").write_text(prefix + block + suffix)
    subprocess.run(["gprbuild", "-q", "-p", "-P", str(work / "test.gpr")],
                   cwd=work, check=True)
    result = subprocess.run([str(work / "registration_test")], text=True, capture_output=True)
    (work / f"{name}.log").write_text(result.stdout + result.stderr)
    if name == "actual":
        assert result.returncode == 0, result.stdout + result.stderr
        print(result.stdout.strip())
    else:
        assert result.returncode != 0 and "ASSERTION_ERROR" in result.stderr, result.stdout + result.stderr
        print(name, "negative control rejected")
assert (root / "userspace/services/intel-gpu/main.adb").read_text() == source
print(f"Hosted adapter evidence (NOT hardware): {work}")
