#!/usr/bin/env python3
"""Execute the native stepped alias preflight with real VM/backing metadata.

Exclusion, context death and retirement receipts are modeled, not hardware proof.
"""
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = (root / "userspace/services/intel-gpu/main.adb").read_text()
start = source.index("   type Recycle_Preparation_Phase is")
end = source.index("   end Prepare_Table_Retirement;", start) + len("   end Prepare_Table_Retirement;")
body = source[start:end]
prefix = '''with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Snapshots;
with Intel_GPU_ADLN_PPGTT;
with Intel_GPU_Table_Allocations;
procedure Preparation_Test is
   package B renames Intel_GPU_Buffer_Reply;
   package Application_VM is new Intel_GPU_VM_Image (8);
   package Live_Snapshots is new Application_VM.Snapshots;
   package Intel_GPU_Buffer_Reply renames B;
begin
   for Fault in 0 .. 22 loop
      declare
         type Context is limited record
            Attempted : Boolean := True;
            Source : Application_VM.Image;
            Parent : B.Backing;
         end record;
         Private_Contexts : array (1 .. 2) of Context;
         package Application_State is
            type Update_Record is limited record
               Tables : B.Backing;
               Candidate : Application_VM.Image;
            end record;
            Updates : array (1 .. 4) of Update_Record;
            function Has_Update (I : Positive) return Boolean is (I <= 4);
         end Application_State;
         type Replacement_Record is record Root : Unsigned_64 := 16#200000#; Revision : Unsigned_64 := 1; end record;
         Limit : Positive := 4;
         Replacement_Tables : Natural := 0;
         package Replacement_Records is
            function Capacity (Ignored : Natural) return Positive is (Limit);
            function Get (Ignored : Natural; I : Positive) return Replacement_Record is
              ((Root => 16#200000#, others => <>));
         end Replacement_Records;
         Recycle_Slot : constant Positive := 1;
         Recycle_Ticket : constant Unsigned_64 := 7;
         function Admitted (Session, Ticket : Unsigned_64; Slot : Positive) return Boolean is
           (Session = 42 and ((Ticket = 7 and Slot = 1) or (Ticket = 8 and Slot = 2)));
         function Receipt (Session, Ticket : Unsigned_64) return Boolean is (False);
         package Table_Allocations is new Intel_GPU_Table_Allocations (Admitted, Receipt);
         Table_Backing_Registry : Table_Allocations.Registry;
         package Application_Buffers is
            function Ticket_Slot (ID : Unsigned_64) return Positive is (if ID = 8 then 2 else 1);
         end Application_Buffers;
         Closed_Table_Index : Positive := (if Fault = 12 then 2 else 1);
         Closed_Table_Source_Revision : Unsigned_64 := 1;
         Buffer_Retirement_Is_Closed_Table : Boolean := Fault in 3 .. 5 | 11 .. 13 | 16;
         Buffer_Retirement_Is_Context : Boolean := Fault >= 17;
         Recycle_Context_Index : Positive := 1;
         Context_Retirement_Revision : Unsigned_64 := 1;
         type Root_Mapping is record DMA : Unsigned_64 := 16#200000#; end record;
         Context_Retirement_Root : Root_Mapping;
         Owner_Ready : Boolean := True;
         function Current_Table_Ticket (I : Positive) return Unsigned_64 is
           (if Fault = 13 then 8 else 7);
         function Recycle_May_Release (Owner, ID : Unsigned_64) return Boolean is
           (Owner_Ready and Owner = 42 and ID in 7 .. 8);
'''
suffix = '''
         procedure Initialize (Object : in out Application_VM.Image;
                               Base : Unsigned_64; Seal : Boolean := True) is
            Pages : Application_VM.Backing_Pages;
            OK : Boolean;
         begin
            for P in Pages'Range loop Pages (P) := Base + Unsigned_64 (P - 1) * 4096; end loop;
            Application_VM.Initialize (Object, Pages, OK); pragma Assert (OK);
            Application_VM.Map_Page (Object, 4096, 16#A00000#,
              Intel_GPU_ADLN_PPGTT.Write_Back, Intel_GPU_ADLN_PPGTT.Read_Write, OK);
            pragma Assert (OK);
            if Seal then Application_VM.Seal (Object, OK); pragma Assert (OK); end if;
         end Initialize;
         Complete, Failed, OK : Boolean;
         Turns : Natural := 0;
      begin
         Initialize (Private_Contexts (1).Source,
           (if Buffer_Retirement_Is_Context or (Buffer_Retirement_Is_Closed_Table and Fault /= 4) then 16#200000# else 16#500000#),
           Seal => Fault /= 3);
         Initialize (Private_Contexts (2).Source,
           (if Fault in 1 | 9 then 16#200000# else 16#600000#));
         if Fault = 9 then
            Live_Snapshots.Forget_Retired (Private_Contexts (2).Source,
              Application_VM.Revision (Private_Contexts (2).Source),
              Application_VM.Root_DMA (Private_Contexts (2).Source), True, OK);
            pragma Assert (OK);
         end if;
         Closed_Table_Source_Revision := Application_VM.Revision (Private_Contexts (1).Source);
         Context_Retirement_Revision := Closed_Table_Source_Revision + (if Fault = 19 then 1 else 0);
         Private_Contexts (1).Parent := B.From_Linear
           (16#200000#, B.Layout.CPU_Base, 4096, 16#200000#);
         if Fault = 5 then Closed_Table_Source_Revision := Closed_Table_Source_Revision + 1; end if;
         for I in 1 .. 4 loop
            Application_State.Updates (I).Tables := B.From_Linear
              (16#200000#, B.Layout.CPU_Base, 4096, 16#200000#);
            Initialize (Application_State.Updates (I).Candidate,
              (if I = 1 and Fault = 21 then 16#201000#
               elsif (I = 1 and (not Buffer_Retirement_Is_Context or Fault = 18)) or (Fault = 2 and I = 2) then 16#200000#
               else 16#700000# + Unsigned_64 (I) * 65536),
              Seal => not (Fault = 10 and I = 2));
         end loop;
         if Fault = 8 then Application_State.Updates (1).Tables := (Ready => False); end if;
         if not Buffer_Retirement_Is_Context then
         Table_Allocations.Install (Table_Backing_Registry, 1, 42, 7,
           Table_Allocations.Replacement_Image, Application_State.Updates (1).Tables, OK);
         pragma Assert (OK = (Fault /= 8));
         end if;
         if Fault in 16 | 20 .. 22 then
            Table_Allocations.Install (Table_Backing_Registry, 2, 42, 8,
              Table_Allocations.Incremental_Tables,
              B.From_Linear (16#201000#, B.Layout.CPU_Base + 4096, 4096, 16#200000#), OK);
            pragma Assert (OK);
            if Fault = 22 then
               Table_Allocations.Revoke (Table_Backing_Registry, 2, 42, 8, OK);
               pragma Assert (OK);
            end if;
         end if;
         if Fault = 15 then
            Table_Allocations.Revoke (Table_Backing_Registry, 1, 42, 7, OK);
            pragma Assert (OK); -- cleanup observations survive live revocation
         end if;
         loop
            Turns := Turns + 1;
            if Turns = 2 then
               if Fault = 6 then Limit := 5; end if;
               if Fault = 7 then Owner_Ready := False; end if;
               if Fault = 14 then
                  Table_Allocations.Revoke (Table_Backing_Registry, 1, 42, 7, OK);
                  pragma Assert (OK); -- a change mid-scan invalidates earlier checks
               end if;
            end if;
            Prepare_Table_Retirement (42, (if Fault in 16 | 20 .. 22 then 8 else 7), Complete, Failed);
            exit when Complete or Failed;
            pragma Assert (Turns < 7);
         end loop;
         pragma Assert (Complete = (Fault in 0 | 9 | 11 | 15 | 16 | 17 | 20 | 22));
         pragma Assert (Failed = (Fault not in 0 | 9 | 11 | 15 | 16 | 17 | 20 | 22));
         if Complete then pragma Assert (Turns = 7); end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Native retirement preparation PASS23: context parent/child groups, revoked child ranges, child-specific aliases, same-slot candidate alias rejection, captured epoch and closed-source identity");
end Preparation_Test;
'''
work = Path(tempfile.mkdtemp(prefix="cubit-retirement-preparation."))
(work / "test.gpr").write_text(f'''project Test is
 for Source_Dirs use (".", "{root}/userspace/services/intel-gpu");
 for Main use ("preparation_test.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato");
 end Compiler;
end Test;
''')
variants = {
    "actual": body,
    "ignore_aliases": body.replace("not Disjoint (Private_Contexts (I).Source)", "False")
                         .replace("not Disjoint (Application_State.Updates (I).Candidate)", "False"),
    "ignore_closed_identity": body.replace("I = Closed_Table_Index", "True")
                                  .replace("Current_Table_Ticket (I) = Recycle_Ticket", "True"),
    "child_as_anchor": body.replace("Current_Table_Ticket (I) = Recycle_Ticket",
                                    "Current_Table_Ticket (I) = ID"),
}
for name, block in variants.items():
    if name != "actual":
        assert block != body
    (work / "preparation_test.adb").write_text(prefix + block + suffix)
    subprocess.run(["alr", "exec", "--", "gprbuild", "-f", "-q", "-p", "-P", str(work / "test.gpr")],
                   cwd=root / "kernel", check=True)
    result = subprocess.run([str(work / "preparation_test")], text=True, capture_output=True)
    (work / f"{name}.log").write_text(result.stdout + result.stderr)
    if name == "actual":
        assert result.returncode == 0, result.stdout + result.stderr
        print(result.stdout.strip())
    else:
        assert result.returncode != 0 and "ASSERTION_ERROR" in result.stderr, result.stdout + result.stderr
        print(f"{name}: negative control rejected")
print(f"Hosted adapter evidence (NOT hardware): {work}")
