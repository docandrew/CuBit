#!/usr/bin/env python3
"""Compile the exact native cleanup gate against fault-controlled host facts.

This tests its decisions, not the truth of hardware/authority observations.
"""
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "userspace/services/intel-gpu"
main = (SOURCE / "main.adb").read_text()
start = main.index("   function Image_Retirement_Owner return Boolean is")
end = main.index("   end Image_Retirement_Owner;", start) + len("   end Image_Retirement_Owner;")
gate = main[start:end]
query_start = main.index("            declare\n               Index : constant Positive := Stored;", main.index("   procedure Handle_Retirement_Query"))
query_end = main.index("            end;", query_start) + len("            end;")
query = main[query_start:query_end]
prefix = r"""
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Application_Lifetime;
with Intel_GPU_GuC_Context_Lifecycle;
procedure Retirement_Gate_Tests is
   package Application_Lifetime renames Intel_GPU_Application_Lifetime;
   package Context_Life renames Intel_GPU_GuC_Context_Lifecycle;
   use type Application_Lifetime.Phase;
   use type Context_Life.Phase;
   package Intel_GPU_Render_Sessions is
      Tag_Base : constant Unsigned_64 := 100;
   end Intel_GPU_Render_Sessions;
   Render_Admission : Boolean := True;
   package Intel_GPU_Render_Control is
      function Issued_Tag (Object : Boolean; Index : Natural) return Unsigned_64 is
        (if not Object or Index not in 1 .. 4 then 0 else 100 + Unsigned_64 (Index));
   end Intel_GPU_Render_Control;
   package Intel_GPU_Submission_Image is
      GGTT_Bytes : constant Unsigned_64 := 20 * 4096;
   end Intel_GPU_Submission_Image;
   Image_Retirement_Index : Natural := 1;
   Image_Retirement_First : Unsigned_64 := 4096;
   PCI_Device : Unsigned_16 := 16#46D2#;
   Render_Backend_Ready, Reset_Pages_Mapped, Work_Drained, Range_Allowed : Boolean := True;
   package Intel_GPU_Native_Reset is
      Last_Succeeded : Boolean := True;
   end Intel_GPU_Native_Reset;
   type Context_Record is record
      Parent_Ticket : Unsigned_64 := 1;
      Life : Application_Lifetime.Phase := Application_Lifetime.Retired;
   end record;
   Private_Contexts : array (1 .. 4) of Context_Record;
   Application_Buffer_State : Boolean := True;
   Ticket_Owner_Valid : Boolean := True;
   package Application_Buffers is
      subtype Ticket is Unsigned_64 range 0 .. 16 * Unsigned_64 (Unsigned_32'Last);
      function Ticket_Session (Object : Boolean; ID : Ticket) return Unsigned_64 is
        (if Ticket_Owner_Valid and ID = 1 then 100 + Unsigned_64 (Image_Retirement_Index) else 0);
   end Application_Buffers;
   type Phases is array (1 .. 4) of Context_Life.Phase;
   Contexts : Phases := [others => Context_Life.Disabled];
   First_Context_ID : constant Unsigned_32 := 7;
   package Context_Pool is
      function Count (Object : Phases) return Natural is (Object'Length);
      function State (Object : Phases; ID : Unsigned_32) return Context_Life.Phase is
        (Object (Positive (ID - First_Context_ID + 1)));
   end Context_Pool;
   package Context_Drain is
      type Retirement_State is (Deregistered, Disabled, Pending, Uncertain, Admission_Open, No_Context);
      Observed : Retirement_State := Deregistered;
      function Observe (Object : Phases; Session : Unsigned_64) return Retirement_State;
   end Context_Drain;
   package body Context_Drain is
      function Observe (Object : Phases; Session : Unsigned_64) return Retirement_State is
      begin
         pragma Assert (Session = 100 + Unsigned_64 (Image_Retirement_Index));
         return Observed;
      end Observe;
   end Context_Drain;
   Application_Map_State : Boolean := True;
   package Application_Maps is
      type Retirement_State is (Clear, Pending, Uncertain);
      Observed : Retirement_State := Clear;
      function Observe_Retirement (Object : Boolean; Session : Unsigned_64) return Retirement_State;
   end Application_Maps;
   package body Application_Maps is
      function Observe_Retirement (Object : Boolean; Session : Unsigned_64) return Retirement_State is
      begin
         pragma Assert (Session = 100 + Unsigned_64 (Image_Retirement_Index));
         return Observed;
      end Observe_Retirement;
   end Application_Maps;
   function Application_Work_Drained (Session : Unsigned_64) return Boolean is
   begin
      pragma Assert (Session = 100 + Unsigned_64 (Image_Retirement_Index));
      return Work_Drained;
   end Application_Work_Drained;
   function Runtime_Range_Allowed (First, Bytes : Unsigned_64) return Boolean is
   begin
      pragma Assert (First = Image_Retirement_First and Bytes = 20 * 4096);
      return Range_Allowed;
   end Runtime_Range_Allowed;
"""
suffix = r"""
   Checks : Natural := 0;
   procedure Expect (Value : Boolean) is
   begin
      pragma Assert (Image_Retirement_Owner = Value);
      Checks := Checks + 1;
   end Expect;
begin
   Expect (True);
   Private_Contexts (1).Parent_Ticket := 0; Expect (False);
   Private_Contexts (1).Parent_Ticket := Application_Buffers.Ticket'Last + 1; Expect (False);
   Private_Contexts (1).Parent_Ticket := 17; Expect (False); -- stale/wrong generation
   Private_Contexts (1).Parent_Ticket := 1;
   Ticket_Owner_Valid := False; Expect (False); Ticket_Owner_Valid := True;
   Image_Retirement_Index := 0; Expect (False); Image_Retirement_Index := 1;
   Image_Retirement_First := 0; Expect (False); Image_Retirement_First := 4096;
   PCI_Device := 0; Expect (False); PCI_Device := 16#46D2#;
   Render_Backend_Ready := False; Expect (False); Render_Backend_Ready := True;
   Reset_Pages_Mapped := False; Expect (False); Reset_Pages_Mapped := True;
   Intel_GPU_Native_Reset.Last_Succeeded := False; Expect (False);
   Intel_GPU_Native_Reset.Last_Succeeded := True;
   Work_Drained := False; Expect (False); Work_Drained := True;
   Range_Allowed := False; Expect (False); Range_Allowed := True;
   for I in Private_Contexts'Range loop
      Image_Retirement_Index := I;
      for Life in Application_Lifetime.Phase loop
         Private_Contexts (I).Life := Life;
         Expect (Life = Application_Lifetime.Retired);
      end loop;
      Private_Contexts (I).Life := Application_Lifetime.Retired;
   end loop;
   for GPU in Context_Drain.Retirement_State loop
      Context_Drain.Observed := GPU;
      Expect (GPU = Context_Drain.Deregistered);
   end loop;
   Context_Drain.Observed := Context_Drain.Deregistered;
   for CPU in Application_Maps.Retirement_State loop
      Application_Maps.Observed := CPU;
      Expect (CPU = Application_Maps.Clear);
   end loop;
   Application_Maps.Observed := Application_Maps.Clear;
   for I in Contexts'Range loop
      for Phase in Context_Life.Phase loop
         Contexts (I) := Phase;
         Expect (Phase in Context_Life.Disabled | Context_Life.Deregistered);
      end loop;
      Contexts (I) := Context_Life.Deregistered;
   end loop;
   Expect (True);
   Ada.Text_IO.Put_Line ("Native retirement gate PASS" & Natural'Image (Checks) &
     " checks (actual function, mocked observations; NOT hardware authority proof)");
   for Registered in Boolean loop
      for Attempted in Boolean loop
         for Status in Image_Retirement.Result loop
            for Uncertain in Boolean loop
               for Pending in Boolean loop
                  Application_Registration_Attempted (1) := Registered;
                  Image_Retirement_Attempted (1) := Attempted;
                  Image_Retirement_Results (1) := Status;
                  Facts := (Work_Pending => Pending, Uncertain => Uncertain);
                  Apply_Query;
                  pragma Assert (Facts.Work_Pending =
                    (Pending or (Registered and not Attempted)));
                  pragma Assert (Facts.Uncertain =
                    (Uncertain or (Registered and Attempted and Status /= Image_Retirement.Address_Released)));
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Native retirement query PASS48 cases: no early completion or uncertainty erasure");
end Retirement_Gate_Tests;
"""
query_fixture = r"""
   package Image_Retirement is
      type Result is (Rejected, Quarantined, Address_Released);
   end Image_Retirement;
   use type Image_Retirement.Result;
   Application_Registration_Attempted : array (1 .. 4) of Boolean := [others => False];
   Image_Retirement_Attempted : array (1 .. 4) of Boolean := [others => False];
   Image_Retirement_Results : array (1 .. 4) of Image_Retirement.Result := [others => Image_Retirement.Rejected];
   type Drain_Facts is record
      Work_Pending, Uncertain : Boolean;
   end record;
   Facts : Drain_Facts;
   procedure Apply_Query is
      Session : constant Unsigned_64 := 101;
      Stored : constant Positive := 1;
   begin
""" + query + "\n   end Apply_Query;\n"
# Operator visibility belongs to the fixture, not the extracted gate.
prefix += "   use type Context_Drain.Retirement_State;\n   use type Application_Maps.Retirement_State;\n"
with tempfile.TemporaryDirectory(prefix="cubit-retirement-gate-") as directory:
    work = Path(directory)
    (work / "retirement_gate_tests.adb").write_text(prefix + gate + query_fixture + suffix)
    (work / "test.gpr").write_text(f'''project Test is
      for Source_Dirs use (".", "{SOURCE}");
      for Object_Dir use "obj";
      for Exec_Dir use ".";
      for Main use ("retirement_gate_tests.adb");
      package Compiler is
        for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
      end Compiler;
    end Test;
    ''')
    subprocess.run(["gprbuild", "-p", "-P", "test.gpr"], cwd=work, check=True)
    subprocess.run([str(work / "retirement_gate_tests")], cwd=work, check=True)
