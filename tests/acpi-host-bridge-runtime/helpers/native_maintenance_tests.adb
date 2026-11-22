pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
procedure Native_Maintenance_Tests is
   package HB is
      type Phase is (Fresh, Retained);
   end HB;
   package One is
      package Runtime is
         package Status is
            type Phase is (Fresh, Idle, Active, Stopped);
         end Status;
         Mode : Status.Phase := Status.Idle;
         function Current return Status.Phase is (Mode);
      end Runtime;
   end One;
   use type One.Runtime.Status.Phase, HB.Phase;
   type Runtime_Phases is array (Positive range <>) of One.Runtime.Status.Phase;
   Model_Held : Boolean := True;
   Model_Retained : Boolean := False;
   Hardware_Healthy, Query_Healthy : Boolean := True;
   Hardware_Deadline, Query_Deadline_Value, Power_Deadline : Unsigned_64 := 9000;
   Hardware_Calls, Query_Calls : Natural := 0;
   package Bridges is
      type Report is record Stage : HB.Phase; end record;
      function Blocks_AML return Boolean is (Model_Held);
      function Observe return Report is ((Stage => (if Model_Retained then HB.Retained else HB.Fresh)));
   end Bridges;
   package Root is
      procedure Maintain_Owner (Deadline : out Unsigned_64; May_Continue : out Boolean);
   end Root;
   package body Root is
      procedure Maintain_Owner (Deadline : out Unsigned_64; May_Continue : out Boolean) is
      begin Hardware_Calls := Hardware_Calls + 1; Deadline := Hardware_Deadline; May_Continue := Hardware_Healthy; end;
   end Root;
   package ACPI_Native_Passive_Queries is
      procedure Maintain (Deadline : out Unsigned_64; May_Continue : out Boolean);
   end ACPI_Native_Passive_Queries;
   package body ACPI_Native_Passive_Queries is
      procedure Maintain (Deadline : out Unsigned_64; May_Continue : out Boolean) is
      begin Query_Calls := Query_Calls + 1; Deadline := Query_Deadline_Value; May_Continue := Query_Healthy; end;
   end ACPI_Native_Passive_Queries;
   package ACPI_Native_Power_Entry is
      function Wake_Deadline_Milliseconds return Unsigned_64 is (Power_Deadline);
   end ACPI_Native_Power_Entry;
   procedure Maintain_Bridge_Owner (Deadline : out Unsigned_64; May_Continue : out Boolean) is
      Query_Deadline : Unsigned_64;
      Queries_Healthy : Boolean;
   begin
      --  The bridge manager exclusively owns this active or retained AML window.
      --  Keep actual hardware maintenance; do not apply the normal Idle-only test.
      Root.Maintain_Owner (Deadline, May_Continue);
      ACPI_Native_Passive_Queries.Maintain (Query_Deadline, Queries_Healthy);
      Deadline := Unsigned_64'Min (Deadline, Query_Deadline);
      Deadline := Unsigned_64'Min (Deadline, ACPI_Native_Power_Entry.Wake_Deadline_Milliseconds);
      May_Continue := May_Continue and then Queries_Healthy
        and then Bridges.Blocks_AML
        and then (One.Runtime.Current in One.Runtime.Status.Idle | One.Runtime.Status.Active
          or else Bridges.Observe.Stage = HB.Retained);
   end Maintain_Bridge_Owner;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with "native maintenance callback"; end if; end;
   Deadline : Unsigned_64;
   Healthy : Boolean;
begin
   for Mode of Runtime_Phases'[One.Runtime.Status.Fresh, One.Runtime.Status.Idle, One.Runtime.Status.Active, One.Runtime.Status.Stopped] loop
      for Held in Boolean loop
         for Retained in Boolean loop
            for Hardware_OK in Boolean loop
               for Queries_OK in Boolean loop
                  for Earliest in 1 .. 4 loop
                     One.Runtime.Mode := Mode; Model_Held := Held; Model_Retained := Retained;
                     Hardware_Healthy := Hardware_OK; Query_Healthy := Queries_OK;
                     Hardware_Deadline := (if Earliest = 1 then 1000 else Unsigned_64'Last);
                     Query_Deadline_Value := (if Earliest = 2 then 1000 else Unsigned_64'Last);
                     Power_Deadline := (if Earliest = 3 then 1000 else Unsigned_64'Last);
                     Hardware_Calls := 0; Query_Calls := 0;
                     Maintain_Bridge_Owner (Deadline, Healthy);
                     Check (Healthy = (Hardware_OK and Queries_OK and Held
                       and (Mode = One.Runtime.Status.Idle or Mode = One.Runtime.Status.Active or Retained)));
                     Check (Deadline = (if Earliest = 4 then Unsigned_64'Last else 1000));
                     Check (Hardware_Calls = 1 and Query_Calls = 1);
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("native maintenance checks" & Checks'Image);
end Native_Maintenance_Tests;
