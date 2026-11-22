pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables;
with AML_Delays;
with AML_Objects;
with ACPI_Service_Core;
with ACPI_FADT;
with ACPI_Local_Module_Startup;
with Module_Fixture;
procedure Local_Module_Tests is
   Mode : constant String := Ada.Command_Line.Argument (2);
   Width : constant String := Ada.Command_Line.Argument (1);
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with "check" & Checks'Image & " " & Mode; end if;
   end Check;
   package Core is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64,Retained_Result_Capacity => 8);
   Service : Core.State (3,4096,1024,3,Value_Object_Capacity => AML_Objects.Max_Objects,
      Value_Byte_Capacity => AML_Objects.Max_Bytes,Value_Element_Capacity => AML_Objects.Max_Elements);
   use type Core.Stage_Status,Core.Freeze_Status;
   use type Core.Values.Access_Status,Core.Import_Phase,Core.Execution_Phase_Result;
   use type Core.Values.Module_Outcome;
   Init_Calls, Execute_Calls, Close_Calls : Natural := 0;
   Initialized : Boolean := False;
   procedure Init (Facts : ACPI_FADT.Description; Accepted : out Boolean);
   procedure Execute (S : in out Core.State; Budget : Natural;
      Result : out Core.Execution_Report; Status : out Core.Values.Access_Status);
   procedure Close is
   begin
      Close_Calls := Close_Calls + 1;
      if Mode in "close-exception" | "execute-close-exception" then
         raise Program_Error with "modeled close";
      end if;
      Initialized := False;
   end Close;
   package Hook is new ACPI_Local_Module_Startup (Core,Init,Execute,Close);
   use type Hook.Phase,Hook.Reason;
   procedure Init (Facts : ACPI_FADT.Description; Accepted : out Boolean) is
      Recursive_Result : Core.Execution_Report;
      Recursive_Status : Core.Values.Access_Status;
   begin
      Init_Calls := Init_Calls + 1;
      Check (Core.Current_Import_Phase (Service) = Core.Catalog_Frozen);
      Check (Facts.Revision = 1 and then Facts.SCI = 9 and then Facts.Profile = 2);
      Check (Facts.FACS = 16#1234_5040# and then Execute_Calls = 0);
      if Mode = "init-exception" then raise Program_Error with "modeled init"; end if;
      if Mode = "reentry" then
         Hook.Execute_Module_Startup (Service,1000,Recursive_Result,Recursive_Status);
         Check (Recursive_Status = Core.Values.Wrong_Phase and then Init_Calls = 1
           and then Execute_Calls = 0 and then Close_Calls = 0);
      end if;
      Accepted := Mode not in "init-refused" | "close-exception"; Initialized := Accepted;
   end Init;
   procedure Execute (S : in out Core.State; Budget : Natural;
      Result : out Core.Execution_Report; Status : out Core.Values.Access_Status) is
   begin
      Execute_Calls := Execute_Calls + 1;
      Check (Initialized and then Init_Calls = 1 and then Close_Calls = 0);
      if Mode in "execute-exception" | "execute-close-exception" then
         raise Program_Error with "modeled execute";
      end if;
      if Mode = "executed-exception" then
         Core.Execute_Frozen (S,Budget,Result,Status);
         raise Program_Error with "modeled post-execution";
      end if;
      if Mode = "false-success" then
         Result := (others => <>); Result.Phase := Core.Module_Execution_Reported;
         Result.Modules.Outcome := Core.Values.Modules_Completed; Status := Core.Values.Available;
      else
         Core.Execute_Frozen (S,(if Mode = "module-refused" then 0 else Budget),Result,Status);
      end if;
   end Execute;
   procedure Seal (Data : in out Firmware_Tables.Bytes) is
      Sum : Firmware_Tables.Byte := 0;
   begin
      Data (Data'First + 9) := 0;
      for B of Data loop Sum := Sum + B; end loop;
      Data (Data'First + 9) := 0 - Sum;
   end Seal;
   function FADT (Length : Positive) return Firmware_Tables.Bytes is
      Data : Firmware_Tables.Bytes (1 .. Length) := [others => 0];
      Signature : constant String := "FACP";
   begin
      for I in Signature'Range loop Data (I) := Character'Pos (Signature (I)); end loop;
      Data (5) := Firmware_Tables.Byte (Length); Data (9) := 1;
      if Length >= 116 then
         Data (37 .. 40) := [16#40#,16#50#,16#34#,16#12#];
         Data (46) := 2; Data (47) := 9;
      end if;
      Seal (Data); return Data;
   end FADT;
   AML : Firmware_Tables.Bytes := Module_Fixture.Data;
   Fixed : Firmware_Tables.Bytes := FADT ((if Mode = "malformed" then 36 else 116));
   Staged : Core.Stage_Status;
   Frozen : Core.Freeze_Status;
   Result, Again : Core.Execution_Report;
   Status, Again_Status : Core.Values.Access_Status;
   Success : constant Boolean := Mode in "normal" | "reentry";
   Expected : Hook.Reason := Hook.Not_Attempted;
   Before_Phase : Core.Import_Phase;
begin
   Check (Width in "32" | "64");
   if Width = "32" then AML (AML'First + 8) := 1; Seal (AML); end if;
   Core.Stage_Table (Service,1,Core.DSDT,AML,Staged); Check (Staged = Core.Staged);
   if Mode /= "missing" then
      Core.Stage_Table (Service,2,Core.Description,Fixed,Staged); Check (Staged = Core.Staged);
      if Mode = "duplicate" then
         Core.Stage_Table (Service,3,Core.Description,Fixed,Staged); Check (Staged = Core.Staged);
      end if;
   end if;
   -- The hook must read the retained copy, not this now-modified source.
   if Fixed'Length >= 116 then Fixed (46) := 7; Fixed (47) := 99; end if;
   if Mode /= "unfrozen" then Core.Freeze_Catalog (Service,Frozen); Check (Frozen = Core.Catalog_Frozen_OK); end if;
   declare
      Propagated : Boolean := False;
   begin
      begin
         Hook.Execute_Module_Startup (Service,(if Mode = "zero-budget" then 0 else 1000),Result,Status);
      exception when others => Propagated := True;
      end;
      Check (not Propagated);
   end;
   Check (Core.Valid (Service));
   Check (Hook.Observe.Local_Attempted = (Init_Calls = 1));
   Check (Hook.Observe.Module_Attempted = (Execute_Calls = 1));
   if Success then
      if Hook.Observe.Stage /= Hook.Completed then
         Ada.Text_IO.Put_Line ("startup failure=" & Hook.Observe.Status'Image &
           " catalog=" & Core.Current_Import_Phase (Service)'Image &
           " phase=" & Result.Phase'Image & " modules=" & Result.Modules.Outcome'Image);
      end if;
      Check (Hook.Observe.Stage = Hook.Completed and then Hook.Observe.Status = Hook.Modules_Completed);
      Check (Init_Calls = 1 and then Execute_Calls = 1 and then Close_Calls = 0);
      Check (Status = Core.Values.Available and then Core.Current_Import_Phase (Service) = Core.Catalog_Executed);
   else
      Expected := (if Mode = "missing" then Hook.Missing_FADT
        elsif Mode = "duplicate" then Hook.Duplicate_FADT
        elsif Mode = "malformed" then Hook.Malformed_FADT
        elsif Mode = "unfrozen" then Hook.Catalog_Not_Frozen
        elsif Mode = "zero-budget" then Hook.No_Work_Budget
        elsif Mode in "init-refused" | "init-exception" | "close-exception" then Hook.Local_Initialization_Refused
        else Hook.Module_Execution_Refused);
      Check (Hook.Observe.Stage = Hook.Failed and then Hook.Observe.Status = Expected);
      Check (Status = Core.Values.Wrong_Phase);
      Check (Close_Calls = 1);
      Check (Initialized = (Mode = "execute-close-exception"));
      Check (Init_Calls = (if Mode in "init-refused" | "init-exception" | "close-exception"
        | "module-refused" | "false-success" | "execute-exception" | "executed-exception"
        | "execute-close-exception" then 1 else 0));
      Check (Execute_Calls = (if Mode in "module-refused" | "false-success"
        | "execute-exception" | "executed-exception" | "execute-close-exception" then 1 else 0));
      if Mode in "init-exception" | "execute-exception" | "executed-exception" | "execute-close-exception" then
         Check (Result.Phase = Core.Execution_Not_Started);
      end if;
   end if;
   Before_Phase := Core.Current_Import_Phase (Service);
   Check (Hook.Observe.Initialization_Raised = (Mode = "init-exception"));
   Check (Hook.Observe.Execution_Raised =
     (Mode in "execute-exception" | "executed-exception" | "execute-close-exception"));
   Check (Hook.Observe.Close_Attempted = not Success);
   Check (Hook.Observe.Close_Completed =
     (not Success and then Mode not in "close-exception" | "execute-close-exception"));
   if Mode = "executed-exception" then
      Check (Before_Phase = Core.Catalog_Executed);
   end if;
   Hook.Execute_Module_Startup (Service,1000,Again,Again_Status);
   Check (Again_Status = Core.Values.Wrong_Phase and then Again.Phase = Core.Execution_Not_Started);
   Check (Core.Current_Import_Phase (Service) = Before_Phase);
   Check (Close_Calls = (if Success then 0 else 1));
   Check (Hook.Observe.Status = (if Success then Hook.Modules_Completed else Expected));
   Ada.Text_IO.Put_Line ("LOCAL MODULE width=" & Width & " mode=" & Mode & " checks" & Checks'Image);
end Local_Module_Tests;
