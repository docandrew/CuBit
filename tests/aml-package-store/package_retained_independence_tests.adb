pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service_Core;
with AML_Delays;
with AML_Execute;
with Osi_Test_Support; use Osi_Test_Support;
with Owner_Transition_Data;
procedure Package_Retained_Independence_Tests is
   package S is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Supported_Interfaces => Configured);
   use type S.Stage_Status;
   use type S.Freeze_Status;
   use type S.Execution_Phase_Result;
   use type S.Values.Access_Status;
   use type S.Values.Module_Outcome;
   use type S.Values.Preparation_Outcome;
   use type AML_Execute.Execution_Status;
   use type S.Values.Value_Kind;
   Checks : Natural := 0;
   Evaluations : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Message; end if; end Check;
   procedure Run (Revision : Positive; First, Second : Bytes) is
      Service : aliased S.State (2, 8192, 4096, 2);
      Stage : S.Stage_Status;
      Frozen : S.Freeze_Status;
      Report : S.Execution_Report;
      Status : S.Values.Access_Status;
      Initialized : S.Namespace.Initialization_Report;
      Node : S.Namespace.Node_ID;
      Result : S.Values.Result;
      Description : S.Values.Value_Description;
      Old_Package, New_Package, Old_Element, New_Element : S.Values.Value_Handle;
      Scalar_Result : AML_Execute.Execution_Result;
      procedure Find (Name : String) is
      begin
         S.Find_Child (Service, S.Namespace.Root, Name, Node, Status);
         Check (Status = S.Values.Available and then Node /= S.Namespace.Root, "find " & Name);
      end Find;
      procedure Number (Handle : S.Values.Value_Handle; Expected : Unsigned_64) is
      begin
         S.Describe_Result (Service, Handle, Description, Status);
         Check (Status = S.Values.Available and then Description.Kind = S.Values.Integer_Description
           and then Description.Number = Expected, "retained integer payload");
      end Number;
      procedure Drop (Handle : in out S.Values.Value_Handle) is
      begin
         S.Release_Result (Service, Handle, Status);
         Check (Status = S.Values.Available, "release retained handle");
      end Drop;
   begin
      Check (Natural (First (9)) = Revision and then Natural (Second (9)) = Revision, "declared revision");
      S.Stage_Table (Service, 1, S.DSDT, First, Stage); Check (Stage = S.Staged, "first stage");
      S.Stage_Table (Service, 2, S.SSDT, Second, Stage); Check (Stage = S.Staged, "second stage");
      Check (S.Observe (Service).Objects = 0, "staging remains empty");
      S.Freeze_Catalog (Service, Frozen); Check (Frozen = S.Catalog_Frozen_OK, "freeze");
      S.Execute_Frozen (Service, 10000, Report, Status);
      Check (Status = S.Values.Available and then Report.Phase = S.Module_Execution_Reported
        and then Report.Preparation = S.Values.Firmware_Prepared
        and then Report.Modules.Outcome = S.Values.Modules_Completed
        and then Report.Modules.Completed = 2 and then Report.Modules.Charged <= 10000, "module execution");
      S.Initialize_Members (Service, Initialized, Status); Check (Status = S.Values.Available, "seal");
      for Repetition in 1 .. 2 loop
         Find ("INIT");
         S.Invoke_Scalar (Service, Node, [others => 0], 0, 10000, Scalar_Result);
         Check (Scalar_Result.Status = AML_Execute.Returned and then Scalar_Result.Value = 0, "initialize package");
         Find ("POST");
         S.Invoke_Retained (Service, Node, [others => 0], 0, 10000, Result, Status);
         Check (Status = S.Values.Available and then Result.Status = AML_Execute.Object_Returned, "old package retained");
         Old_Package := Result.Handle;
         S.Read_Result_Element (Service, Old_Package, 0, Old_Element, Status);
         Check (Status = S.Values.Available, "old integer retained");
         Number (Old_Element, 16#FFFFFFFFFFFFFFFF#);
         Find ("RAW0");
         S.Invoke_Retained (Service, Node, [others => 0], 0, 10000, Result, Status);
         Check (Status = S.Values.Available and then Result.Status = AML_Execute.Reference_Returned,
           "Index survives package rebinding");
         Drop (Result.Handle);
         Find ("POST");
         S.Invoke_Retained (Service, Node, [others => 0], 0, 10000, Result, Status);
         Check (Status = S.Values.Available and then Result.Status = AML_Execute.Object_Returned, "new package retained");
         New_Package := Result.Handle;
         S.Read_Result_Element (Service, New_Package, 0, New_Element, Status);
         Check (Status = S.Values.Available and then S.Retained_Results (Service) = 4, "independent roots");
         Number (Old_Element, 16#FFFFFFFFFFFFFFFF#);
         Number (New_Element, 0);
         Drop (Old_Package); Drop (New_Package);
         Number (Old_Element, 16#FFFFFFFFFFFFFFFF#);
         Number (New_Element, 0);
         Drop (Old_Element); Drop (New_Element);
         Check (S.Retained_Results (Service) = 0, "all roots released");
         Evaluations := Evaluations + 1;
      end loop;
   end Run;
begin
   Run (1, Owner_Transition_Data.Case_2_base, Owner_Transition_Data.Case_2_source);
   Run (2, Owner_Transition_Data.Case_4_base, Owner_Transition_Data.Case_4_source);
   Check (Evaluations = 4, "both revisions repeated");
   Ada.Text_IO.Put_Line ("Package retained independence checks" & Checks'Image);
end Package_Retained_Independence_Tests;
