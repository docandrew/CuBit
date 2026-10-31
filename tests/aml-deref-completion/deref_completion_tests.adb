pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service_Core;
with AML_Delays;
with AML_Execute;
with Osi_Test_Support; use Osi_Test_Support;
with Deref_Completion_Data;
procedure Deref_Completion_Tests is
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
   type Observation_Kind is (Scalar, Package_Element);
   type Expected_Result is record
      Name : String (1 .. 4);
      Number : Unsigned_64;
      Kind : Observation_Kind;
   end record;
   type Expectations is array (Positive range <>) of Expected_Result;
   procedure Run (Revision : Positive; First, Second : Bytes; Expected : Expectations) is
      Service : aliased S.State (2, 8192, 4096, 2);
      Stage : S.Stage_Status;
      Frozen : S.Freeze_Status;
      Report : S.Execution_Report;
      Status : S.Values.Access_Status;
      Initialized : S.Namespace.Initialization_Report;
      Node : S.Namespace.Node_ID;
      Result : S.Values.Result;
      Description : S.Values.Value_Description;
      Element : S.Values.Value_Handle;
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
      for Item of Expected loop
         S.Find_Child (Service, S.Namespace.Root, Item.Name, Node, Status);
         Check (Status = S.Values.Available and then Node /= S.Namespace.Root, "find " & Item.Name);
         S.Invoke_Retained (Service, Node, [others => 0], 0, 10000, Result, Status);
         Check (Status = S.Values.Available and then Result.Charged <= 10000, "invoke " & Item.Name);
         case Item.Kind is
            when Scalar =>
               Check (Result.Status = AML_Execute.Returned, "explicit scalar result " & Item.Name & Result.Status'Image);
               Check (Result.Number = Item.Number, "revision" & Revision'Image & " " & Item.Name & " expected" & Item.Number'Image & " actual" & Result.Number'Image);

            when Package_Element =>
               Check (Result.Status = AML_Execute.Object_Returned, "missing package " & Item.Name);
               Check (S.Retained_Results (Service) = 1, "package retained");
               S.Describe_Result (Service, Result.Handle, Description, Status);
               Check (Status = S.Values.Available and then Description.Kind = S.Values.Package_Description
                 and then Description.Length = 1, "package shape " & Item.Name);
               -- This reads a retained package element through the observation
               -- API, not an AML DerefOf opcode that could truncate it.
               S.Read_Result_Element (Service, Result.Handle, 0, Element, Status);
               Check (Status = S.Values.Available and then S.Retained_Results (Service) = 2, "element pin");
               S.Describe_Result (Service, Element, Description, Status);
               Check (Status = S.Values.Available and then Description.Kind = S.Values.Integer_Description, "integer element");
               Check (Description.Number = Item.Number,
                 "revision" & Revision'Image & " package " & Item.Name & " expected" & Item.Number'Image & " actual" & Description.Number'Image);
               S.Release_Result (Service, Element, Status); Check (Status = S.Values.Available, "release element");
               S.Release_Result (Service, Result.Handle, Status); Check (Status = S.Values.Available, "release package");
         end case;
         Check (S.Retained_Results (Service) = 0, "retained result leak");
         Evaluations := Evaluations + 1;
      end loop;
   end Run;
begin
   Run (1, Deref_Completion_Data.Base_1, Deref_Completion_Data.Code_1,
     [("TEST", 16#FFFFFFFFFFFFFFFF#, Scalar), ("TEST", 16#FFFFFFFFFFFFFFFF#, Scalar)]); -- r1-arg-control
   Run (1, Deref_Completion_Data.Base_2, Deref_Completion_Data.Code_2,
     [("TEST", 16#FFFFFFFFFFFFFFFF#, Scalar), ("TEST", 16#FFFFFFFFFFFFFFFF#, Scalar)]); -- r1-local-control
   Run (2, Deref_Completion_Data.Base_3, Deref_Completion_Data.Code_3,
     [("TEST", 16#FFFFFFFFFFFFFFFF#, Scalar), ("TEST", 16#FFFFFFFFFFFFFFFF#, Scalar)]); -- r2-arg-control
   Run (2, Deref_Completion_Data.Base_4, Deref_Completion_Data.Code_4,
     [("TEST", 16#FFFFFFFFFFFFFFFF#, Scalar), ("TEST", 16#FFFFFFFFFFFFFFFF#, Scalar)]); -- r2-local-control
   Run (1, Deref_Completion_Data.Base_5, Deref_Completion_Data.Code_5,
     [("TEST", 16#00000000FFFFFFFF#, Scalar), ("TEST", 16#00000000FFFFFFFF#, Scalar)]); -- r1-arg-ref
   Run (1, Deref_Completion_Data.Base_6, Deref_Completion_Data.Code_6,
     [("TEST", 16#00000000FFFFFFFF#, Scalar), ("TEST", 16#00000000FFFFFFFF#, Scalar)]); -- r1-arg-held
   Run (1, Deref_Completion_Data.Base_7, Deref_Completion_Data.Code_7,
     [("TEST", 16#00000000FFFFFFFF#, Scalar), ("TEST", 16#00000000FFFFFFFF#, Scalar)]); -- r1-local-ref
   Run (1, Deref_Completion_Data.Base_8, Deref_Completion_Data.Code_8,
     [("TEST", 16#00000000FFFFFFFF#, Scalar), ("TEST", 16#00000000FFFFFFFF#, Scalar)]); -- r1-local-held
   Run (2, Deref_Completion_Data.Base_9, Deref_Completion_Data.Code_9,
     [("TEST", 16#FFFFFFFFFFFFFFFF#, Scalar), ("TEST", 16#FFFFFFFFFFFFFFFF#, Scalar)]); -- r2-arg-ref
   Run (2, Deref_Completion_Data.Base_10, Deref_Completion_Data.Code_10,
     [("TEST", 16#FFFFFFFFFFFFFFFF#, Scalar), ("TEST", 16#FFFFFFFFFFFFFFFF#, Scalar)]); -- r2-arg-held
   Run (2, Deref_Completion_Data.Base_11, Deref_Completion_Data.Code_11,
     [("TEST", 16#FFFFFFFFFFFFFFFF#, Scalar), ("TEST", 16#FFFFFFFFFFFFFFFF#, Scalar)]); -- r2-local-ref
   Run (2, Deref_Completion_Data.Base_12, Deref_Completion_Data.Code_12,
     [("TEST", 16#FFFFFFFFFFFFFFFF#, Scalar), ("TEST", 16#FFFFFFFFFFFFFFFF#, Scalar)]); -- r2-local-held
   Run (1, Deref_Completion_Data.Base_13, Deref_Completion_Data.Code_13,
     [("PRAW", 16#FFFFFFFFFFFFFFFF#, Package_Element), ("PDRE", 16#00000000FFFFFFFF#, Scalar), ("PAFT", 16#00000000FFFFFFFF#, Package_Element), ("PRAW", 16#FFFFFFFFFFFFFFFF#, Package_Element), ("PDRE", 16#00000000FFFFFFFF#, Scalar), ("PAFT", 16#00000000FFFFFFFF#, Package_Element)]); -- package-original-mutation
   Run (2, Deref_Completion_Data.Base_14, Deref_Completion_Data.Code_14,
     [("PRAW", 16#FFFFFFFFFFFFFFFF#, Package_Element), ("PDRE", 16#FFFFFFFFFFFFFFFF#, Scalar), ("PAFT", 16#FFFFFFFFFFFFFFFF#, Package_Element), ("PRAW", 16#FFFFFFFFFFFFFFFF#, Package_Element), ("PDRE", 16#FFFFFFFFFFFFFFFF#, Scalar), ("PAFT", 16#FFFFFFFFFFFFFFFF#, Package_Element)]); -- package-original-mutation
   Check (Evaluations = 36, "all frame controls and package before/after observations");
   Ada.Text_IO.Put_Line ("Deref completion checks" & Checks'Image & " observations" & Evaluations'Image);
end Deref_Completion_Tests;
