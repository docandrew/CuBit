pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service_Core;
with AML_Delays;
with AML_Execute;
with Osi_Test_Support; use Osi_Test_Support;
with Osi_Store_Data;
procedure Store_Oracle_Tests is
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
   Checks : Natural := 0;
   Evaluations : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Message; end if; end Check;
   type Expected_Result is record
      Name : String (1 .. 4);
      Number : Unsigned_64;
   end record;
   type Expectations is array (Positive range <>) of Expected_Result;
   procedure Run (Revision : Positive; First, Second : Bytes; Expected : Expectations) is
      Service : aliased S.State (2, 8192, 4096);
      Stage : S.Stage_Status;
      Frozen : S.Freeze_Status;
      Report : S.Execution_Report;
      Status : S.Values.Access_Status;
      Initialized : S.Namespace.Initialization_Report;
      Node : S.Namespace.Node_ID;
      Result : AML_Execute.Execution_Result;
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
         S.Invoke_Scalar (Service, Node, [others => 0], 0, 10000, Result);
         Check (Result.Status = AML_Execute.Returned, "missing scalar " & Item.Name & " " & Result.Status'Image);
         Check (Result.Charged <= 10000 and then Result.Value = Item.Number,
           "revision" & Revision'Image & " " & Item.Name & " expected" & Item.Number'Image & " actual" & Result.Value'Image);
         Check (S.Retained_Results (Service) = 0, "retained result leak");
         Evaluations := Evaluations + 1;
      end loop;
   end Run;
begin
   -- These are ACPICA observations, not a normative assertion that every
   -- reference-count implementation must expose the same internal objects.
   -- In revision 1, SRC0 and DST0 deliberately distinguish mutation of the
   -- shared source from its copied target. DRET/PASS preserve raw64 controls.
   -- RSLT's zero is the observed exact DerefOf fixture, not a general rule.
   Run (1, Osi_Store_Data.Revision_1_base, Osi_Store_Data.Revision_1_source,
     [("DRET", 16#FFFFFFFFFFFFFFFF#),
      ("LOCL", 16#00000000FFFFFFFF#),
      ("LEXP", 16#00000000FFFFFFFF#),
      ("ARGT", 16#00000000FFFFFFFF#),
      ("AEXP", 16#00000000FFFFFFFF#),
      ("RSLT", 16#0000000000000000#),
      ("RARG", 16#FFFFFFFFFFFFFFFF#),
      ("NMST", 16#00000000FFFFFFFF#),
      ("NEXP", 16#00000000FFFFFFFF#),
      ("PIND", 16#00000000FFFFFFFF#),
      ("SRC0", 16#00000000FFFFFFFF#),
      ("DST0", 16#FFFFFFFFFFFFFFFF#),
      ("PASS", 16#FFFFFFFFFFFFFFFF#)]);
   Run (2, Osi_Store_Data.Revision_2_base, Osi_Store_Data.Revision_2_source,
     [("DRET", 16#FFFFFFFFFFFFFFFF#),
      ("LOCL", 16#FFFFFFFFFFFFFFFF#),
      ("LEXP", 16#FFFFFFFFFFFFFFFF#),
      ("ARGT", 16#FFFFFFFFFFFFFFFF#),
      ("AEXP", 16#FFFFFFFFFFFFFFFF#),
      ("RSLT", 16#0000000000000000#),
      ("RARG", 16#FFFFFFFFFFFFFFFF#),
      ("NMST", 16#FFFFFFFFFFFFFFFF#),
      ("NEXP", 16#FFFFFFFFFFFFFFFF#),
      ("PIND", 16#FFFFFFFFFFFFFFFF#),
      ("SRC0", 16#FFFFFFFFFFFFFFFF#),
      ("DST0", 16#FFFFFFFFFFFFFFFF#),
      ("PASS", 16#FFFFFFFFFFFFFFFF#)]);
   Check (Evaluations = 26, "all fixed oracle evaluations visited");
   Ada.Text_IO.Put_Line ("OSI store oracle checks" & Checks'Image & " evaluations" & Evaluations'Image);
end Store_Oracle_Tests;
