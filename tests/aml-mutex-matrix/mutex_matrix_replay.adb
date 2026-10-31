with Ada.Text_IO;
with AML_Decode;
with ACPI_Service_Core;
with AML_Delays;
with Mutex_Matrix_Data;
procedure Mutex_Matrix_Replay is
   package D renames Mutex_Matrix_Data;
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64);
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Byte;
   use type AML_Execute.Execution_Status;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type D.Expected_Kind;
   Budget : constant := 10_000;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label; end if;
   end Check;
   function Status_Of (Expected : D.Expectation) return AML_Execute.Execution_Status is
     (case Expected.Kind is
         when D.Integer_Result => AML_Execute.Returned,
         when D.Mutex_Order_Error => AML_Execute.Mutex_Order,
         when D.Division_By_Zero_Error => AML_Execute.Division_By_Zero,
         when D.Mutex_Not_Acquired_Error => Not_Acquired_Status);

   procedure Run_Case (Index : D.Case_Index) is
      -- A fresh public Core/owner/catalog for EVERY table, not each observation.
      -- All six original calls retain ordering and state within this case.
      Service : aliased C.State (1, 4096, 4096, 1);
      Stage : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Report : C.Execution_Report;
      Access_Result : C.Values.Access_Status;
      Members : C.Namespace.Initialization_Report;
      Bytes : constant AML_Decode.Bytes := D.Table (Index);
      Label : constant String := D.Case_Name (Index);
      procedure Observe (Position : D.Observation_Index) is
         Name : constant String := D.Method_Name (Position);
         Expected : constant D.Expectation := D.Expected (Index, Position);
         Node : C.Namespace.Node_ID;
         Descriptor : C.Node_Descriptor;
         Value : C.Values.Result;
         Actual : AML_Execute.Execution_Status;
         Number : AML_Decode.Integer_Value := 0;
         Integer_Present : Boolean := False;
         Release_Access : C.Values.Access_Status;
      begin
         C.Find_Child (Service, C.Namespace.Root, Name, Node, Access_Result);
         Check (Access_Result = C.Values.Available and then Node /= C.Namespace.Root, Label & " find " & Name);
         C.Describe_Node (Service, Node, Descriptor, Access_Result);
         Check (Access_Result = C.Values.Available, Label & " describe " & Name);
         C.Evaluate_Named (Service, Descriptor, Budget, Value, Access_Result);
         Check (Access_Result = C.Values.Available, Label & " public evaluation access " & Name);
         Actual := Value.Status;
         Ada.Text_IO.Put_Line (Label & " #" & Position'Image & " " & Name &
           " actual=" & Actual'Image & " expected=" & Status_Of (Expected)'Image &
           " charged=" & Value.Charged'Image);
         -- NEVER inspect a nonmatching discriminant field, even in diagnostics.
         case Actual is
            when AML_Execute.Returned =>
               Number := Value.Number;
               Integer_Present := True;
               Ada.Text_IO.Put_Line ("  integer=" & Number'Image);
            when AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
                 AML_Execute.Metadata_Returned =>
               C.Release_Result (Service, Value.Handle, Release_Access);
               Check (Release_Access = C.Values.Available, Label & " release unexpected retained result");
            when others => null;
         end case;
         Check (Value.Charged <= Budget, Label & " bounded charge " & Name);
         Check (C.Retained_Results (Service) = 0, Label & " retained cleanup " & Name);
         Check (Actual = Status_Of (Expected), Label & " exact semantic status " & Name);
         if Expected.Kind = D.Integer_Result then
            Check (Integer_Present and then Number = Expected.Number, Label & " exact integer " & Name);
         end if;
         Check (C.Valid (Service), Label & " valid after " & Name);
      end Observe;
   begin
      Check (Bytes (Bytes'First + 8) = D.Revision (Index), Label & " original revision");
      C.Stage_Table (Service, 1, C.DSDT, Bytes, Stage);
      Check (Stage = C.Staged, Label & " stage full exact DSDT");
      C.Freeze_Catalog (Service, Frozen);
      Check (Frozen = C.Catalog_Frozen_OK, Label & " freeze");
      -- Core reads DSDT revision to select Bits32/Bits64; no forced width override.
      C.Execute_Frozen (Service, Budget, Report, Access_Result);
      Check (Access_Result = C.Values.Available and then
             Report.Modules.Outcome = C.Values.Modules_Completed, Label & " module admission");
      C.Initialize_Members (Service, Members, Access_Result);
      Check (Access_Result = C.Values.Available, Label & " initialize members");
      for Position in D.Observation_Index loop Observe (Position); end loop;
      Check (C.Retained_Results (Service) = 0, Label & " final retained cleanup");
   end Run_Case;
begin
   -- This required formal cannot collapse the absent semantic category into an
   -- existing success/order/division/unsupported catch-all to manufacture pass.
   Check (Not_Acquired_Status not in AML_Execute.Returned | AML_Execute.Object_Returned |
     AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned | AML_Execute.No_Return |
     AML_Execute.Mutex_Order | AML_Execute.Division_By_Zero | AML_Execute.Unsupported |
     AML_Execute.Unsupported_Value, "distinct exact not-acquired status required");
   for Index in D.Case_Index loop Run_Case (Index); end loop;
   Ada.Text_IO.Put_Line ("Mutex public Core matrix checks:" & Checks'Image);
end Mutex_Matrix_Replay;
