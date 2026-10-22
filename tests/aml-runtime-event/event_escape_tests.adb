with Ada.Text_IO;
with ACPI_Service_Core;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Integers;
with AML_Execute;
with Event_Oracle_Data;
procedure Event_Escape_Tests is
   package S is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 64);
   use type AML_Decode.Integer_Value;
   use type S.Stage_Status;
   use type S.Freeze_Status;
   use type S.Execution_Phase_Result;
   use type S.Values.Access_Status;
   use type S.Values.Module_Outcome;
   use type S.Values.Value_Kind;
   use type AML_Execute.Execution_Status;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   procedure Run (Table : Bytes; Width : Integer_Width) is
      Service : aliased S.State (1, 4096, 4096);
      Stage : S.Stage_Status;
      Frozen : S.Freeze_Status;
      Report : S.Execution_Report;
      Status : S.Values.Access_Status;
      Initialized : S.Namespace.Initialization_Report;
      Node : S.Namespace.Node_ID;
      Result : S.Values.Result;
      Description : S.Values.Value_Description;
      Element, Saved, Stale : S.Values.Value_Handle;
      Timeout_Value : constant Integer_Value := AML_Integers.Normalize (Integer_Value'Last, Width);
      procedure Invoke (Name : String) is
      begin
         S.Find_Child (Service, S.Namespace.Root, Name, Node, Status);
         Check (Status = S.Values.Available and then Node /= S.Namespace.Root, "find " & Name);
         S.Invoke_Retained (Service, Node, [others => 0], 0, 10000, Result, Status);
         Check (Status = S.Values.Available, "invoke " & Name);
      end Invoke;
      procedure Release_Result is
      begin
         S.Release_Result (Service, Result.Handle, Status);
         Check (Status = S.Values.Available, "release result");
      end Release_Result;
   begin
      S.Stage_Table (Service, 1, S.DSDT, Table, Stage);
      Check (Stage = S.Staged, "stage");
      S.Freeze_Catalog (Service, Frozen); Check (Frozen = S.Catalog_Frozen_OK, "freeze");
      S.Execute_Frozen (Service, 10000, Report, Status);
      Check (Status = S.Values.Available and then Report.Phase = S.Module_Execution_Reported
        and then Report.Modules.Outcome = S.Values.Modules_Completed, "module");
      S.Initialize_Members (Service, Initialized, Status);
      Check (Status = S.Values.Available, "initialize");
      Invoke ("MAKE"); Check (Result.Status = AML_Execute.Object_Returned, "escaped event result");
      Saved := Result.Handle;
      S.Describe_Result (Service, Saved, Description, Status);
      Check (Status = S.Values.Available and then Description.Kind = S.Values.Event_Description, "retained event kind");
      for Repeat in 1 .. 2 loop
         Invoke ("TEST"); Check (Result.Status = AML_Execute.Returned and then Result.Number = 0, "direct argument escape");
         Invoke ("COPY"); Check (Result.Status = AML_Execute.Returned and then Result.Number = 0, "copied escape");
         for Case_ID in 1 .. 2 loop
            Invoke ((if Case_ID = 1 then "DIST" else "INDE"));
            Check (Result.Status = AML_Execute.Object_Returned, "pair package");
            S.Describe_Result (Service, Result.Handle, Description, Status);
            Check (Status = S.Values.Available and then Description.Kind = S.Values.Package_Description
              and then Description.Length = 2, "pair shape");
            for Index in 0 .. 1 loop
               S.Read_Result_Element (Service, Result.Handle, Index, Element, Status);
               Check (Status = S.Values.Available, "pair element");
               S.Describe_Result (Service, Element, Description, Status);
               Check (Status = S.Values.Available and then Description.Kind = S.Values.Integer_Description
                 and then Description.Number = (if Index = 0 then Timeout_Value else 0), "exact oracle pair");
               S.Release_Result (Service, Element, Status); Check (Status = S.Values.Available, "release element");
            end loop;
            Release_Result;
         end loop;
         Invoke ("REST"); Check (Result.Status = AML_Execute.Returned and then Result.Number = Timeout_Value, "reset drains");
         S.Describe_Result (Service, Saved, Description, Status);
         Check (Status = S.Values.Available and then Description.Kind = S.Values.Event_Description, "old event survives same-name invocations");
         Check (S.Retained_Results (Service) = 1, "only saved event retained");
      end loop;
      Stale := Saved;
      S.Release_Result (Service, Saved, Status); Check (Status = S.Values.Available, "release saved event");
      S.Describe_Result (Service, Stale, Description, Status);
      Check (Status /= S.Values.Available, "released event handle rejected");
      Check (S.Retained_Results (Service) = 0, "no retained roots");
   end Run;
begin
   Run (Event_Oracle_Data.Revision_1, Bits_32);
   Run (Event_Oracle_Data.Revision_2, Bits_64);
   Ada.Text_IO.Put_Line ("Event escape checks" & Checks'Image);
end Event_Escape_Tests;
