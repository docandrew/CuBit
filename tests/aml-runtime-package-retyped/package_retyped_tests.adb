with Ada.Text_IO;
with ACPI_Service_Core;
with AML_Decode;
with AML_Delays;
with AML_Execute;
with Package_Retyped_Data;
procedure Package_Retyped_Tests is
   package C is new ACPI_Service_Core (Perform_Delay => AML_Delays.Unavailable_Provider);
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Execution_Phase_Result;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   use type C.Values.Value_Kind;
   use type AML_Decode.Bytes;
   Checks : Natural := 0;
   Fuel : constant Positive := 10_000;
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   procedure Run (Label : String; Table : AML_Decode.Bytes;
                  Expected_Type : AML_Decode.Integer_Value) is
      A : aliased C.State (1, Table'Length, Table'Length, 1);
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Report : C.Execution_Report;
      Status : C.Values.Access_Status;
      Init : C.Namespace.Initialization_Report;
      Node : C.Namespace.Node_ID;
      Result : C.Values.Result;
      procedure Invoke (Name : String; Expected_Number : AML_Decode.Integer_Value;
                        Compound : Boolean := False) is
         Nodes : constant Natural := C.Observe (A).Objects;
         Description : C.Values.Value_Description;
         Handle, Child : C.Values.Value_Handle;
         Data : AML_Decode.Bytes (1 .. 3);
         Copied : Natural;
      begin
         C.Find_Child (A, C.Namespace.Root, Name, Node, Status);
         Check (Status = C.Values.Available and then Node /= C.Namespace.Root, Label & " find " & Name);
         C.Invoke_Retained (A, Node, [others => 0], 0, Fuel, Result, Status);
         Check (Status = C.Values.Available, Label & " access " & Name);
         Check (Result.Charged > 0 and then Result.Charged <= Fuel, Label & " charged " & Name);
         if not Compound then
            Check (Result.Status = AML_Execute.Returned, Label & " scalar status " & Result.Status'Image);
            Check (Result.Number = Expected_Number, Label & " scalar value");
         else
            Check (Result.Status = AML_Execute.Object_Returned, Label & " object status " & Result.Status'Image);
            Check (C.Retained_Results (A) = 1, Label & " retained result");
            Handle := Result.Handle;
            C.Describe_Result (A, Handle, Description, Status);
            Check (Status = C.Values.Available, Label & " describe");
            if Expected_Type = 3 then
               Check (Description.Kind = C.Values.Buffer_Description and then Description.Length = 3, Label & " buffer size");
               C.Read_Result_Bytes (A, Handle, 0, Data, Copied, Status);
               Check (Status = C.Values.Available and then Copied = 3 and then Data = [16#A1#, 16#B2#, 16#C3#], Label & " all buffer bytes");
            else
               Check (Description.Kind = C.Values.Package_Description and then Description.Length = 2, Label & " package size");
               for Index in 0 .. 1 loop
                  C.Read_Result_Element (A, Handle, Index, Child, Status);
                  Check (Status = C.Values.Available, Label & " child");
                  C.Describe_Result (A, Child, Description, Status);
                  Check (Status = C.Values.Available and then Description.Kind = C.Values.Integer_Description
                    and then Description.Number = (if Index = 0 then 16#33# else 16#44#), Label & " exact child");
                  C.Release_Result (A, Child, Status);
                  Check (Status = C.Values.Available, Label & " release child");
               end loop;
            end if;
            C.Release_Result (A, Handle, Status);
            Check (Status = C.Values.Available, Label & " release result");
         end if;
         Check (C.Retained_Results (A) = 0, Label & " retained cleanup");
         Check (C.Observe (A).Objects = Nodes, Label & " temporary name cleanup");
      end Invoke;
   begin
      C.Stage_Table (A, 1, C.DSDT, Table, Staged); Check (Staged = C.Staged, Label & " stage");
      C.Freeze_Catalog (A, Frozen); Check (Frozen = C.Catalog_Frozen_OK, Label & " freeze");
      C.Execute_Frozen (A, Fuel, Report, Status);
      Check (Status = C.Values.Available and then Report.Phase = C.Module_Execution_Reported
        and then Report.Modules.Outcome = C.Values.Modules_Completed, Label & " module");
      C.Initialize_Members (A, Init, Status); Check (Status = C.Values.Available, Label & " initialize");
      for Repeat in 1 .. 2 loop
         Invoke ("TEST", 16#55#, Expected_Type /= 1);
         Invoke ("OBS0", 1);
         Invoke ("OBS1", Expected_Type);
      end loop;
   end Run;
begin
   Run ("nestedbuffer-integer-r1", Package_Retyped_Data.nestedbuffer_integer_r1, 1);
   Run ("nestedbuffer-buffer-r1", Package_Retyped_Data.nestedbuffer_buffer_r1, 3);
   Run ("nestedbuffer-package-r1", Package_Retyped_Data.nestedbuffer_package_r1, 4);
   Run ("nestedbuffer-integer-r2", Package_Retyped_Data.nestedbuffer_integer_r2, 1);
   Run ("nestedbuffer-buffer-r2", Package_Retyped_Data.nestedbuffer_buffer_r2, 3);
   Run ("nestedbuffer-package-r2", Package_Retyped_Data.nestedbuffer_package_r2, 4);
   Ada.Text_IO.Put_Line ("Runtime package retyped-name checks" & Checks'Image);
end Package_Retyped_Tests;
