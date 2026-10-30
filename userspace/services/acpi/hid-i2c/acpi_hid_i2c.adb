pragma Ada_2022;
with AML_Decode;
with AML_Names;
with ACPI_Device_Policy;
package body ACPI_HID_I2C with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type V.Access_Status;
   use type V.Value_Handle;
   use type V.Value_Kind;
   use type N.Object_Kind;
   use type N.Lookup_Status;
   use type AML_Execute.Execution_Status;
   use type Interfaces.Unsigned_8;
   STA_Member : constant AML_Names.Segment := "_STA";
   Enabled_Mask : constant Interfaces.Unsigned_64 := 16#02#;
   DSM_Member : constant AML_Names.Segment := "_DSM";
   HID_I2C_UUID : constant AML_Decode.Bytes :=
     [16#F7#, 16#F6#, 16#DF#, 16#3C#, 16#67#, 16#42#, 16#55#, 16#45#,
      16#AD#, 16#05#, 16#B3#, 16#0A#, 16#3D#, 16#89#, 16#38#, 16#DE#];
   Revision : constant AML_Decode.Integer_Value := 1;
   Query_Function : constant AML_Decode.Integer_Value := 0;
   Descriptor_Function : constant AML_Decode.Integer_Value := 1;
   Required_Function_Bits : constant Interfaces.Unsigned_8 := 16#03#;
   DSM_Argument_Count : constant V.Argument_Count := 4;
   CID_Member : constant AML_Names.Segment := "_CID";
   Compatible_ID : constant AML_Decode.Integer_Value := 16#500C_D041#;
   Compatible_Name : constant AML_Decode.Bytes :=
     [16#50#, 16#4E#, 16#50#, 16#30#, 16#43#, 16#35#, 16#30#];
   procedure Observe_Status
     (Service : aliased in out Core.State; Device : Core.Node_Descriptor;
      Budget : Natural; Report : out Status_Report) is
      Before_Roots : constant Natural := Core.Retained_Results (Service);
      Root : V.Value_Handle := V.No_Value;
      Member : Core.Node_Descriptor;
      Found : N.Lookup_Result;
      Value : V.Result;
      procedure Run is
      begin
         if not Core.Current_Node (Service, Device)
           or else V.Node_Kind (Device) /= N.Device_Object then return; end if;
         if Budget = 0 then Report.Outcome := Budget_Exhausted; return; end if;
         Core.Find_Object (Service, V.Position (Device), STA_Member, Found, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Lookup_Failed; return; end if;
         if Found.Status = N.Not_Found then
            Report.Flags := ACPI_Device_Policy.Missing_Status_Default;
            Report.Defaulted := True;
         else
            if Found.Status /= N.Found then Report.Outcome := Lookup_Failed; return; end if;
            Core.Describe_Node (Service, Found.Node, Member, Report.Access_Result);
            if Report.Access_Result /= V.Available then Report.Outcome := Lookup_Failed; return; end if;
            Evaluate_Named (Service, Member, Budget, Value, Report.Access_Result);
            Report.Execution := Value.Status; Report.Last_Charge := Value.Charged;
            if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
              AML_Execute.Metadata_Returned then Root := Value.Handle; end if;
            if Value.Charged > Budget then Report.Outcome := Provider_Contract_Error; return; end if;
            Report.Charged := Value.Charged;
            if not Core.Current_Node (Service, Device) or else not Core.Current_Node (Service, Member) then
               Report.Outcome := Identity_Changed; return;
            end if;
            if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return; end if;
            if Value.Status = AML_Execute.Budget_Exceeded then Report.Outcome := Budget_Exhausted; return; end if;
            if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
              AML_Execute.Metadata_Returned | AML_Execute.No_Return then
               Report.Outcome := Wrong_Result_Type; return;
            elsif Value.Status /= AML_Execute.Returned then
               Report.Outcome := Evaluation_Failed; return;
            end if;
            Report.Flags := Interfaces.Unsigned_64 (Value.Number);
         end if;
         Report.Present := (Report.Flags and ACPI_Device_Policy.Present_Mask) /= 0;
         Report.Enabled := (Report.Flags and Enabled_Mask) /= 0;
         Report.Functioning := (Report.Flags and ACPI_Device_Policy.Functioning_Mask) /= 0;
         Report.Outcome := Status_Available;
      end Run;
   begin
      Report := (others => <>);
      Run;
      if Root /= V.No_Value then
         Core.Release_Result (Service, Root, Report.Cleanup_Result);
         if Report.Cleanup_Result /= V.Available or else Root /= V.No_Value then
            Report.Outcome := Cleanup_Failed;
         end if;
      end if;
      if Report.Outcome /= Cleanup_Failed and then Core.Retained_Results (Service) /= Before_Roots then
         Report.Outcome := Provider_Contract_Error;
      end if;
      if Report.Outcome /= Status_Available then
         Report.Flags := 0; Report.Present := False; Report.Enabled := False;
         Report.Functioning := False; Report.Defaulted := False;
      end if;
   end Observe_Status;
   procedure Identify
     (Service : aliased in out Core.State; Device : Core.Node_Descriptor;
      Budget : Natural; Report : out Identification_Report) is
      use type AML_Decode.Bytes;
      Before_Roots : constant Natural := Core.Retained_Results (Service);
      Root, Element : V.Value_Handle := V.No_Value;
      Cleanup_Error : Boolean := False;
      Member : Core.Node_Descriptor;
      Node : N.Node_ID;
      Found : N.Lookup_Result;
      Value : V.Result;
      Matched : Boolean := False;
      procedure Drop (Handle : in out V.Value_Handle) is
         Status : V.Access_Status;
      begin
         if Handle = V.No_Value then return; end if;
         Core.Release_Result (Service, Handle, Status);
         if Status /= V.Available or else Handle /= V.No_Value then
            Cleanup_Error := True; Report.Cleanup_Result := Status;
         end if;
      end Drop;
      procedure Charge (Accepted : out Boolean) is
      begin
         if Report.Charged = Budget then
            Report.Outcome := Budget_Exhausted; Accepted := False; return;
         end if;
         Report.Charged := Report.Charged + 1;
         Report.IDs_Examined := Report.IDs_Examined + 1;
         Accepted := True;
      end Charge;
      procedure Inspect (Handle : V.Value_Handle; Accepted : out Boolean) is
         Description : V.Value_Description;
         Data : AML_Decode.Bytes (Compatible_Name'Range);
         Copied : Natural;
      begin
         Accepted := False;
         Core.Describe_Result (Service, Handle, Description, Report.Access_Result);
         if Report.Access_Result /= V.Available then
            Report.Outcome := Evaluation_Failed; return;
         end if;
         case Description.Kind is
            when V.Integer_Description =>
               Matched := Matched or else Description.Number = Compatible_ID;
            when V.String_Description =>
               if Description.Length = Compatible_Name'Length then
                  Core.Read_Result_Bytes (Service, Handle, 0, Data, Copied, Report.Access_Result);
                  if Report.Access_Result /= V.Available or else Copied /= Data'Length then
                     Report.Outcome := Evaluation_Failed; return;
                  end if;
                  Matched := Matched or else Data = Compatible_Name;
               end if;
            when others => Report.Outcome := Wrong_Result_Type; return;
         end case;
         Accepted := True;
      end Inspect;
      procedure Run is
         Description : V.Value_Description;
         Accepted : Boolean;
      begin
         if not Core.Current_Node (Service, Device) or else V.Node_Kind (Device) /= N.Device_Object then return; end if;
         if Budget = 0 then Report.Outcome := Budget_Exhausted; return; end if;
         Core.Find_Object (Service, V.Position (Device), CID_Member, Found, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Lookup_Failed; return; end if;
         if Found.Status = N.Not_Found then Report.Outcome := Missing_CID; return; end if;
         if Found.Status /= N.Found then Report.Outcome := Lookup_Failed; return; end if;
         Node := Found.Node;
         Core.Describe_Node (Service, Node, Member, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Lookup_Failed; return; end if;
         Evaluate_Named (Service, Member, Budget, Value, Report.Access_Result);
         Report.Execution := Value.Status; Report.AML_Charged := Value.Charged;
         if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
           AML_Execute.Metadata_Returned
         then Root := Value.Handle; end if;
         if Value.Charged > Budget then Report.Outcome := Provider_Contract_Error; return; end if;
         Report.Charged := Value.Charged;
         if not Core.Current_Node (Service, Device) or else not Core.Current_Node (Service, Member) then
            Report.Outcome := Identity_Changed; return;
         end if;
         if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return; end if;
         if Value.Status = AML_Execute.Budget_Exceeded then Report.Outcome := Budget_Exhausted; return; end if;
         if Value.Status = AML_Execute.Returned then
            Charge (Accepted); if not Accepted then return; end if;
            Matched := Value.Number = Compatible_ID;
         elsif Value.Status = AML_Execute.Object_Returned then
            Core.Describe_Result (Service, Root, Description, Report.Access_Result);
            if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return; end if;
            if Description.Kind = V.Package_Description then
               if Description.Length > 0 then
                  for I in 0 .. Description.Length - 1 loop
                     Charge (Accepted); if not Accepted then return; end if;
                     Core.Read_Result_Element (Service, Root, I, Element, Report.Access_Result);
                     if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return; end if;
                     Inspect (Element, Accepted);
                     Drop (Element);
                     if Cleanup_Error then Report.Outcome := Cleanup_Failed; return; end if;
                     if not Accepted then return; end if;
                  end loop;
               end if;
            else
               Charge (Accepted); if not Accepted then return; end if;
               Inspect (Root, Accepted); if not Accepted then return; end if;
            end if;
         else Report.Outcome := Wrong_Result_Type; return;
         end if;
         Report.Outcome := (if Matched then Compatible else Not_Compatible);
      end Run;
   begin
      Report := (others => <>);
      Run;
      -- Failed releases are not retried; retain their original handle for the
      -- report's root-count check without risking repeated ownership changes.
      if not Cleanup_Error then Drop (Element); end if;
      Drop (Root);
      if Cleanup_Error and then Report.Outcome /= Provider_Contract_Error then
         Report.Outcome := Cleanup_Failed;
      elsif not Cleanup_Error and then Core.Retained_Results (Service) /= Before_Roots then
         Report.Outcome := Provider_Contract_Error;
      end if;
   end Identify;
   procedure Query_Descriptor
     (Service : aliased in out Core.State; Device : Core.Node_Descriptor;
      Budget : Natural; Report : out Query_Report) is
      Before_Roots : constant Natural := Core.Retained_Results (Service);
      UUID, Package_Value, Temporary : V.Value_Handle := V.No_Value;
      UUID_Tried, Package_Tried, Temporary_Tried : Boolean := False;
      Cleanup_Error : Boolean := False;
      Member : Core.Node_Descriptor;
      Node : N.Node_ID;
      Found : N.Lookup_Result;
      Value : V.Result;
      Args : V.Arguments := [others => (V.Immediate_Argument, 0)];
      procedure Drop (Handle : in out V.Value_Handle; Tried : in out Boolean) is
         Released : V.Access_Status;
      begin
         if Tried or else Handle = V.No_Value then return; end if;
         Tried := True;
         Core.Release_Result (Service, Handle, Released);
         if Released /= V.Available or else Handle /= V.No_Value then
            Cleanup_Error := True;
            Report.Cleanup_Result := Released;
         end if;
      end Drop;
      procedure Call (Index : AML_Decode.Integer_Value; Accepted : out Boolean) is
         Remaining : constant Natural := Budget - Report.Charged;
      begin
         Accepted := False;
         if Remaining = 0 then Report.Outcome := Budget_Exhausted; return; end if;
         Args (2) := (V.Immediate_Argument, Index);
         Invoke (Service, Node, Args, DSM_Argument_Count, Remaining, Value, Report.Access_Result);
         Report.Calls := Report.Calls + 1;
         Report.Last_Charge := Value.Charged;
         Report.Execution := Value.Status;
         if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
           AML_Execute.Metadata_Returned
         then Temporary := Value.Handle; Temporary_Tried := False; end if;
         if Value.Charged > Remaining then
            -- Preserve the last accepted total and the offending charge; do
            -- not overflow Natural by adding a broken provider's observation.
            Report.Outcome := Provider_Contract_Error; return;
         end if;
         Report.Charged := Report.Charged + Value.Charged;
         if not Core.Current_Node (Service, Device) or else not Core.Current_Node (Service, Member) then
            Report.Outcome := Identity_Changed;
         elsif Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed;
         elsif Value.Status = AML_Execute.Budget_Exceeded then Report.Outcome := Budget_Exhausted;
         elsif Value.Status not in AML_Execute.Returned | AML_Execute.Object_Returned |
           AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned
         then Report.Outcome := Evaluation_Failed;
         else Accepted := True;
         end if;
      end Call;
      procedure Prepare_Arguments (Accepted : out Boolean) is
      begin
         Accepted := False;
         Core.Create_Buffer_Result (Service, HID_I2C_UUID, Value, Report.Access_Result);
         if Value.Status = AML_Execute.Object_Returned then UUID := Value.Handle; UUID_Tried := False; end if;
         if Report.Access_Result /= V.Available or else Value.Status /= AML_Execute.Object_Returned then
            Report.Outcome := Allocation_Failed; return;
         end if;
         Core.Create_Empty_Package_Result (Service, Value, Report.Access_Result);
         if Value.Status = AML_Execute.Object_Returned then Package_Value := Value.Handle; Package_Tried := False; end if;
         if Report.Access_Result /= V.Available or else Value.Status /= AML_Execute.Object_Returned then
            Report.Outcome := Allocation_Failed; return;
         end if;
         Args (0) := (V.Retained_Argument, UUID);
         Args (1) := (V.Immediate_Argument, Revision);
         Args (3) := (V.Retained_Argument, Package_Value);
         Accepted := True;
      end Prepare_Arguments;
      procedure Run is
         Accepted : Boolean;
         Description : V.Value_Description;
         Mask : AML_Decode.Bytes (1 .. 1);
         Copied : Natural;
      begin
         if not Core.Current_Node (Service, Device) or else V.Node_Kind (Device) /= N.Device_Object then return; end if;
         if Budget = 0 then Report.Outcome := Budget_Exhausted; return; end if;
         Core.Find_Object (Service, V.Position (Device), DSM_Member, Found, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Lookup_Failed; return; end if;
         if Found.Status = N.Not_Found then Report.Outcome := Missing_DSM; return; end if;
         if Found.Status /= N.Found then Report.Outcome := Lookup_Failed; return; end if;
         Node := Found.Node;
         Core.Describe_Node (Service, Node, Member, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Lookup_Failed; return; end if;
         if V.Node_Kind (Member) /= N.Method_Object then Report.Outcome := Not_A_Method; return; end if;
         Prepare_Arguments (Accepted); if not Accepted then return; end if;
         Call (Query_Function, Accepted); if not Accepted then return; end if;
         if Value.Status /= AML_Execute.Object_Returned then Report.Outcome := Wrong_Result_Type; return; end if;
         Core.Describe_Result (Service, Temporary, Description, Report.Access_Result);
         if Report.Access_Result /= V.Available then Report.Outcome := Evaluation_Failed; return; end if;
         if Description.Kind /= V.Buffer_Description then Report.Outcome := Wrong_Result_Type; return; end if;
         if Description.Length = 0 then Report.Outcome := Function_Unsupported; return; end if;
         Core.Read_Result_Bytes (Service, Temporary, 0, Mask, Copied, Report.Access_Result);
         if Report.Access_Result /= V.Available or else Copied /= Mask'Length then
            Report.Outcome := Evaluation_Failed; return;
         end if;
         Report.Functions := Mask (1);
         if (Report.Functions and Required_Function_Bits) /= Required_Function_Bits then
            Report.Outcome := Function_Unsupported; return;
         end if;
         Drop (Temporary, Temporary_Tried);
         Drop (Package_Value, Package_Tried);
         Drop (UUID, UUID_Tried);
         if Cleanup_Error then Report.Outcome := Cleanup_Failed; return; end if;
         if Report.Charged = Budget then Report.Outcome := Budget_Exhausted; return; end if;
         -- AML arguments are mutable. Each protocol call gets a fresh UUID and
         -- package so the first method cannot change the second request.
         Prepare_Arguments (Accepted); if not Accepted then return; end if;
         Call (Descriptor_Function, Accepted); if not Accepted then return; end if;
         if Value.Status /= AML_Execute.Returned then Report.Outcome := Wrong_Result_Type; return; end if;
         if Value.Number > AML_Decode.Integer_Value (Descriptor_Register'Last) then
            Report.Outcome := Invalid_Register; return;
         end if;
         Report.Address := Descriptor_Register (Value.Number);
         Report.Outcome := Descriptor_Available;
      end Run;
   begin
      Report := (others => <>);
      Run;
      Drop (Temporary, Temporary_Tried);
      Drop (Package_Value, Package_Tried);
      Drop (UUID, UUID_Tried);
      if Cleanup_Error and then Report.Outcome /= Provider_Contract_Error then
         Report.Outcome := Cleanup_Failed;
      elsif not Cleanup_Error and then Core.Retained_Results (Service) /= Before_Roots then
         Report.Outcome := Provider_Contract_Error;
      end if;
      if Report.Outcome /= Descriptor_Available then Report.Address := 0; end if;
   end Query_Descriptor;
end ACPI_HID_I2C;
