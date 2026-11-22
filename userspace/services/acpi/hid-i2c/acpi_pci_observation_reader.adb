pragma Ada_2022;
with AML_Names;
with ACPI_Evaluation_Budgets;
package body ACPI_PCI_Observation_Reader with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type V.Access_Status;
   use type V.Value_Kind, AML_Decode.Byte;
   use type N.Object_Kind, N.Lookup_Status, AML_Execute.Execution_Status;
   Names : constant array (Member) of AML_Names.Segment :=
     [Segment => "_SEG", Base_Bus => "_BBN", Address => "_ADR"];
   procedure Read (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Selected : Member; Budget : Natural;
      Result : out Report) is
      Found : N.Lookup_Result;
      Item : Core.Node_Descriptor;
      Value : V.Result;
      Handle : V.Value_Handle;
   begin
      Result := (others => <>);
      if not Core.Current_Node (Service, Device)
        or else V.Node_Kind (Device) /= N.Device_Object then return; end if;
      if Budget = 0 then
         Result.Status := Budget_Exhausted;
         Result.Execution := AML_Execute.Budget_Exceeded; return;
      end if;
      Core.Find_Object (Service, V.Position (Device), Names (Selected), Found, Result.Access_Result);
      if Result.Access_Result /= V.Available then Result.Status := Lookup_Failed; return;
      elsif Found.Status = N.Not_Found then Result.Status := Absent; return;
      elsif Found.Status /= N.Found then Result.Status := Lookup_Failed; return; end if;
      Core.Describe_Node (Service, Found.Node, Item, Result.Access_Result);
      if Result.Access_Result /= V.Available then Result.Status := Lookup_Failed; return; end if;
      Evaluate_Named (Service, Item, Budget, Value, Result.Access_Result);
      Result.Charged := Value.Charged;
      Result.Execution := Value.Status;
      Result.Recovery_Needed := Value.Status in AML_Execute.Fatal_Requested | AML_Execute.Field_Indeterminate;
      if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
        AML_Execute.Metadata_Returned
      then
         Handle := Value.Handle;
         Core.Release_Result (Service, Handle, Result.Cleanup_Result);
         if Result.Cleanup_Result /= V.Available or else Handle /= V.No_Value then
            Result.Pending := Handle;
            Result.Recovery_Needed := True;
            Result.Status := (if Result.Charged > Budget then Provider_Contract_Error else Cleanup_Failed);
            return;
         end if;
      end if;
      if Result.Charged > Budget then Result.Status := Provider_Contract_Error; Result.Recovery_Needed := True;
      elsif not Core.Current_Node (Service, Device) or else not Core.Current_Node (Service, Item) then
         Result.Status := Identity_Changed;
      elsif Result.Access_Result /= V.Available then Result.Status := Evaluation_Failed;
      elsif Value.Status = AML_Execute.Budget_Exceeded then Result.Status := Budget_Exhausted;
      elsif Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
        AML_Execute.Metadata_Returned then Result.Status := Wrong_Result_Type;
      elsif Value.Status /= AML_Execute.Returned then Result.Status := Evaluation_Failed;
      else Result.Status := Observed; Result.Raw := Value.Number;
      end if;
   end Read;
   procedure Identify_Root (Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Result : out Root_Report) is
      use type AML_Decode.Bytes;
      PCI_ID : constant AML_Decode.Integer_Value := 16#030A_D041#;
      PCI_Express_ID : constant AML_Decode.Integer_Value := 16#080A_D041#;
      PCI_Name : constant AML_Decode.Bytes := [16#50#,16#4E#,16#50#,16#30#,16#41#,16#30#,16#33#];
      PCI_Express_Name : constant AML_Decode.Bytes := [16#50#,16#4E#,16#50#,16#30#,16#41#,16#30#,16#38#];
      CID_Element_Charge : constant Positive := 1;
      Before_Roots : constant Natural := Core.Retained_Results (Service);
      Fuel : ACPI_Evaluation_Budgets.State (Budget);
      Found : N.Lookup_Result;
      Item : Core.Node_Descriptor;
      Value : V.Result;
      Root, Element, Resolved : V.Value_Handle := V.No_Value;
      Root_Tried, Element_Tried, Resolved_Tried, Cleanup_Error : Boolean := False;
      Match : Root_Kind := Unknown;
      function Integer_ID (Number : AML_Decode.Integer_Value) return Root_Kind is
        (if Number = PCI_ID then PCI elsif Number = PCI_Express_ID then PCI_Express else Unknown);
      procedure Drop (Handle : in out V.Value_Handle; Tried : in out Boolean) is
         Status : V.Access_Status;
      begin
         if Tried or else Handle = V.No_Value then return; end if;
         Tried := True;
         Core.Release_Result (Service, Handle, Status);
         if Status /= V.Available or else Handle /= V.No_Value then
            Result.Cleanup_Result := Status; Cleanup_Error := True;
         end if;
      end Drop;
      procedure Charge_Inspection (Accepted : out Boolean) is
      begin
         ACPI_Evaluation_Budgets.Charge (Fuel, CID_Element_Charge, Accepted);
         Result.Charged := ACPI_Evaluation_Budgets.Spent (Fuel);
         if not Accepted then Result.Status := Budget_Exhausted; end if;
      end Charge_Inspection;
      procedure Scalar_ID (Handle : V.Value_Handle; Description : V.Value_Description) is
         Text : AML_Decode.Bytes (PCI_Name'Range);
         Terminator : AML_Decode.Bytes (1 .. 1);
         Copied : Natural;
      begin
         Match := Unknown;
         if Description.Kind = V.Integer_Description then
            Match := Integer_ID (Description.Number);
         elsif Description.Kind = V.String_Description
           and then Description.Length in PCI_Name'Length .. PCI_Name'Length + 1 then
            Core.Read_Result_Bytes (Service, Handle, 0, Text, Copied, Result.Access_Result);
            if Result.Access_Result /= V.Available or else Copied /= Text'Length then
               Result.Status := Evaluation_Failed; return;
            end if;
            Match := (if Text = PCI_Name then PCI elsif Text = PCI_Express_Name then PCI_Express else Unknown);
            if Description.Length = PCI_Name'Length + 1 then
               Core.Read_Result_Bytes (Service, Handle, PCI_Name'Length, Terminator, Copied, Result.Access_Result);
               if Result.Access_Result /= V.Available or else Copied /= Terminator'Length then
                  Result.Status := Evaluation_Failed;
               elsif Terminator (1) /= 0 then Match := Unknown; end if;
            end if;
         end if;
      end Scalar_ID;
      procedure Inspect (Allow_Package : Boolean) is
         Description : V.Value_Description;
         Elements : Natural;
         Accepted : Boolean;
      begin
         if Value.Status = AML_Execute.Returned then
            Match := Integer_ID (Value.Number); Result.Status := Observed; return;
         end if;
         Core.Describe_Result (Service, Root, Description, Result.Access_Result);
         if Result.Access_Result /= V.Available then Result.Status := Evaluation_Failed; return; end if;
         if Description.Kind in V.String_Description | V.Integer_Description then
            Result.Status := Observed; Scalar_ID (Root, Description); return;
         elsif not Allow_Package or else Description.Kind /= V.Package_Description then
            Result.Status := Wrong_Result_Type; return;
         end if;
         Result.Status := Observed; Elements := Description.Length;
         if Elements = 0 then return; end if;
         for Index in 0 .. Elements - 1 loop
            Charge_Inspection (Accepted); if not Accepted then return; end if;
            Core.Read_Result_Element (Service, Root, Index, Element, Result.Access_Result);
            Element_Tried := False;
            if Result.Access_Result /= V.Uninitialized_Element then
               if Result.Access_Result /= V.Available then Result.Status := Evaluation_Failed; return; end if;
               loop
                  Core.Describe_Result (Service, Element, Description, Result.Access_Result);
                  if Result.Access_Result /= V.Available then Result.Status := Evaluation_Failed; return; end if;
                  exit when Description.Kind /= V.Reference_Description;
                  Charge_Inspection (Accepted); if not Accepted then return; end if;
                  Core.Dereference_Result (Service, Element, Resolved, Result.Access_Result);
                  Resolved_Tried := False;
                  if Result.Access_Result /= V.Available then Result.Status := Evaluation_Failed; return; end if;
                  Drop (Element, Element_Tried);
                  if Cleanup_Error then return; end if;
                  Element := Resolved; Resolved := V.No_Value; Element_Tried := False;
               end loop;
               Scalar_ID (Element, Description);
               Drop (Element, Element_Tried);
               if Cleanup_Error or else Result.Status /= Observed then return; end if;
               exit when Match /= Unknown;
            end if;
         end loop;
      end Inspect;
      procedure Observe_Member (Name : AML_Names.Segment; Allow_Package : Boolean) is
         Accepted : Boolean;
      begin
         Match := Unknown; Result.Status := Evaluation_Failed;
         if ACPI_Evaluation_Budgets.Remaining (Fuel) = 0 then Result.Status := Budget_Exhausted; return; end if;
         Core.Find_Object (Service, V.Position (Device), Name, Found, Result.Access_Result);
         if Result.Access_Result /= V.Available then Result.Status := Lookup_Failed; return;
         elsif Found.Status = N.Not_Found then Result.Status := Absent; return;
         elsif Found.Status /= N.Found then Result.Status := Lookup_Failed; return; end if;
         Core.Describe_Node (Service, Found.Node, Item, Result.Access_Result);
         if Result.Access_Result /= V.Available then Result.Status := Lookup_Failed; return; end if;
         Evaluate_Named (Service, Item, ACPI_Evaluation_Budgets.Remaining (Fuel), Value, Result.Access_Result);
         Result.Execution := Value.Status;
         if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned then
            Root := Value.Handle; Root_Tried := False;
         end if;
         ACPI_Evaluation_Budgets.Charge (Fuel, Value.Charged, Accepted);
         Result.Charged := ACPI_Evaluation_Budgets.Spent (Fuel);
         if not Accepted then
            Result.Status := Provider_Contract_Error; Result.Recovery_Needed := True; return;
         end if;
         if Value.Status in AML_Execute.Fatal_Requested | AML_Execute.Field_Indeterminate then
            Result.Recovery_Needed := True;
         end if;
         if not Core.Current_Node (Service, Device) or else not Core.Current_Node (Service, Item) then
            Result.Status := Identity_Changed;
         elsif Result.Access_Result /= V.Available then Result.Status := Evaluation_Failed;
         elsif Value.Status = AML_Execute.Budget_Exceeded then Result.Status := Budget_Exhausted;
         elsif Value.Status in AML_Execute.Returned | AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
             AML_Execute.Metadata_Returned then Inspect (Allow_Package);
         else Result.Status := Evaluation_Failed; end if;
      end Observe_Member;
   begin
      Result := (others => <>);
      if not Core.Current_Node (Service, Device) or else V.Node_Kind (Device) /= N.Device_Object then return; end if;
      Observe_Member ("_HID", False);
      Drop (Root, Root_Tried);
      if Result.Status in Observed | Absent and then Match = Unknown and then not Cleanup_Error and then not Result.Recovery_Needed then
         Observe_Member ("_CID", True);
         if Result.Status = Absent then Result.Status := Observed; end if;
      end if;
      Drop (Resolved, Resolved_Tried); Drop (Element, Element_Tried); Drop (Root, Root_Tried);
      Result.Pending := Root; Result.Pending_Element := Element; Result.Pending_Dereference := Resolved;
      if Cleanup_Error then Result.Status := Cleanup_Failed; Result.Recovery_Needed := True;
      elsif Core.Retained_Results (Service) /= Before_Roots then
         Result.Status := Provider_Contract_Error; Result.Recovery_Needed := True;
      end if;
      if Result.Status = Observed then Result.Kind := Match; end if;
   end Identify_Root;
   procedure Observe_Controller (Service : aliased in out Core.State;
      Controller : Core.Node_Descriptor; Budget : Natural;
      Result : out Topology_Report) is
      Parent : Core.Node_Descriptor;
      Fuel : ACPI_Evaluation_Budgets.State (Budget);
      Access_Result : V.Access_Status;
      Missing, Missing_Segment : Boolean := False;
      Segment_Node : N.Node_ID;
      Segment_Lookup : V.Access_Status;
      function Current return Boolean is
        (Core.Current_Node (Service, Controller)
         and then Core.Current_Node (Service, Parent));
      procedure Charge (Amount : Natural; Accepted : out Boolean) is
      begin
         ACPI_Evaluation_Budgets.Charge (Fuel, Amount, Accepted);
         Result.Charged := ACPI_Evaluation_Budgets.Spent (Fuel);
         if not Accepted then
            -- Do not add an untrusted provider charge: even Natural'Last
            -- cannot overflow or replenish the caller's remaining budget.
            Result.Provider_Charge_Refused := True;
            Result.Recovery_Needed := True;
            Result.Status := Topology_Recovery_Required;
         end if;
      end Charge;
      Accepted : Boolean;
   begin
      Result := (others => <>);
      if not Core.Current_Node (Service, Controller)
        or else V.Node_Kind (Controller) /= N.Device_Object then return; end if;
      Result.Controller := Controller;
      if Budget = 0 then Result.Status := Topology_Budget_Exhausted; return; end if;
      Result.Status := Parent_Unresolved;
      if V.Parent_Position (Controller) = N.Root then return; end if;
      Core.Describe_Node (Service, V.Parent_Position (Controller), Parent, Access_Result);
      if Access_Result /= V.Available or else not Core.Current_Node (Service, Parent)
        or else V.Node_Kind (Parent) /= N.Device_Object then return; end if;
      Result.Host_Bridge := Parent;
      Identify_Root (Service, Parent, Budget, Result.Identity);
      Charge (Result.Identity.Charged, Accepted);
      if not Accepted then return; end if;
      if Result.Identity.Recovery_Needed then
         Result.Recovery_Needed := True; Result.Status := Topology_Recovery_Required; return;
      elsif not Current then Result.Status := Topology_Identity_Changed; return;
      elsif Result.Identity.Status = Budget_Exhausted then
         Result.Status := Topology_Budget_Exhausted; return;
      elsif Result.Identity.Status /= Observed or else Result.Identity.Kind = Unknown then
         Result.Status := Root_Unresolved; return;
      end if;
      for Selected in Member loop
         Read (Service, (if Selected = Address then Controller else Parent),
           Selected, ACPI_Evaluation_Budgets.Remaining (Fuel), Result.Members (Selected));
         Charge (Result.Members (Selected).Charged, Accepted);
         if not Accepted then return; end if;
         if Result.Members (Selected).Recovery_Needed then
            Result.Recovery_Needed := True; Result.Status := Topology_Recovery_Required; return;
         elsif not Current then Result.Status := Topology_Identity_Changed; return;
         end if;
         case Result.Members (Selected).Status is
            when Observed => null;
            when Absent =>
               if Selected = Segment then
                  -- Canonical lookup also reports unresolved aliases as absent.
                  -- The standard default requires the name itself to be absent.
                  Core.Find_Child (Service, V.Position (Parent), Names (Segment), Segment_Node, Segment_Lookup);
                  if Segment_Lookup /= V.Available or else Segment_Node /= N.Root then
                     Result.Status := Topology_Observation_Failed; return;
                  end if;
                  Missing_Segment := True;
               else Missing := True; end if;
            when Budget_Exhausted => Result.Status := Topology_Budget_Exhausted; return;
            when others => Result.Status := Topology_Observation_Failed; return;
         end case;
      end loop;
      Result.Status := (if Missing then Topology_Incomplete else Topology_Observed);
      Result.Segment_Defaulted := not Missing and then Missing_Segment;
   end Observe_Controller;
end ACPI_PCI_Observation_Reader;
