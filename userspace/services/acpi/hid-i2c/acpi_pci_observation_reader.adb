pragma Ada_2022;
with AML_Names;
package body ACPI_PCI_Observation_Reader with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type V.Access_Status, V.Value_Handle;
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
      Found : N.Lookup_Result;
      Item : Core.Node_Descriptor;
      Value : V.Result;
      Handle : V.Value_Handle := V.No_Value;
      Description : V.Value_Description;
      Text : AML_Decode.Bytes (PCI_Name'Range);
      Terminator : AML_Decode.Bytes (1 .. 1);
      Copied : Natural;
      Match : Root_Kind := Unknown;
   begin
      Result := (others => <>);
      if not Core.Current_Node (Service, Device) or else V.Node_Kind (Device) /= N.Device_Object then return; end if;
      if Budget = 0 then Result.Status := Budget_Exhausted; Result.Execution := AML_Execute.Budget_Exceeded; return; end if;
      Core.Find_Object (Service, V.Position (Device), "_HID", Found, Result.Access_Result);
      if Result.Access_Result /= V.Available then Result.Status := Lookup_Failed; return;
      elsif Found.Status = N.Not_Found then Result.Status := Absent; return;
      elsif Found.Status /= N.Found then Result.Status := Lookup_Failed; return; end if;
      Core.Describe_Node (Service, Found.Node, Item, Result.Access_Result);
      if Result.Access_Result /= V.Available then Result.Status := Lookup_Failed; return; end if;
      Evaluate_Named (Service, Item, Budget, Value, Result.Access_Result);
      Result.Charged := Value.Charged; Result.Execution := Value.Status;
      Result.Recovery_Needed := Value.Status in AML_Execute.Fatal_Requested | AML_Execute.Field_Indeterminate;
      Result.Status := Evaluation_Failed;
      if Result.Access_Result = V.Available and then Value.Status = AML_Execute.Returned then
         Match := (if Value.Number = PCI_ID then PCI elsif Value.Number = PCI_Express_ID then PCI_Express else Unknown);
         Result.Status := Observed;
      elsif Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned then
         Handle := Value.Handle;
         if Result.Access_Result = V.Available then
            Core.Describe_Result (Service, Handle, Description, Result.Access_Result);
            if Result.Access_Result = V.Available then
               Result.Status := Wrong_Result_Type;
               if Description.Kind = V.String_Description then
                  Result.Status := Observed;
                  if Description.Length in PCI_Name'Length .. PCI_Name'Length + 1 then
                     Core.Read_Result_Bytes (Service, Handle, 0, Text, Copied, Result.Access_Result);
                     if Result.Access_Result /= V.Available or else Copied /= Text'Length then Result.Status := Evaluation_Failed;
                     else
                        Match := (if Text = PCI_Name then PCI elsif Text = PCI_Express_Name then PCI_Express else Unknown);
                        if Description.Length = PCI_Name'Length + 1 then
                           Core.Read_Result_Bytes (Service, Handle, PCI_Name'Length, Terminator, Copied, Result.Access_Result);
                           if Result.Access_Result /= V.Available or else Copied /= 1 then Result.Status := Evaluation_Failed;
                           elsif Terminator (1) /= 0 then Match := Unknown; end if;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;
         Core.Release_Result (Service, Handle, Result.Cleanup_Result);
         if Result.Cleanup_Result /= V.Available or else Handle /= V.No_Value then
            Result.Pending := Handle; Result.Recovery_Needed := True;
            Result.Status := (if Result.Charged > Budget then Provider_Contract_Error else Cleanup_Failed); return;
         end if;
      elsif Value.Status = AML_Execute.Budget_Exceeded then Result.Status := Budget_Exhausted;
      end if;
      if Result.Charged > Budget then Result.Status := Provider_Contract_Error; Result.Recovery_Needed := True;
      elsif not Core.Current_Node (Service, Device) or else not Core.Current_Node (Service, Item) then Result.Status := Identity_Changed;
      end if;
      if Result.Status = Observed then Result.Kind := Match; end if;
   end Identify_Root;
   procedure Observe_Controller (Service : aliased in out Core.State;
      Controller : Core.Node_Descriptor; Budget : Natural;
      Result : out Topology_Report) is
      Parent : Core.Node_Descriptor;
      Access_Result : V.Access_Status;
      Missing : Boolean := False;
      function Current return Boolean is
        (Core.Current_Node (Service, Controller)
         and then Core.Current_Node (Service, Parent));
      procedure Charge (Amount : Natural; Accepted : out Boolean) is
      begin
         Accepted := Amount <= Budget - Result.Charged;
         if Accepted then
            Result.Charged := Result.Charged + Amount;
         else
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
           Selected, Budget - Result.Charged, Result.Members (Selected));
         Charge (Result.Members (Selected).Charged, Accepted);
         if not Accepted then return; end if;
         if Result.Members (Selected).Recovery_Needed then
            Result.Recovery_Needed := True; Result.Status := Topology_Recovery_Required; return;
         elsif not Current then Result.Status := Topology_Identity_Changed; return;
         end if;
         case Result.Members (Selected).Status is
            when Observed => null;
            when Absent => Missing := True;
            when Budget_Exhausted => Result.Status := Topology_Budget_Exhausted; return;
            when others => Result.Status := Topology_Observation_Failed; return;
         end case;
      end loop;
      Result.Status := (if Missing then Topology_Incomplete else Topology_Observed);
   end Observe_Controller;
end ACPI_PCI_Observation_Reader;
