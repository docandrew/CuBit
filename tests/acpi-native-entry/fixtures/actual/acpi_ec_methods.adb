pragma Ada_2022;
package body ACPI_EC_Methods with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type V.Access_Status, V.Value_Handle;
   use type N.Object_Kind, N.Lookup_Status;
   use type AML_Execute.Execution_Status;
   Hex_Digits : constant String := "0123456789ABCDEF";
   Hex_Radix : constant Event_Number := 16;
   function Method_Name (Number : Event_Number) return AML_Names.Segment is
     (['_', 'Q', Hex_Digits (Natural (Number / Hex_Radix) + 1),
       Hex_Digits (Natural (Number mod Hex_Radix) + 1)]);
   function Is_Bound (Item : Binding) return Boolean is (Item.Bound);
   function Number (Item : Binding) return Event_Number is (Item.Event);
   procedure Resolve
     (Service : Core.State; Scope : Core.Node_Descriptor; Event : Event_Number;
      Item : out Binding; Result : out Resolution_Report) is
      Lookup : N.Lookup_Result;
      Descriptor : Core.Node_Descriptor;
   begin
      Item := No_Binding;
      Result := (Status => Invalid_Scope, others => <>);
      if not Core.Current_Node (Service, Scope)
        or else V.Node_Kind (Scope) /= N.Device_Object
      then
         return;
      end if;
      Core.Find_Object (Service, V.Position (Scope), Method_Name (Event), Lookup,
                        Result.Access_Result);
      if Result.Access_Result /= V.Available or else Lookup.Status not in N.Found | N.Not_Found then
         Result.Status := Lookup_Failed;
         return;
      elsif Lookup.Status = N.Not_Found then
         Result.Status := Missing;
         return;
      end if;
      Core.Describe_Node (Service, Lookup.Node, Descriptor, Result.Access_Result);
      if Result.Access_Result /= V.Available then
         Result.Status := Lookup_Failed;
      elsif not Core.Current_Node (Service, Descriptor)
        or else V.Node_Kind (Descriptor) /= N.Method_Object
      then
         Result.Status := Unsupported_Handler;
      else
         Item := (Bound => True, Scope => Scope, Method => Descriptor, Event => Event);
         Result.Status := ACPI_EC_Methods.Found;
      end if;
   end Resolve;
   function Current (Service : Core.State; Item : Binding) return Boolean is
      Fresh : Binding;
      Report : Resolution_Report;
   begin
      if not Item.Bound then return False; end if;
      Resolve (Service, Item.Scope, Item.Event, Fresh, Report);
      return Report.Status = Found and then Fresh = Item;
   end Current;
   function Consumed (Run : Invocation) return Boolean is (Run.Used);
   function Pending (Run : Invocation) return Boolean is (Run.Handle /= V.No_Value);
   function Observe (Run : Invocation) return Invocation_Report is (Run.Info);
   procedure Release (Run : in out Invocation; Service : in out Core.State) is
   begin
      if not Pending (Run) then return; end if;
      Release_Result (Service, Run.Handle, Run.Info.Cleanup_Result);
      if Run.Info.Cleanup_Result /= V.Available or else Pending (Run) then Run.Info.Status := Cleanup_Failed; end if;
   end Release;
   procedure Invoke
     (Run : in out Invocation; Service : aliased in out Core.State;
      Item : Binding; Budget : Natural; Action : out Action_Result) is
      Value : V.Result;
      procedure Evaluate is
      begin
         if not Current (Service, Item) then Run.Info.Status := Invalid_Binding; return; end if;
         if Budget = 0 then Run.Info.Status := Budget_Exhausted; Run.Info.Execution := AML_Execute.Budget_Exceeded; return; end if;
         Core.Describe_Node (Service, N.Root, Run.Owner, Run.Info.Access_Result);
         if Run.Info.Access_Result /= V.Available then Run.Info.Status := Evaluation_Failed; return; end if;
         Evaluate_Named (Service, Item.Method, Budget, Value, Run.Info.Access_Result);
         Run.Info.Reported_Charge := Value.Charged; Run.Info.Execution := Value.Status;
         if Value.Status = AML_Execute.Fatal_Requested then Run.Info.Fatal := (Present => True, Item => Value.Fatal); end if;
         if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned then
            Run.Handle := Value.Handle;
            if Run.Handle = V.No_Value then Run.Info.Status := Provider_Contract_Error; return; end if;
         end if;
         if Value.Charged > Budget then Run.Info.Status := Provider_Contract_Error; return; end if;
         Run.Info.Charged := Value.Charged;
         if not Current (Service, Item) then Run.Info.Status := Identity_Changed; return; end if;
         if Run.Info.Access_Result /= V.Available then Run.Info.Status := Evaluation_Failed; return; end if;
         case Value.Status is
            when AML_Execute.Returned | AML_Execute.Object_Returned | AML_Execute.Reference_Returned |
                 AML_Execute.Metadata_Returned | AML_Execute.No_Return => Run.Info.Status := Succeeded;
            when AML_Execute.Budget_Exceeded => Run.Info.Status := Budget_Exhausted;
            when AML_Execute.Field_Indeterminate => Run.Info.Status := Indeterminate;
            when others => Run.Info.Status := Evaluation_Failed;
         end case;
      end Evaluate;
   begin
      if Run.Used then Action := Already_Consumed; return; end if;
      Action := Accepted; Run.Used := True; Evaluate; Release (Run, Service);
   end Invoke;
   procedure Close
     (Run : in out Invocation; Service : in out Core.State; Action : out Action_Result) is
   begin
      Action := No_Pending_Result; if not Pending (Run) then return; end if;
      Action := Wrong_Context; if not Core.Current_Binding (Service, Run.Owner) then return; end if;
      Action := Accepted; Release (Run, Service);
      if not Pending (Run) then Run.Info.Status := Closed; end if;
   end Close;
end ACPI_EC_Methods;
