pragma Ada_2022;
with ACPI_Evaluation_Budgets;
package body ACPI_Host_Bridge_Query with SPARK_Mode is
   package V renames Core.Values;
   package N renames Core.Namespace;
   use type V.Value_Handle, V.Access_Status;
   use type N.Lookup_Status, AML_Execute.Execution_Status;
   use type Roots.Outcome, Roots.Root_Kind, Reader.Copy_Status;
   function Observe (Run : Session) return Report is (Run.Info);
   function Consumed (Run : Session) return Boolean is (Run.Started);
   function Pending (Run : Session) return Boolean is
     (Run.Handle /= V.No_Value or else Run.Info.Identity.Pending /= V.No_Value
      or else Run.Info.Identity.Pending_Element /= V.No_Value
      or else Run.Info.Identity.Pending_Dereference /= V.No_Value
      or else Run.Info.Segment.Pending /= V.No_Value
      or else Run.Info.Base_Bus.Pending /= V.No_Value);
   function Current (Run : Session; Service : Core.State) return Boolean is
     (Core.Current_Node (Service, Run.Device)
      and then Core.Current_Node (Service, Run.Resource_Node));
   procedure Drop (Run : in out Session; Service : aliased in out Core.State) is
   begin
      if Run.Handle = V.No_Value then return; end if;
      if Run.Release_Tried then Run.Info.Status := Cleanup_Failed; return; end if;
      Run.Release_Tried := True;
      Core.Release_Result (Service, Run.Handle, Run.Info.Cleanup_Result);
      if Run.Info.Cleanup_Result /= V.Available or else Run.Handle /= V.No_Value then
         Run.Info.Status := Cleanup_Failed; Run.Info.Recovery_Needed := True;
      end if;
   end Drop;
   procedure Begin_Query (Run : in out Session;
      Service : aliased in out Core.State; Device : Core.Node_Descriptor;
      Budget : Natural; Maximum_Items : ACPI_Resource_Data.Item_Count;
      Action : out Action_Result) is
      Found : N.Lookup_Result;
      Value : V.Result;
      Fuel : ACPI_Evaluation_Budgets.State (Budget);
      Good : Boolean;
      procedure Charge (Amount : Natural; Accepted : out Boolean) is
      begin
         ACPI_Evaluation_Budgets.Charge (Fuel, Amount, Accepted);
         Run.Info.Charged := ACPI_Evaluation_Budgets.Spent (Fuel);
         if not Accepted then
            Run.Info.Status := Provider_Contract_Error;
            Run.Info.Recovery_Needed := True;
         end if;
      end Charge;
      procedure Read_Coordinate (Selected : Roots.Member; Item : out Roots.Report) is
      begin
         Roots.Read (Service, Device, Selected, Budget - Run.Info.Charged, Item);
         Charge (Item.Charged, Good); if not Good then return; end if;
         Run.Info.Recovery_Needed := Run.Info.Recovery_Needed or Item.Recovery_Needed;
         if Item.Status not in Roots.Observed | Roots.Absent or else Item.Recovery_Needed then
            Run.Info.Status := (if Item.Status = Roots.Budget_Exhausted then
              Budget_Exhausted else Coordinate_Failed);
         end if;
      end Read_Coordinate;
      procedure Start is
      begin
         Roots.Identify_Root (Service, Device, Budget, Run.Info.Identity);
         Charge (Run.Info.Identity.Charged, Good); if not Good then return; end if;
         Run.Info.Recovery_Needed := Run.Info.Identity.Recovery_Needed;
         if Run.Info.Identity.Status /= Roots.Observed or else Run.Info.Recovery_Needed then
            Run.Info.Status := (if Run.Info.Identity.Status = Roots.Budget_Exhausted then
              Budget_Exhausted else Root_Unresolved); return;
         end if;
         if Run.Info.Identity.Kind = Roots.Unknown then Run.Info.Status := Not_A_Root; return; end if;
         Run.Info.Status := Not_Started;
         Read_Coordinate (Roots.Segment, Run.Info.Segment);
         if Run.Info.Status /= Not_Started then return; end if;
         Read_Coordinate (Roots.Base_Bus, Run.Info.Base_Bus);
         if Run.Info.Status /= Not_Started then return; end if;
         if Run.Info.Charged = Budget then Run.Info.Status := Budget_Exhausted; return; end if;
         Core.Find_Object (Service, V.Position (Device), "_CRS", Found, Run.Info.Access_Result);
         if Run.Info.Access_Result /= V.Available then Run.Info.Status := Lookup_Failed; return; end if;
         if Found.Status = N.Not_Found then Run.Info.Status := Missing_Resources; return;
         elsif Found.Status /= N.Found then Run.Info.Status := Lookup_Failed; return; end if;
         Core.Describe_Node (Service, Found.Node, Run.Resource_Node, Run.Info.Access_Result);
         if Run.Info.Access_Result /= V.Available then Run.Info.Status := Lookup_Failed; return; end if;
         Evaluate_Named (Service, Run.Resource_Node, Budget - Run.Info.Charged, Value, Run.Info.Access_Result);
         Run.Info.Execution := Value.Status;
         if Value.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned then
            Run.Handle := Value.Handle;
         end if;
         Charge (Value.Charged, Good); if not Good then return; end if;
         Run.Info.Recovery_Needed := Value.Status in AML_Execute.Fatal_Requested | AML_Execute.Field_Indeterminate;
         if not Current (Run, Service) then
            Run.Info.Status := Identity_Changed; Run.Info.Recovery_Needed := True; return;
         elsif Run.Info.Access_Result /= V.Available then Run.Info.Status := Evaluation_Failed;
         elsif Value.Status = AML_Execute.Budget_Exceeded then Run.Info.Status := Budget_Exhausted;
         elsif Value.Status /= AML_Execute.Object_Returned then Run.Info.Status := Wrong_Result_Type;
         elsif Run.Handle = V.No_Value then Run.Info.Status := Provider_Contract_Error; Run.Info.Recovery_Needed := True;
         else Run.Info.Status := Awaiting_Buffer;
         end if;
      end Start;
   begin
      if Run.Started then Action := Already_Consumed; return; end if;
      Run.Started := True; Action := Accepted; Run.Device := Device;
      Run.Maximum_Items := Maximum_Items;
      Start;
      if Run.Info.Status /= Awaiting_Buffer and then Run.Handle /= V.No_Value then
         if Current (Run, Service) then Drop (Run, Service);
         else Run.Info.Recovery_Needed := True;
         end if;
      end if;
   end Begin_Query;
   procedure Copy_And_Finish (Run : in out Session;
      Service : aliased in out Core.State; Data : out Firmware_Tables.Bytes;
      Action : out Action_Result) is
   begin
      Data := [others => 0];
      if Run.Info.Status /= Awaiting_Buffer or else Run.Handle = V.No_Value then
         Action := Not_Pending; return;
      elsif not Current (Run, Service) then Action := Wrong_Context; return;
      end if;
      Action := Accepted;
      Reader.Copy_Template (Service, Run.Handle, Data, Run.Info.Copy, Run.Maximum_Items);
      if Run.Info.Copy.Status = Reader.Insufficient_Capacity then return; end if;
      Run.Info.Status := (if Run.Info.Copy.Status = Reader.Copied_Template then Copied else Copy_Failed);
      Drop (Run, Service);
      if Run.Info.Status = Cleanup_Failed then Data := [others => 0]; end if;
   end Copy_And_Finish;
   procedure Cancel (Run : in out Session;
      Service : aliased in out Core.State; Action : out Action_Result) is
   begin
      if Run.Info.Status /= Awaiting_Buffer or else Run.Handle = V.No_Value then
         Action := Not_Pending; return;
      elsif not Current (Run, Service) then Action := Wrong_Context; return;
      end if;
      Action := Accepted; Run.Info.Status := Cancelled; Drop (Run, Service);
   end Cancel;
end ACPI_Host_Bridge_Query;
