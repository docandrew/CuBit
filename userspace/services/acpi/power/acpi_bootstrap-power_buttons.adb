pragma Ada_2022;
with AML_Execute;
package body ACPI_Bootstrap.Power_Buttons with SPARK_Mode is
   package C renames ACPI_Service;package N renames C.Namespace;
   use type N.Object_Kind,ACPI_Local_Runtime_Status.Outcome;
   use type Bindings.Binding_Outcome,Identity.Identification_Outcome;
   use type Bindings.Receipt_Number,Bindings.Observation;
   use type AML_Execute.Execution_Status;
   function Valid (Item : Run) return Boolean is
     (Item.Used <= Item.Capacity and then (if Item.Published then
       Item.Attempted and Item.Opened and Item.Closed and not Item.Working and not Item.Retiring));
   procedure Retire (Item : in out Run) is
   begin Item.Retiring := True;Item.Published := False;end Retire;
   function Quiet (Item : Run) return Boolean is
     (not Item.Working and then (not Item.Opened or else Item.Closed));
   function Available (Session : State; Item : Run) return Boolean is
   begin
      if not Item.Published or Item.Working or Item.Retiring or not Device_Walk_Finished (Session) then return False;end if;
      for I in 1 .. Item.Used loop
         if Bindings.Is_Bound (Item.Values (I), Session.Core) then return True;end if;
      end loop;
      return False;
   end Available;
   procedure Discover (Session : in out State; Item : in out Run; Budget : Natural;
      Deadline : Interfaces.Unsigned_64; Result : out Report) is
      Last : N.Node_ID;
      Device : C.Node_Descriptor;Access_Result : C.Values.Access_Status;
      Safe_Close : Boolean := True;
   begin
      Result := (others => <>);
      if Item.Working then Result.Status := Reentered;return;end if;
      if Item.Retiring then Result.Status := Retired;return;end if;
      if Item.Attempted then Result.Status := Already_Attempted;return;end if;
      if not Device_Walk_Finished (Session) or else Retained_Results_Pending (Session)
        or else Pending_Notifications (Session) /= 0 then return;end if;
      if Budget = 0 or Deadline in 0 | Interfaces.Unsigned_64'Last then Result.Status := Invalid_Limits;return;end if;
      Item.Attempted := True;Item.Working := True;
      Begin_Event (Session.Core, Deadline, Result.Open_Result);
      if Result.Open_Result /= ACPI_Local_Runtime_Status.Event_Opened then
         Item.Working := False;Result.Status := Window_Refused;return;
      end if;
      Result.Opened := True;Item.Opened := True;
      Last := C.Namespace_Node_Count (Session.Core);Result.Status := Unavailable;
      for I in N.Root .. Last loop
         exit when Item.Retiring;
         if I /= N.Root then Result.Scanned := Result.Scanned + 1;end if;
         C.Describe_Node (Session.Core, I, Device, Access_Result);
         if Access_Result /= C.Values.Available then Result.Status := Topology_Changed;exit;end if;
         if C.Values.Node_Kind (Device) = N.Device_Object then
            if Item.Used = Item.Capacity then Result.Status := Storage_Insufficient;exit;end if;
            if Result.Charged = Budget then Result.Status := Observation_Failed;exit;end if;
            Item.Used := Item.Used + 1;Result.Candidates := Result.Candidates + 1;
            Bindings.Bind (Item.Values (Item.Used), Session.Core, Device, Budget - Result.Charged, Result.Last_Binding);
            if Result.Last_Binding.Charged > Budget - Result.Charged then
               Safe_Close := False;Result.Status := Observation_Failed;exit;
            end if;
            Result.Charged := Result.Charged + Result.Last_Binding.Charged;
            if Result.Last_Binding.Outcome = Bindings.Bound then Result.Bound := Result.Bound + 1;
            elsif Result.Last_Binding.Outcome = Bindings.Identity_Refused
              and then Result.Last_Binding.Identity.Outcome in Identity.Not_Button | Identity.No_HID then null;
            elsif Result.Last_Binding.Outcome = Bindings.Device_Unavailable then null;
            else
               Safe_Close := Result.Last_Binding.Identity.Outcome not in Identity.Cleanup_Error | Identity.Evaluator_Error
                 and then Result.Last_Binding.Identity.Execution not in AML_Execute.Fatal_Requested | AML_Execute.Field_Indeterminate
                 and then Result.Last_Binding.Status.Execution not in AML_Execute.Fatal_Requested | AML_Execute.Field_Indeterminate
                 and then Result.Last_Binding.Status.Outcome not in Identity.Device_Status.Cleanup_Failed | Identity.Device_Status.Provider_Contract_Error;
               Result.Status := Observation_Failed;exit;
            end if;
            if C.Namespace_Node_Count (Session.Core) /= Last then Result.Status := Topology_Changed;exit;end if;
         end if;
      end loop;
      End_Event (Session.Core, Safe_Close, Result.Close_Result);
      Result.Closed := Result.Close_Result = ACPI_Local_Runtime_Status.Event_Completed;
      Item.Closed := Result.Closed;Item.Working := False;
      if not Result.Closed then Result.Status := Close_Failed;
      elsif Item.Retiring then Result.Status := Retired;
      elsif Result.Status = Unavailable and then Result.Bound /= 0 and then not Retained_Results_Pending (Session) then
         Item.Published := True;
         if Available (Session, Item) then Result.Status := Ready;else Item.Published := False;end if;
      end if;
   end Discover;
   procedure Observe_Retained (Session : State; Item : in out Run;
      ID : Bindings.Receipt_Number; Event : C.Notification_Observation;Accepted : out Boolean) is
      Result : Bindings.Observation;
   begin
      Accepted := False;if Item.Working or ID = 0 then return;end if;
      -- Retired/unavailable registrations explicitly consume without an effect.
      Accepted := True;if not Item.Published or Item.Retiring then return;end if;
      for I in 1 .. Item.Used loop
         Bindings.Observe (Item.Values (I), Session.Core, ID, Event, Result);
         if Result = Bindings.Pressed then Record_Press;end if;
      end loop;
   end Observe_Retained;
end ACPI_Bootstrap.Power_Buttons;
