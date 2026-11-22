pragma Ada_2022;
with AML_Execute;
package body ACPI_EC_Event_Receiver with SPARK_Mode is
   package E renames ACPI_EC_Event_Types;
   package P renames ACPI_EC_Event_Policy;
   use type E.Controller_ID;
   use type Core.Node_Descriptor, Core.Namespace.Object_Kind, Core.Values.Access_Status;
   use type Methods.Resolution_Outcome, Methods.Invocation_Outcome, Methods.Action_Result;
   use type P.Binding_Result, P.Admission;
   use type AML_Execute.Execution_Status;
   function Recovery_Needed return Boolean is (Fatal_Seen);
   function Fatal_Diagnostic return AML_Fatal.Observation is (Fatal_Info);
   function Consumed (Run : Delivery) return Boolean is (Run.Used);
   function Pending (Run : Delivery) return Boolean is (Methods.Pending (Run.Handler));
   function Observe (Run : Delivery) return Report is (Run.Info);
   function Receipt (Run : Delivery) return E.Notice_Option is
     (if Run.Used then (Present => True, Item => Run.Event)
      else (Present => False));
   function Find (Controller : E.Controller_ID) return Slot_Count is
   begin
      for I in Slot_Index loop
         if P.View (Routes (I).Logic).Used
           and then P.View (Routes (I).Logic).Controller = Controller then return I; end if;
      end loop;
      return 0;
   end Find;
   function Enrollment_Of (Result : P.Binding_Result) return Enrollment is
     (case Result is
        when P.Bound => Installed,
        when P.Invalid_Identity | P.Other_Controller => Invalid_Identity,
        when P.Route_Held => Route_Held,
        when P.Already_Active => Already_Active,
        when P.Old_Epoch => Old_Epoch);
   function Outcome_Of (Result : P.Admission) return Outcome is
     (case Result is
        when P.Admitted => Claimed,
        when P.Unknown_Controller => Unknown_Controller,
        when P.Disabled => Disabled,
        when P.Wrong_Sender => Wrong_Sender,
        when P.Wrong_Epoch => Wrong_Epoch,
        when P.Scope_Changed => Scope_Changed,
        when P.Invalid_Token => Invalid_Token,
        when P.Duplicate => Duplicate,
        when P.Token_Conflict => Token_Conflict,
        when P.Old_Token => Old_Token,
        when P.Field_Uncertain => Field_Uncertain,
        when P.Busy => Busy);
   procedure Install
     (Service : Core.State; Controller : E.Controller_ID; Epoch : E.Epoch_ID;
      Sender : CuBit.Process_IDs.Process_ID; Scope : Core.Node_Descriptor;
      Result : out Enrollment)
   is
      Selected : Slot_Count := Find (Controller);
      Decision : P.Binding_Result;
   begin
      Result := Receiver_Busy; if In_Call then return; end if;
      Result := Receiver_Recovery_Required; if Fatal_Seen then return; end if;
      Result := Invalid_Identity; if not P.Identity_Valid (Controller, Epoch, Sender) then return; end if;
      Result := Invalid_Scope;
      if not Core.Current_Node (Service, Scope)
        or else Core.Values.Node_Kind (Scope) /= Core.Namespace.Device_Object
      then return; end if;
      if Selected /= 0 then
         Decision := P.Binding_Decision (Routes (Selected).Logic, Controller, Epoch, Sender);
         if Decision /= P.Bound then Result := Enrollment_Of (Decision); return; end if;
      end if;
      for I in Slot_Index loop
         declare V : constant P.Description := P.View (Routes (I).Logic); begin
            if I /= Selected and then V.Used and then (V.Enabled or V.Held or V.Uncertain)
              and then Routes (I).Scope = Scope
            then Result := Scope_Conflict; return; end if;
         end;
      end loop;
      if Selected = 0 then
         for I in Slot_Index loop
            if not P.View (Routes (I).Logic).Used then Selected := I; exit; end if;
         end loop;
      end if;
      Result := Capacity_Exhausted; if Selected = 0 then return; end if;
      P.Bind (Routes (Selected).Logic, Controller, Epoch, Sender, Decision);
      Result := Enrollment_Of (Decision);
      if Decision = P.Bound then Routes (Selected).Scope := Scope; end if;
   end Install;
   procedure Revoke (Controller : E.Controller_ID; Epoch : E.Epoch_ID; Accepted : out Boolean) is
      Selected : constant Slot_Count := Find (Controller);
   begin
      Accepted := False;
      if In_Call or else Selected = 0 then return; end if;
      P.Revoke (Routes (Selected).Logic, Controller, Epoch, Accepted);
   end Revoke;
   procedure Dispatch
     (Run : in out Delivery; Service : aliased in out Core.State;
      Sender : CuBit.Process_IDs.Process_ID; Item : E.Notice;
      Budget : Natural; Result : out Outcome)
   is
      Selected : Slot_Count;
      Binding : Methods.Binding;
      Action : Methods.Action_Result;
      Status : Core.Values.Access_Status;
      Admission : P.Admission;
      Scope_Current, Accepted : Boolean;
   begin
      Result := Already_Consumed; if Run.Used then return; end if;
      Result := Busy; if In_Call then return; end if;
      Result := Recovery_Required; if Fatal_Seen then return; end if;
      Selected := Find (Item.Controller);
      Result := Unknown_Controller; if Selected = 0 then return; end if;
      Scope_Current := Core.Current_Node (Service, Routes (Selected).Scope);
      Admission := P.Decision (Routes (Selected).Logic, Sender, Item, Scope_Current);
      Result := Outcome_Of (Admission); if Admission /= P.Admitted then return; end if;
      Core.Describe_Node (Service, Core.Namespace.Root, Run.Owner, Status);
      Result := Context_Failed; if Status /= Core.Values.Available then return; end if;
      --  The proved policy claims this exact identity before any AML callback.
      P.Claim (Routes (Selected).Logic, Sender, Item, Scope_Current, Admission);
      Result := Outcome_Of (Admission); if Admission /= P.Admitted then return; end if;
      Run.Used := True; Run.Slot := Selected; Run.Event := Item;
      In_Call := True; Run.Info.Status := Claimed;
      Methods.Resolve (Service, Routes (Selected).Scope, Methods.Event_Number (Item.Number),
                       Binding, Run.Info.Resolution);
      if Run.Info.Resolution.Status /= Methods.Found then
         Run.Info.Status := (if Run.Info.Resolution.Status = Methods.Missing then Handler_Missing else Resolution_Failed);
      else
         Run.Info.Field_Uncertainty := True;
         P.Guard (Routes (Selected).Logic, Run.Event, Accepted);
         if not Accepted then Result := Context_Failed; return; end if;
         Methods.Invoke (Run.Handler, Service, Binding, Budget, Action);
         Run.Info.Invocation := Methods.Observe (Run.Handler);
         if Run.Info.Invocation.Execution = AML_Execute.Fatal_Requested then
            Fatal_Seen := True;
            Fatal_Info := Run.Info.Invocation.Fatal;
         end if;
         Run.Info.Field_Uncertainty := Run.Info.Invocation.Execution = AML_Execute.Field_Indeterminate;
         if Run.Info.Field_Uncertainty then Run.Info.Status := Field_Uncertain;
         elsif Pending (Run) then Run.Info.Status := Cleanup_Pending;
         elsif Action = Methods.Accepted and then Run.Info.Invocation.Status = Methods.Succeeded then Run.Info.Status := Completed;
         else Run.Info.Status := Invocation_Failed;
         end if;
      end if;
      P.Settle (Routes (Selected).Logic, Run.Event, Pending (Run), Run.Info.Field_Uncertainty, Accepted);
      if not Accepted then Result := Context_Failed; return; end if;
      In_Call := False; Result := Run.Info.Status;
   end Dispatch;
   procedure Close (Run : in out Delivery; Service : in out Core.State; Result : out Close_Result) is
      Action : Methods.Action_Result;
      Accepted : Boolean;
   begin
      Result := Receiver_Busy; if In_Call then return; end if;
      Result := No_Pending_Result; if not Run.Used or else not Pending (Run) then return; end if;
      Result := Wrong_Context;
      if not Core.Current_Binding (Service, Run.Owner) or else Run.Slot = 0
        or else not P.Matches (Routes (Run.Slot).Logic, Run.Event)
      then return; end if;
      In_Call := True;
      Methods.Close (Run.Handler, Service, Action);
      Run.Info.Invocation := Methods.Observe (Run.Handler);
      P.Settle (Routes (Run.Slot).Logic, Run.Event, Pending (Run), Run.Info.Field_Uncertainty, Accepted);
      if not Accepted then Result := Cleanup_Failed; return; end if;
      In_Call := False;
      if Action = Methods.Accepted and then not Pending (Run) then
         Result := Closed;
         Run.Info.Status := (if Run.Info.Field_Uncertainty then Field_Uncertain else Cleanup_Complete);
      else Result := Cleanup_Failed;
      end if;
   end Close;
end ACPI_EC_Event_Receiver;
