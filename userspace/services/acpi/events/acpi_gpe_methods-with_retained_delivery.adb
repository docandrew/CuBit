pragma Ada_2022;
package body ACPI_GPE_Methods.With_Retained_Delivery with SPARK_Mode is
   Fault : Invocation;
   Busy, Broken : Boolean := False;
   Reason : Failure := None;
   Last : Invocation_Report;
   function Failed return Boolean is (Broken);
   function Pending_Result return Boolean is (Pending (Fault));
   function Why return Failure is (Reason);
   function Observation return Invocation_Report is (Last);
   procedure Deliver (Service : aliased in out Core.State; Item : Binding;
      Budget : Natural; Result : out Delivery_Result) is
      Run : Invocation;
      Action : Action_Result;
      Moved : Transfer_Result;
      procedure Retain is
      begin
         if Pending (Run) then
            Transfer (Run, Fault, Moved);
            -- Healthy entry admits only a fresh fault slot; no callback can
            -- access it. Failure ends admission before any future delivery.
            pragma Assert (Moved = Transferred);
         end if;
      end Retain;
   begin
      Result := Refused;
      if Busy or else Broken or else Consumed (Fault) or else Pending (Fault) then return; end if;
      Busy := True;
      Invoke (Run, Service, Item, Budget, Action);
      Last := Observe (Run);
      if Pending (Run) then
         Broken := True; Reason := Result_Retained; Retain; Result := Delivery_Failed;
      elsif Action /= Accepted or else Last.Status /= Succeeded then
         Broken := True; Reason := Handler_Refused; Result := Delivery_Failed;
      else Result := Delivered;
      end if;
      Busy := False;
   exception
      when others =>
         Broken := True; Reason := Callback_Exception;
         Last := Observe (Run); Retain;
         Busy := False; Result := Delivery_Failed;
   end Deliver;
   procedure Close_Result (Service : in out Core.State; Action : out Action_Result) is
   begin
      Action := Wrong_Context;
      if Busy then return; end if;
      Busy := True; Close (Fault, Service, Action);
      if Action = Accepted then Last := Observe (Fault); end if;
      Busy := False;
   exception
      when others =>
         Broken := True; Reason := Callback_Exception; Busy := False;
         Action := Wrong_Context;
   end Close_Result;
end ACPI_GPE_Methods.With_Retained_Delivery;
