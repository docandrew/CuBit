pragma Ada_2022;
package body ACPI_Bootstrap.Power_Preparation with SPARK_Mode is
   use type Driver.Outcome, ACPI_Local_Runtime_Status.Outcome;
   procedure Invalidate (Item : in out Run) is
   begin
      Item.Invalidated := True; Item.Ready := False;
      if not Item.Working then Driver.Invalidate (Item.Controller); end if;
   end Invalidate;
   procedure Retire (Item : in out Run) is
   begin Item.Retiring := True; Invalidate (Item); end Retire;
   function Current_Types (Session : State; Item : Run) return Driver.Copied_Types is
     (if Item.Ready and then not Item.Working and then not Item.Retiring and then not Item.Invalidated
        and then Device_Walk_Finished (Session)
      then Driver.Current_Types (Item.Controller, Session.Core) else Driver.Empty_Types);
   procedure Prepare (Session : in out State; Item : in out Run;
      Registers : Objects.Register_Model; Budget : Natural;
      Deadline_Microseconds : Interfaces.Unsigned_64; Result : out Report) is
      Safe_Close : Boolean;
   begin
      Result := (others => <>);
      if Item.Working then Result.Status := Reentered; return; end if;
      Invalidate (Item);
      if Item.Retiring then Result.Status := Retired; return; end if;
      if not Device_Walk_Finished (Session) or else Retained_Results_Pending (Session) then return; end if;
      if Deadline_Microseconds in 0 | Interfaces.Unsigned_64'Last then
         Result.Preparation.Status := Driver.Invalid_Window; Result.Status := Preparation_Failed; return;
      elsif Budget = 0 then
         Result.Preparation.Status := Driver.No_Budget; Result.Status := Preparation_Failed; return;
      end if;
      Item.Invalidated := False; Item.Working := True;
      Begin_Event (Session.Core, Deadline_Microseconds, Result.Open_Result);
      if Result.Open_Result /= ACPI_Local_Runtime_Status.Event_Opened then
         Invalidate (Item); Item.Working := False; Driver.Invalidate (Item.Controller);
         Result.Status := Window_Refused; return;
      end if;
      Result.Opened := True;
      if Item.Retiring or else Item.Invalidated then
         End_Event (Session.Core, False, Result.Close_Result);
         Result.Closed := Result.Close_Result = ACPI_Local_Runtime_Status.Event_Completed;
         Item.Working := False; Invalidate (Item);
         Result.Status := (if Item.Retiring then Retired else Preparation_Failed); return;
      end if;
      Driver.Prepare_S5 (Item.Controller, Session.Core, Registers, Budget, Deadline_Microseconds, Result.Preparation);
      Safe_Close := Result.Preparation.Status in Driver.Prepared | Driver.Prepared_With_Method_Failure | Driver.No_Sleep_Data;
      End_Event (Session.Core, Safe_Close, Result.Close_Result);
      Result.Closed := Result.Close_Result = ACPI_Local_Runtime_Status.Event_Completed;
      Item.Working := False;
      if not Result.Closed then
         Invalidate (Item); Result.Status := Close_Failed;
      elsif Item.Retiring then
         Invalidate (Item); Result.Status := Retired;
      elsif Item.Invalidated or else not Safe_Close or else Retained_Results_Pending (Session) then
         Invalidate (Item); Result.Status := Preparation_Failed;
      elsif Result.Preparation.Status in Driver.Prepared | Driver.Prepared_With_Method_Failure then
         Item.Ready := True;
         Result.Status := (if Result.Preparation.Status = Driver.Prepared then Prepared else Prepared_With_Method_Failure);
      else
         Invalidate (Item); Result.Status := Preparation_Failed;
      end if;
   end Prepare;
end ACPI_Bootstrap.Power_Preparation;
