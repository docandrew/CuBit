pragma Ada_2022;
package body HID_Touchpad_Native_Wait is
   package M renames CuBit.Messages;
   use type M.Activity_Result;
   procedure Delay_Retry (Fallback : Backoff_Milliseconds;
      Result : out M.Activity_Result) is
      Reply : constant Pump.W.Word := M.syscall (M.SYSCALL_SLEEP, Fallback);
   begin
      Result := (if Reply = Pump.W.Word'Last then M.Unavailable else M.Deadline_Reached);
   end Delay_Retry;
   procedure Await (Plan : Pump.Report; Fallback : Backoff_Milliseconds;
      Result : out M.Activity_Result) is
   begin
      case Plan.Wait_For is
         when Pump.Immediate => Result := M.Work_Available;
         when Pump.Clock_Backoff => Delay_Retry (Fallback, Result); return;
         when Pump.Activity => Result := M.Wait_For_Activity_Until (M.Wait_Forever);
         when Pump.Until_Time => Result := M.Wait_For_Activity_Until (Plan.Deadline);
      end case;
      if Result = M.Unavailable then Delay_Retry (Fallback, Result); end if;
   end Await;
end HID_Touchpad_Native_Wait;
