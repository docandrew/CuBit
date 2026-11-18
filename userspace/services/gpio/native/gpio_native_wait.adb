pragma Ada_2022;
with CuBit.Waits;
with Interfaces;
package body GPIO_Native_Wait is
   package M renames CuBit.Messages;
   package T renames GPIO_Loop_Time;
   use type M.Activity_Result, Interfaces.Unsigned_64;
   procedure Await (Plan : T.Wait_Plan; Fallback : T.Duration; Result : out M.Activity_Result) is
      Reply : Interfaces.Unsigned_64;
   begin
      case Plan.Kind is
         when T.Activity => Result := M.Wait_For_Activity_Until (M.Wait_Forever);
         when T.Until_Time => Result := M.Wait_For_Activity_Until (Interfaces.Unsigned_64 (Plan.Deadline));
         when T.Relative_Delay =>
            Reply := CuBit.Waits.Sleep_For (Interfaces.Unsigned_64 (Plan.Backoff));
            Result := (if Reply = Interfaces.Unsigned_64'Last then M.Unavailable else M.Deadline_Reached);
            return;
      end case;
      if Result = M.Unavailable then Reply := CuBit.Waits.Sleep_For (Interfaces.Unsigned_64 (Fallback)); end if;
   end Await;
end GPIO_Native_Wait;
