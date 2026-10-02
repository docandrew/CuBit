with Interfaces;
-- Retry only deferred publication work; this is not a periodic frame clock.
package Client_Frame_Wakeup with SPARK_Mode, Pure is
   subtype Tick is Interfaces.Unsigned_64;
   use type Tick;
   function Deadline (Now, Application : Tick; Pending : Boolean) return Tick is
     (if not Pending or else Now > Tick'Last - 4 then Application
      elsif Application = 0 then Now + 4
      else Tick'Min (Application, Now + 4))
     with Post =>
       (if not Pending or else Now > Tick'Last - 4 then Deadline'Result = Application
        else Deadline'Result /= 0 and then Deadline'Result <= Now + 4 and then
          (Application = 0 or else Deadline'Result <= Application));
end Client_Frame_Wakeup;
