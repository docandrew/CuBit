with Transport_Model;
package body CuBit.Monotonic is
   function Milliseconds return Interfaces.Unsigned_64 is
      Value : constant Interfaces.Unsigned_64 := Transport_Model.Now;
   begin
      Transport_Model.Clock_Reads := Transport_Model.Clock_Reads + 1;
      if Transport_Model.Advance_Clock then
         Transport_Model.Now := Transport_Model.Next_Time;
         Transport_Model.Advance_Clock := False;
      end if;
      return Value;
   end Milliseconds;
end CuBit.Monotonic;
