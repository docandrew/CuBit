with Owner_Test_Control;
package body CuBit.Monotonic is
   function Milliseconds return Interfaces.Unsigned_64 is
   begin
      Owner_Test_Control.Native_Reads := Owner_Test_Control.Native_Reads + 1;
      return raise Program_Error with "absent register endpoint must not sample IPC clock";
   end Milliseconds;
end CuBit.Monotonic;
