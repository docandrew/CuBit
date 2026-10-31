pragma Ada_2022;
with Delay_Model;
package body CuBit.Kernel_Calls is
   function Call (Number : CuBit.Kernel_ABI.System_Call;
      A0, A1, A2, A3, A4, A5 : Unsigned_64 := 0) return Unsigned_64 is
      pragma Unreferenced (A1, A2, A3, A4, A5);
   begin
      if Number = CuBit.Kernel_ABI.Yield then return Delay_Model.Yield_Result;
      elsif Number = CuBit.Kernel_ABI.Sleep_Until_Monotonic_Microsecond then
         return Delay_Model.Sleep_Result (A0);
      else raise Program_Error with "unexpected kernel operation";
      end if;
   end Call;
   function Submit (Slot : Unsigned_64; Label : Unsigned_32; Length : Unsigned_8;
      W0, W1, W2, W3 : Unsigned_64; Token : Unsigned_64) return Unsigned_64 is
      pragma Unreferenced (Slot, Label, Length, W0, W1, W2, W3, Token);
   begin
      return (raise Program_Error with "unexpected submission");
   end Submit;
end CuBit.Kernel_Calls;
