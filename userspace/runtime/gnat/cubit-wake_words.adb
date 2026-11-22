pragma Ada_2022;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with Futex_ABI;

package body CuBit.Wake_Words is

   procedure Wake is
      Ignore : constant Unsigned_64 :=
        CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Futex_Wake, Address, 1, Futex_ABI.Private_Space);
   begin
      null;
   end Wake;

end CuBit.Wake_Words;
