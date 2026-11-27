pragma Ada_2022;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with Notification_ABI;

package body CuBit.Wake_Words is

   procedure Wake is
      Ignore : constant Unsigned_64 :=
        CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Wake, Notification_ABI.Private_Word, Address, 1);
   begin
      null;
   end Wake;

end CuBit.Wake_Words;
