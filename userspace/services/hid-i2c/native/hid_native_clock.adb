pragma Ada_2022;
with CuBit.Messages;
package body HID_Native_Clock is
   package W renames I2C_Transfer_Wire;
   use type W.Word;
   Time : W.Word := 0;
   Valid : Boolean := False;
   procedure Sample_Time (Now : out W.Word; Available : out Boolean) is
   begin
      Now := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME);
      Available := Now /= W.Word'Last;
   end Sample_Time;
   procedure Cache_Time (Now : W.Word; Available : Boolean) is
   begin Time := Now; Valid := Available; end Cache_Time;
   procedure Cached_Time (Now : out W.Word; Available : out Boolean) is
   begin Now := Time; Available := Valid; end Cached_Time;
end HID_Native_Clock;
