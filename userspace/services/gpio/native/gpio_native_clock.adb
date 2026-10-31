pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
package body GPIO_Native_Clock is
   use type Interfaces.Unsigned_64;
   Seen : Boolean := False;
   Previous : Interfaces.Unsigned_64 := 0;
   Status : Outcome := Unavailable;
   function Last_Outcome return Outcome is (Status);
   procedure Read (Now : out Intel_GPIO_IRQ.Tick; Available : out Boolean) is
      Value : constant Interfaces.Unsigned_64 :=
        CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME);
   begin
      Now := 0; Available := False;
      if Value = Interfaces.Unsigned_64'Last then Status := Unavailable;
      elsif Seen and then Value < Previous then Status := Reversed;
      else
         Seen := True; Previous := Value; Status := Sampled;
         Now := Intel_GPIO_IRQ.Tick (Value); Available := True;
      end if;
   end Read;
end GPIO_Native_Clock;
