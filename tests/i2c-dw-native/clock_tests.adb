with Ada.Text_IO;
with Interfaces;
with CuBit.Messages;
with I2C_Native_Clock;
with DW_I2C_Transfer;
procedure Clock_Tests is
   package M renames CuBit.Messages;
   package C renames I2C_Native_Clock;
   use type Interfaces.Unsigned_64;
   use type DW_I2C_Transfer.Tick;
   use type C.Outcome;
   Checks : Natural := 0;
   procedure Sample (Value : Interfaces.Unsigned_64; Expected : C.Outcome) is
      Now : DW_I2C_Transfer.Tick;
      OK : Boolean;
      Before : constant Natural := M.Calls;
   begin
      M.Sample := Value;
      C.Read (Now, OK);
      if M.Calls /= Before + 1 or else C.Last_Outcome /= Expected or else
        OK /= (Expected = C.Sampled) or else
        Now /= (if OK then DW_I2C_Transfer.Tick (Value) else 0)
      then raise Program_Error with "clock continuity/availability mismatch"; end if;
      Checks := Checks + 1;
   end Sample;
begin
   Sample (Interfaces.Unsigned_64'Last, C.Unavailable);
   Sample (0, C.Sampled);
   Sample (0, C.Sampled);
   for Value in Interfaces.Unsigned_64 range 1 .. 1_024 loop
      Sample (Value, C.Sampled);
      Sample (Value, C.Sampled);
      Sample (Value - 1, C.Reversed);
      Sample (Interfaces.Unsigned_64'Last, C.Unavailable);
      Sample (Value - 1, C.Reversed);
   end loop;
   Sample (Interfaces.Unsigned_64'Last - 1, C.Sampled);
   Sample (Interfaces.Unsigned_64'Last, C.Unavailable);
   Sample (0, C.Reversed);
   Sample (Interfaces.Unsigned_64'Last - 2, C.Reversed);
   Sample (Interfaces.Unsigned_64'Last - 1, C.Sampled);
   Ada.Text_IO.Put_Line ("clock checks" & Checks'Image);
end Clock_Tests;
