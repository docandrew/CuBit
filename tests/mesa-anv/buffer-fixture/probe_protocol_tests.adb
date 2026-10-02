with Interfaces; use Interfaces;
with Native_GPU_Probe_Protocol; use Native_GPU_Probe_Protocol;
with Ada.Text_IO;
procedure Probe_Protocol_Tests is
   Value : Words;
   Count : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      if not Condition then raise Program_Error; end if;
      Count := Count + 1;
   end Check;
begin
   Check (Valid_Desktop_Request ([1, 0, 0, 0]));
   for Code in Unsigned_64 range 0 .. 255 loop
      Check (Valid_Desktop_Reply ([Code, 1, 0, 0]) = (Code <= 2));
   end loop;
   for Word in 0 .. 3 loop
      for Bit in 0 .. 63 loop
         Value := [1, 0, 0, 0];
         Value (Word) := Value (Word) xor Shift_Left (Unsigned_64'(1), Bit);
         Check (not Valid_Desktop_Request (Value));
         if Word /= 0 then
            Value := [0, 1, 0, 0];
            Value (Word) := Value (Word) xor Shift_Left (Unsigned_64'(1), Bit);
            Check (not Valid_Desktop_Reply (Value));
         end if;
      end loop;
   end loop;
   Check (Valid_Request (Request (Read_Target)));
   Check (not Valid_Request (Request (Read_Target, 1)));
   Check (not Valid_Request (Request (Retire_Target)));
   Check (Valid_Request (Request (Retire_Target, Unsigned_64'Last)));
   for Action in Operation loop
      for Code in Status loop
         Value := Reply (Code);
         Check (Valid_Reply (Action, Value) =
           (if Code = Success then Action = Retire_Target
            elsif Code = Pending then Action = Retire_Target else True));
         Value := Reply (Code, 42);
         Check (Valid_Reply (Action, Value) =
           (if Code = Success then Action = Read_Target
            elsif Code = Pending then Action = Retire_Target else True));
      end loop;
      for Bit in 0 .. 63 loop
         Value := Reply (Success, (if Action = Read_Target then 42 else 0));
         Value (1) := Value (1) xor Shift_Left (Unsigned_64'(1), Bit);
         Check (not Valid_Reply (Action, Value));
         Value := Reply (Success, (if Action = Read_Target then 42 else 0));
         Value (3) := Value (3) xor Shift_Left (Unsigned_64'(1), Bit);
         Check (not Valid_Reply (Action, Value));
         Value := Reply (Denied);
         Value (2) := Shift_Left (Unsigned_64'(1), Bit);
         Check (not Valid_Reply (Action, Value));
      end loop;
      for Invalid in Unsigned_64 range 5 .. 255 loop
         Check (not Valid_Reply (Action, [Invalid, 1, 0, 0]));
      end loop;
   end loop;
   for Bit in 0 .. 63 loop
      Value := Request (Retire_Target, 42);
      Value (3) := Shift_Left (Unsigned_64'(1), Bit);
      Check (not Valid_Request (Value));
   end loop;
   Ada.Text_IO.Put_Line ("probe protocol PASS checks=" & Count'Image);
end Probe_Protocol_Tests;
