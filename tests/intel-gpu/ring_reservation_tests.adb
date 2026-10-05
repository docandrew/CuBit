with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Intel_GPU_Ring_Reservation; use Intel_GPU_Ring_Reservation;
with Intel_GPU_Ring_Registers;
procedure Ring_Reservation_Tests is
   package Registers renames Intel_GPU_Ring_Registers;
   function Head_Bits is new Ada.Unchecked_Conversion (Registers.Head_Register, Unsigned_32);
   function Tail_Bits is new Ada.Unchecked_Conversion (Registers.Tail_Register, Unsigned_32);
   P : Plan;
   Checks, Wraps : Natural := 0;
   Tail : Unsigned_32 := 384;
   Bytes, Distance, Needed : Unsigned_32;
begin
   for Wrap in Unsigned_32 range 0 .. 2047 loop
      declare H : constant Registers.Head_Register :=
        Registers.Decode_Head (Shift_Left (Wrap, 21) or 16128);
      begin
         pragma Assert (Registers.Valid (H, Ring_Bytes));
         pragma Assert (Registers.Offset (H) = 16128);
         pragma Assert (Unsigned_32 (H.Wrap_Count) = Wrap);
      end;
   end loop;
   for Bit in 0 .. 31 loop
      declare
         Raw : constant Unsigned_32 := Shift_Left (1, Bit);
         H : constant Registers.Head_Register := Registers.Decode_Head (Raw);
         T : constant Registers.Tail_Register := Registers.Decode_Tail (Raw);
      begin
         pragma Assert (Head_Bits (H) = Raw and Tail_Bits (T) = Raw);
         pragma Assert (Registers.Valid (H, Ring_Bytes) =
           (Bit in 2 .. 13 or else Bit in 21 .. 31));
         pragma Assert (Registers.Valid (T, Ring_Bytes) = (Bit in 3 .. 13));
      end;
   end loop;
   -- Exhaust all DWORD head/QWORD tail positions for both native segment sizes.
   -- Oracle uses signed distance arithmetic, separately from implementation.
   for Kind in 1 .. 2 loop
      Bytes := (if Kind = 1 then 120 else 384);
      for H in 0 .. 4095 loop
         for T in 0 .. 2047 loop
            P := Reserve (Unsigned_32 (H * 4), Unsigned_32 (T * 8), Bytes);
            Distance := Unsigned_32 ((H * 4 - T * 8 + 16383) mod 16384 + 1);
            Needed := Bytes;
            if T * 8 + Integer (Bytes) > 16320 then
               Needed := Needed + Unsigned_32 (16384 - T * 8);
            end if;
            pragma Assert ((P.Status = Ready) = (Needed + 64 <= Distance));
            if P.Status = Ready then
               pragma Assert (P.Consumed = Needed and P.Tail mod 8 = 0);
               pragma Assert (P.Tail <= 16320 and P.Tail = P.Start + Bytes);
               pragma Assert ((P.Padding = 0 and P.Start = Unsigned_32 (T * 8)) or else
                 (P.Padding = Unsigned_32 (16384 - T * 8) and P.Start = 0));
            end if;
            Checks := Checks + 1;
         end loop;
      end loop;
   end loop;
   for Cycle in 1 .. 4096 loop
      -- Simulated full retirement, NOT a native GPU completion claim.
      P := Reserve (Tail, Tail, 384);
      pragma Assert (P.Status = Ready);
      if P.Padding /= 0 then Wraps := Wraps + 1; end if;
      Tail := P.Tail;
   end loop;
   pragma Assert (Wraps > 90);
   pragma Assert (Reserve (4, 0, 384).Status = No_Space);
   pragma Assert (Reserve (0, 0, 0).Status = Invalid);
   pragma Assert (Reserve (1, 0, 384).Status = Invalid);
   pragma Assert (Reserve (0, 4, 384).Status = Invalid);
   pragma Assert (Reserve (0, 16384, 384).Status = Invalid);
   pragma Assert (Reserve (0, 0, Unsigned_32'Last).Status = Invalid);
   Ada.Text_IO.Put_Line ("Ring reservation PASS: " & Checks'Image &
     " head/tail/size cases, 4096 simulated retirements, wraps=" & Wraps'Image &
     "; register fields/MBZ/wrap counter checked, no hardware execution");
end Ring_Reservation_Tests;
