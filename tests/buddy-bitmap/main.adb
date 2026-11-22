with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Buddy_Bitmap; use Buddy_Bitmap;
procedure Main is
   procedure Check (Last : Frame_Number; Highest_Order : Order; Exhaustive : Boolean) is
      L : constant Layout := Make (Last, Highest_Order);
      Previous_End : Count := 0 with Ghost;
      procedure Check_Frame (O : Order; F : Frame_Number) is
         B : constant Count := Locate (L, O, F);
      begin
         Prove_Word_Coverage (L, B);
         pragma Assert (B >= First_Bit (L, O) and B < Limit_Bit (L, O));
         pragma Assert (Word_Index (B) < Word_Count (L));
         pragma Assert (Word_Index (B) * 64 + Count (Within_Word (B)) = B);
         pragma Assert (B - First_Bit (L, O) = F / (2 ** (O + 1)));
      end Check_Frame;
   begin
      for O in 0 .. Highest_Order loop
         pragma Assert (First_Bit (L, O) = Previous_End);
         if O > 0 then
            Prove_Separate_Orders (L, 0, O, Last, 0);
         end if;
         pragma Assert (Limit_Bit (L, O) - First_Bit (L, O) = Last / (2 ** (O + 1)) + 1);
         if Exhaustive then
            for F in 0 .. Last loop Check_Frame (O, F); end loop;
         else
            Check_Frame (O, 0);
            Check_Frame (O, Last / 2);
            Check_Frame (O, Last);
         end if;
         Previous_End := Limit_Bit (L, O);
      end loop;
      pragma Assert (Previous_End = Total_Bits (L));
   end Check;
begin
   -- Reproduce the old inclusive-maximum bug: its last order-0 bit (7)
   -- coincided with the old order-1 starting offset (also 7).
   declare
      Last : constant Frame_Number := 15;
      L : constant Layout := Make (Last, 1);
      Old_Order_1_First : constant Count := Last / 2;
   begin
      pragma Assert (Old_Order_1_First = Last / Pair_Span (0));
      pragma Assert (Locate (L, 0, Last) < Locate (L, 1, 0));
      pragma Assert (First_Bit (L, 1) = 8);
   end;
   -- Every small inclusive maximum, including odd counts and final partial pairs.
   for Last in Frame_Number range 0 .. 1025 loop
      Check (Last, 11, True);
   end loop;
   for O in Order loop
      Check (2 ** (O + 1) - 1, O, False);
      if O < Order'Last then Check (2 ** (O + 1), O, False); end if;
      Check (Frame_Number'Last, O, False);
   end loop;
   -- Independent quotient oracle over deterministic full-width inputs,
   -- not just small addresses or the first/last frame of a layout.
   declare
      package Random_Frames is new Ada.Numerics.Discrete_Random (Frame_Number);
      Generator : Random_Frames.Generator;
   begin
      Random_Frames.Reset (Generator, 20260913);
      for Sample in 1 .. 10_000 loop
         declare
            Last : constant Frame_Number := Random_Frames.Random (Generator);
            Frame : constant Frame_Number := Random_Frames.Random (Generator) mod (Last + 1);
            L : constant Layout := Make (Last, Order'Last);
         begin
            for O in Order loop
               pragma Assert (Count (Pair_Index (Frame, O)) = Frame / (2 ** (O + 1)));
               pragma Assert (Locate (L, O, Frame) = First_Bit (L, O) + Frame / (2 ** (O + 1)));
            end loop;
         end;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("PASS buddy bitmap: exact per-order coverage, inclusive last frame, word bounds, all supported orders");
   Ada.Text_IO.Put_Line ("PASS shift indexing: 400000 deterministic full-width comparisons against division");
end Main;
