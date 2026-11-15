with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Present_Timing; use CuBit.Present_Timing;
--  Hosted tests of the LAT-002 present timing codec: the backend reply and
--  the compact frame-result form round-trip every valid value tried, refuse
--  malformed words, and expand wrapped 32-bit times against the receiver's
--  clock across the wrap.
procedure Present_Timing_Tests is
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL " & Name);
         raise Program_Error with Name;
      end if;
      Checks := Checks + 1;
   end Check;

   Seed : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   function Next return Unsigned_64 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Seed;
   end Next;

   procedure Reply_Cases is
      T : Timing;
      Arm, Lag, Wait : Unsigned_64;
   begin
      Check (Decode_Reply ([0, 0, 0]) = No_Timing, "all-zero reply is no timing");
      for I in 1 .. 100_000 loop
         Arm := Next mod 2 ** 50;
         Wait := Next mod 100_000;
         Lag := Next mod (Maximum_Lag_Us + 1);
         T := (if I mod 2 = 0
               then (Hardware_Latch, Arm, Arm + Wait, Arm + Wait + Lag, Next mod (Maximum_Period_Us + 1))
               else (Host_Fence, Arm, Arm + Wait, Arm + Wait + Lag, 0));
         Check (Valid (T), "generated timing valid");
         Check (Decode_Reply (Encode_Reply (T)) = T, "reply round trip");
      end loop;
      T := (Hardware_Latch, 10, 20, 30, 16_667);
      declare
         W : Reply_Words := Encode_Reply (T);
      begin
         W (3) := W (3) or 16#4000_0000#;   --  bit 30: outside period and lag
         Check (Decode_Reply (W) = No_Timing, "stray reply bit refused");
         W := Encode_Reply (T);
         W (1) := 21;                       --  armed after latched
         Check (Decode_Reply (W) = No_Timing, "arm after latch refused");
         W := Encode_Reply (T);
         W (3) := (W (3) and not 16#FF#) or 3;  --  unknown source
         Check (Decode_Reply (W) = No_Timing, "unknown source refused");
         W := Encode_Reply ((Host_Fence, 10, 20, 20, 0));
         W (3) := W (3) or Shift_Left (Unsigned_64 (16_667), 8);
         Check (Decode_Reply (W) = No_Timing, "fence with a period refused");
      end;
   end Reply_Cases;

   procedure Compact_Cases is
      T, E : Timing;
      C : Compact;
      Now, Arm : Unsigned_64;
   begin
      for I in 1 .. 100_000 loop
         Arm := Next mod 2 ** 60;
         T := (Hardware_Latch, Arm, Arm + Next mod 50_000, 0, Next mod (Maximum_Period_Us + 1));
         T.Observed_Us := T.Latched_Us + Next mod 10_000;
         C := To_Compact (T);
         Check (Unpack (Pack (C)) = C, "compact round trip");
         --  The receiver sees it a little later (under 71 minutes).
         Now := T.Observed_Us + Next mod 4_000_000_000;
         E := Expand (C, Now);
         Check (E = T, "expand recovers the full times");
      end loop;
      --  Across the 32-bit wrap of the low bits.
      T := (Hardware_Latch, 16#1_FFFF_FFF0#, 16#2_0000_0010#, 16#2_0000_0020#, 16_667);
      C := To_Compact (T);
      Check (C.Armed_Low = 16#FFFF_FFF0# and then C.Latched_Low = 16#10#, "low bits");
      Check (Expand (C, 16#2_0000_0400#) = T, "expand across the wrap");
      Check (Full (5, 16#3_0000_0004#) = 16#2_0000_0005#, "full picks the latest earlier time");
      Check (Unpack ((0, 0)) = No_Compact and then Pack (No_Compact) = (0, 0), "none packs to zero");
      Check (Unpack ((Shift_Left (Unsigned_64 (3), 8), 0)) = No_Compact, "unknown packed source");
   end Compact_Cases;
begin
   Reply_Cases;
   Compact_Cases;
   Ada.Text_IO.Put_Line ("present-timing: PASS checks=" & Checks'Image);
end Present_Timing_Tests;
