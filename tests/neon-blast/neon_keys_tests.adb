------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Linux-hosted tests of Neon_Keys (the UI-049 prototype): every set 1 code
--  decodes and round-trips; a 1 kHz interleaved WASD, Shift and Space
--  stream (with auto-repeat) gives exact per-frame snapshots; a tap shorter
--  than a frame still counts; Release_All leaves no stuck key.
------------------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Neon_Keys; use Neon_Keys;

procedure Neon_Keys_Tests is
   Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Put_Line ("FAIL " & What);
         Failures := Failures + 1;
      end if;
   end Check;

   B : Board := Empty_Board;
   Snap : Snapshot;
   Frame_Us : constant := 4_167;   --  a 240 Hz frame
begin
   --  Decoding.
   for K in Known_Key loop
      Check (Decode (Code_Of (K)) = K, "round trip " & Key'Image (K));
      Check (Label (K)'Length in 1 .. 8, "label " & Key'Image (K));
   end loop;
   Check (Decode (0) = Unknown and then Decode (16#59#) = Unknown and then Decode (16#FF#) = Unknown,
          "codes outside the table are Unknown");
   Check (Decode (16#11#) = W and then Decode (16#1E#) = A and then Decode (16#1F#) = S
          and then Decode (16#20#) = D and then Decode (16#39#) = Space and then Decode (16#2A#) = Left_Shift,
          "WASD, Space and Shift at their set 1 positions");

   --  A 1 kHz stream: W down at 1 ms, A at 2 ms, Shift at 3 ms; W repeats
   --  every ms (auto-repeat), Space tapped 5 .. 6 ms (inside one frame).
   Press (B, W, False, 1_000);
   Press (B, A, False, 2_000);
   Press (B, Left_Shift, False, 3_000);
   for T in 4 .. 8 loop
      Press (B, W, True, Timestamp (T * 1_000));   --  repeats change nothing
   end loop;
   Press (B, Space, False, 5_000);
   Release (B, Space, 6_000);
   Take_Snapshot (B, Snap);
   Check (Is_Down (Snap, W) and then Is_Down (Snap, A) and then Is_Down (Snap, Left_Shift), "held keys down");
   Check (Snap.Down_At (W) = 1_000, "repeat did not move W's press time");
   Check (not Is_Down (Snap, Space) and then Was_Pressed (Snap, Space) and then Active (Snap, Space),
          "a tap inside one frame counts once");
   Check (Snap.Released (Space) and then Snap.Up_At (Space) = 6_000, "the tap's release is kept");
   Check (Held_Count (B) = 3, "rollover: three held");

   --  Next frame: the tap is gone, the holds remain, nothing re-pressed.
   Release (B, A, Timestamp (Frame_Us + 500));
   Take_Snapshot (B, Snap);
   Check (not Was_Pressed (Snap, Space) and then not Active (Snap, Space), "the tap is not seen twice");
   Check (not Was_Pressed (Snap, W) and then Is_Down (Snap, W), "W held, not pressed again");
   Check (not Is_Down (Snap, A) and then Snap.Released (A), "A released in this frame");

   --  A repeat for a key whose press was lost still holds it.
   Press (B, D, True, 20_000);
   Check (Held (B, D), "a repeat of an unseen press holds the key");

   --  Focus lost / input resynchronised: nothing stays held.
   Release_All (B, 30_000);
   Take_Snapshot (B, Snap);
   Check (Held_Count (B) = 0 and then (for all K in Key => not Is_Down (Snap, K)), "no stuck keys");
   Check (Snap.Released (W) and then Snap.Released (Left_Shift) and then Snap.Released (D),
          "every held key got its release");
   Check (Snap.Up_At (W) = 30_000, "release time recorded");

   --  1 kHz alternation for a second: W/S and A/D flip every ms; each
   --  frame snapshot must see exactly the state after the last event.
   declare
      Now : Timestamp := 100_000;
      Expect_W, Expect_A : Boolean := False;
   begin
      for Ms in 1 .. 1_000 loop
         Now := Now + 1_000;
         if Ms mod 2 = 1 then
            if Expect_W then Release (B, W, Now); Press (B, S, False, Now);
            else Release (B, S, Now); Press (B, W, False, Now); end if;
            Expect_W := not Expect_W;
         else
            if Expect_A then Release (B, A, Now); Press (B, D, False, Now);
            else Release (B, D, Now); Press (B, A, False, Now); end if;
            Expect_A := not Expect_A;
         end if;
         if Ms mod 4 = 0 then
            Take_Snapshot (B, Snap);
            Check (Is_Down (Snap, W) = Expect_W and then Is_Down (Snap, S) = not Expect_W,
                   "W/S state at" & Ms'Image);
            Check (Is_Down (Snap, A) = Expect_A and then Is_Down (Snap, D) = not Expect_A,
                   "A/D state at" & Ms'Image);
            Check (Held_Count (B) = 2, "two held at" & Ms'Image);
         end if;
      end loop;
   end;

   if Failures = 0 then
      Put_Line ("NEON-KEYS: ALL PASS");
   else
      Put_Line ("NEON-KEYS: FAILURES" & Natural'Image (Failures));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Neon_Keys_Tests;
