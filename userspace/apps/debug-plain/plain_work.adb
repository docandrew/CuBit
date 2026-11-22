------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Waits;

package body Plain_Work is

   --  How long each round sleeps: long beside a debugger's reads, so the
   --  thread is nearly always found waiting.
   Pause_Ms : constant := 200;
   Step_Modulus : constant := 7;
   Remaining_Scale : constant := 3;

   procedure Pause_Between (Step : Positive)
   with No_Inline;

   procedure Pause_Between (Step : Positive) is
      --  Live across the sleep: a debugger reads it from the frame.
      Remaining : constant Integer := Step * Remaining_Scale;
   begin
      CuBit.Waits.Sleep_For (Pause_Ms);
      Totals.Last := Remaining;
   end Pause_Between;

   procedure Run is
      Step : Positive := 1;
   begin
      loop
         Totals.Rounds := Totals.Rounds + 1;
         Pause_Between (Step);
         Step := Step mod Step_Modulus + 1;
      end loop;
   end Run;

end Plain_Work;
