------------------------------------------------------------------------------
--  Hosted tests for the clock publication (shared/time, docs/fast-clock.md):
--  the conversion against exact reference values, validation, the seqlock
--  counter rules, and the generic reader against a writer racing it on
--  another CPU (a torn read shows up as a wrong time).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Clock_Publication; use Clock_Publication;
with Racing_Writer;

procedure Main is
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         Put_Line ("fast-clock: " & Name & " PASS");
      else
         Failures := Failures + 1;
         Put_Line ("fast-clock: " & Name & " FAIL");
      end if;
   end Check;

   --  floor (2**32 * 10**9 / f) and floor (t * scale / 2**32), computed
   --  with Python's exact integers.
   type Frequency_Index is range 1 .. 8;
   type Tick_Index is range 1 .. 7;
   Frequencies : constant array (Frequency_Index) of Counter_Frequency :=
     [1_000_000_000, 1_689_600_000, 2_400_000_000, 2_904_000_000,
      3_000_000_000, 3_600_000_000, 5_000_000_000, 16_000_000_000];
   Scales : constant array (Frequency_Index) of Tick_Scale :=
     [4_294_967_296, 2_542_002_424, 1_789_569_706, 1_478_983_228,
      1_431_655_765, 1_193_046_471, 858_993_459, 268_435_456];
   Ticks : constant array (Tick_Index) of Elapsed_Ticks :=
     [0, 1, 2 ** 32 - 1, 2 ** 32, 2 ** 32 + 1, 1_000_000_000_000,
      2 ** 63 - 1];
   Expected : constant array (Frequency_Index, Tick_Index) of Unsigned_64 :=
     [[0, 1, 4294967295, 4294967296, 4294967297, 1000000000000,
       9223372036854775807],
      [0, 0, 2542002423, 2542002424, 2542002424, 591856060549,
       5458908638716362751],
      [0, 0, 1789569705, 1789569706, 1789569706, 416666666511,
       3843071680591167487],
      [0, 0, 1478983227, 1478983228, 1478983228, 344352616928,
       3176092297796255743],
      [0, 0, 1431655764, 1431655765, 1431655765, 333333333255,
       3074457344902430719],
      [0, 0, 1193046470, 1193046471, 1193046471, 277777777751,
       2562047787776606207],
      [0, 0, 858993458, 858993459, 858993459, 199999999953,
       1844674406941458431],
      [0, 0, 268435455, 268435456, 268435456, 62500000000,
       576460752303423487]];

   All_Scales, All_Values, Second_Accurate : Boolean := True;
   P : Parameters;
   Value : Unsigned_64;
   Ok : Boolean;
begin
   for F in Frequency_Index loop
      All_Scales := All_Scales and then Scale_Of (Frequencies (F)) = Scales (F);
      for T in Tick_Index loop
         All_Values := All_Values and then
           Elapsed_Nanoseconds (Ticks (T), Scales (F)) = Expected (F, T);
      end loop;
      --  One second of ticks is one second, within the scale's truncation
      --  (at most f / 2**32 ns) and the final floor.
      declare
         Second : constant Unsigned_64 :=
           Elapsed_Nanoseconds (Frequencies (F), Scale_Of (Frequencies (F)));
         Slack : constant Unsigned_64 := Frequencies (F) / One_Nanosecond + 1;
      begin
         Second_Accurate := Second_Accurate and then
           Second <= Nanoseconds_Per_Second and then
           Nanoseconds_Per_Second - Second <= Slack;
      end;
   end loop;
   Check (All_Scales, "scale for 8 frequencies matches exact reference");
   Check (All_Values, "56 conversions match exact reference");
   Check (Second_Accurate, "one second of ticks converts to one second");

   P := Initial (2_400_000_000, 5_000, 7 * Nanoseconds_Per_Millisecond);
   Convert (P, 5_000, Value, Ok);
   Check (Ok and then Value = 7 * Nanoseconds_Per_Millisecond,
          "time at the base counter is the base time");
   Convert (P, 4_000, Value, Ok);
   Check (Ok and then Value = 7 * Nanoseconds_Per_Millisecond,
          "a counter behind the base reads the base time");
   Convert (P, 5_000 + 2_400_000_000, Value, Ok);
   Check (Ok and then Value = 1_007_000_000 - 1,
          "one second after the base");
   Check (Milliseconds (Value) = 1_006 and then Microseconds (Value) = 1_006_999,
          "milliseconds and microseconds floor the same nanoseconds");
   Convert (P, 5_000 + 2 ** 63, Value, Ok);
   Check (not Ok and then Value = 0,
          "a counter beyond the convertible range is unavailable");

   Convert (Not_Published, 1, Value, Ok);
   Check (not Ok, "an unpublished page is unavailable");
   Check (not Valid ((P with delta Scale => 0))
          and then not Valid ((P with delta Scale => One_Nanosecond + 1))
          and then not Valid ((P with delta Version => 2))
          and then not Valid ((P with delta Frequency => 999_999_999))
          and then not Valid ((P with delta Base_Time => Maximum_Base + 1)),
          "malformed parameters are rejected");

   Check (Stable (4, 4) and then not Stable (5, 5) and then not Stable (4, 6),
          "a snapshot is kept only between equal even counters");
   Check (Writing (Opened (4)) and then not Writing (Closed (Opened (4))),
          "opening makes the counter odd, closing even");

   Racing_Writer.Run (Value);
   Check (Value = 0, "no torn snapshot against a racing writer");
   Check (Racing_Writer.Stable_Reads > 0,
          "the racing reader completed stable reads");

   if Failures = 0 then
      Put_Line ("fast-clock: PASS");
   else
      Put_Line ("fast-clock: FAIL");
      raise Program_Error;
   end if;
end Main;
