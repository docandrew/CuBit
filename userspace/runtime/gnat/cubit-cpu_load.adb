------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.CPU_Load with SPARK_Mode is

   function Add (Left, Right : Unsigned_64) return Unsigned_64 renames CTP.Saturating_Add;

   function Between (Earlier, Later : CTP.Times) return Interval is
      Idle : constant Unsigned_64 := Elapsed (Earlier.Idle_Ticks, Later.Idle_Ticks);
      Busy : constant Unsigned_64 :=
        Add (Elapsed (Earlier.Thread_Ticks, Later.Thread_Ticks),
             Elapsed (Earlier.Scheduler_Ticks, Later.Scheduler_Ticks));
   begin
      return (Busy => Busy, Total => Add (Busy, Idle));
   end Between;

   function Share (Part, Whole : Unsigned_64) return Percent is
      Scaled_Part, Scaled_Whole : Unsigned_64;
      Result : Unsigned_64;
   begin
      if Part = 0 or else Whole = 0 then
         return 0;
      elsif Part = Whole then
         return Full;
      end if;
      --  Part * 100 must not overflow: scale both down together first
      --  (the ratio keeps at least 2**56 of resolution).
      Scaled_Part := Part;
      Scaled_Whole := Whole;
      if Scaled_Whole > Unsigned_64'Last / (2 * Full_Percent) then
         Scaled_Part := Part / 256;
         Scaled_Whole := Whole / 256;
      end if;
      pragma Assert (Scaled_Part <= Scaled_Whole);
      pragma Assert (Scaled_Whole <= Unsigned_64'Last / (2 * Full_Percent));
      if Scaled_Whole = 0 then
         return 0;
      end if;
      Result := (Scaled_Part * (2 * Full_Percent) + Scaled_Whole) / (2 * Scaled_Whole);
      pragma Assert (Result <= Full_Percent);
      --  Some use never rounds to none, and some idle never to full.
      if Result = 0 then
         Result := 1;
      elsif Result = Full_Percent then
         Result := Full_Percent - 1;
      end if;
      return Percent (Result);
   end Share;

   function Sum (Earlier, Later : Snapshot) return Interval is
      Count : constant Natural := Natural (CTP.Processor_Count'Min (Earlier.Count, Later.Count));
      Result : Interval;
      Each : Interval;
   begin
      for C in 0 .. Count - 1 loop
         pragma Loop_Invariant (Valid (Result));
         if Comparable (Earlier.CPUs (C), Later.CPUs (C)) then
            Each := Between (Earlier.CPUs (C).Totals, Later.CPUs (C).Totals);
            Result := (Busy  => Add (Result.Busy, Each.Busy),
                       Total => Add (Result.Total, Each.Total));
         end if;
      end loop;
      return Result;
   end Sum;

   function Present (S : Snapshot) return Natural is
      Result : Natural := 0;
   begin
      for C in 0 .. Natural (S.Count) - 1 loop
         pragma Loop_Invariant (Result <= C);
         if S.CPUs (C).Kind /= CTP.Not_Present then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Present;
end CuBit.CPU_Load;
