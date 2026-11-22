pragma Ada_2022;
package body Clock_Discipline with SPARK_Mode is
   --  UTC at Now_MS projected from an anchor, saturating past the range.
   procedure Project
     (UTC_MS : UTC_Milliseconds; Anchor_MS, Now_MS : Unsigned_64;
      Result : out UTC_Milliseconds; Success : out Boolean)
   is
      Elapsed : Unsigned_64;
   begin
      Result := 0;
      Success := False;
      if Now_MS < Anchor_MS then
         return;
      end if;
      Elapsed := Now_MS - Anchor_MS;
      if Elapsed > Latest_UTC_MS - UTC_MS then
         return;
      end if;
      Result := UTC_MS + Elapsed;
      Success := True;
   end Project;

   procedure Initialize
     (Item : out State; RTC_Available : Boolean;
      RTC_UTC_Seconds, RTC_Monotonic_MS : Unsigned_64;
      Floor_MS : UTC_Milliseconds)
   is
   begin
      Item := (others => <>);
      if RTC_Available and then RTC_UTC_Seconds <= Latest_UTC_MS / 1_000 and then
        RTC_UTC_Seconds * 1_000 >= Floor_MS
      then
         Item :=
           (Valid => True,
            Base_UTC_MS => RTC_UTC_Seconds * 1_000,
            Base_Monotonic_MS => RTC_Monotonic_MS,
            RTC_Valid => True,
            RTC_UTC_MS => RTC_UTC_Seconds * 1_000,
            RTC_Monotonic_MS => RTC_Monotonic_MS,
            Current_Quality => RTC_Only);
      end if;
   end Initialize;

   procedure Current
     (Item : State; Now_MS : Unsigned_64; UTC_MS : out UTC_Milliseconds;
      Success : out Boolean)
   is
   begin
      UTC_MS := 0;
      Success := False;
      if Item.Valid then
         Project (Item.Base_UTC_MS, Item.Base_Monotonic_MS, Now_MS, UTC_MS, Success);
      end if;
   end Current;

   procedure Apply
     (Item : in out State; Candidate : Sample; Now_MS : Unsigned_64;
      Floor_MS : UTC_Milliseconds; Result : out Outcome)
   is
      Estimate, RTC_Now : UTC_Milliseconds;
      Projected, RTC_Known : Boolean;
      Distance : Unsigned_64;
   begin
      if Candidate.Observed_Monotonic_MS > Now_MS then
         Result := Rejected_Future_Observation;
         return;
      elsif Now_MS - Candidate.Observed_Monotonic_MS > Maximum_Sample_Age_MS then
         Result := Rejected_Stale;
         return;
      elsif Candidate.UTC_MS > Latest_UTC_MS then
         Result := Rejected_Out_Of_Range;
         return;
      end if;
      Project (Candidate.UTC_MS, Candidate.Observed_Monotonic_MS, Now_MS,
               Estimate, Projected);
      if not Projected then
         Result := Rejected_Out_Of_Range;
         return;
      elsif Estimate < Floor_MS then
         Result := Rejected_Below_Floor;
         return;
      elsif Candidate.Uncertainty_MS > Maximum_Uncertainty_MS then
         Result := Rejected_Uncertainty;
         return;
      end if;

      if Candidate.Authenticated then
         if Candidate.Sources < 1 then
            Result := Rejected_Sources;
            return;
         end if;
      else
         if Candidate.Sources < Minimum_Unauthenticated_Sources then
            Result := Rejected_Sources;
            return;
         elsif Item.Current_Quality = Network_Authenticated then
            Result := Rejected_Conflict;
            return;
         end if;
         if Item.RTC_Valid then
            Project (Item.RTC_UTC_MS, Item.RTC_Monotonic_MS, Now_MS,
                     RTC_Now, RTC_Known);
            if not RTC_Known then
               Result := Rejected_Conflict;
               return;
            end if;
            Distance :=
              (if Estimate >= RTC_Now then Estimate - RTC_Now else RTC_Now - Estimate);
            if Distance > Maximum_Unauthenticated_Step_MS then
               Result := Rejected_Conflict;
               return;
            end if;
         end if;
      end if;

      Item.Valid := True;
      Item.Base_UTC_MS := Estimate;
      Item.Base_Monotonic_MS := Now_MS;
      Item.Current_Quality :=
        (if Candidate.Authenticated then Network_Authenticated
         else Network_Unauthenticated);
      Result := Stepped;
   end Apply;
end Clock_Discipline;
