package body CCL.Configurations.Changes with SPARK_Mode => On is
   subtype Setting_Position is Natural range 0 .. MAX_SETTINGS;
   function Find (Plan : Configuration_Plan; Key : Key_Text) return Setting_Position is
   begin
      for I in 1 .. Plan.Setting_Count loop
         if Plan.Settings (I).Key.Data (1 .. Plan.Settings (I).Key.Length) =
           Key.Data (1 .. Key.Length)
         then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   function Compare (Before, After : Compilation_Result) return Review is
      Result : Review;
   begin
      if not Before.Success or else not After.Success
        or else Before.Plan.Kind /= System_Profile
        or else After.Plan.Kind /= System_Profile
      then
         return Result;
      end if;

      for I in 1 .. After.Plan.Setting_Count loop
         declare
            Previous : constant Setting_Position := Find (Before.Plan, After.Plan.Settings (I).Key);
         begin
            if Previous = 0 then
               Result.Candidate (I) := Added;
            elsif Before.Plan.Settings (Previous).Value.Data
              (1 .. Before.Plan.Settings (Previous).Value.Length) /=
              After.Plan.Settings (I).Value.Data
              (1 .. After.Plan.Settings (I).Value.Length)
            then
               Result.Candidate (I) := Replaced;
            end if;
         end;
      end loop;
      for I in 1 .. Before.Plan.Setting_Count loop
         Result.Removed (I) := Find (After.Plan, Before.Plan.Settings (I).Key) = 0;
      end loop;
      Result.Accepted := True;
      return Result;
   end Compare;
end CCL.Configurations.Changes;
