package body Intel_GPU_Mode_Admission with SPARK_Mode is
   function Judge_All (List : C.Mode_List; Item : Facts) return Verdict_Array is
      Result : Verdict_Array (1 .. List.Count) := [others => Admitted];
   begin
      for I in Result'Range loop
         Result (I) := Judge (List.Items (I).Timing, Item);
         pragma Loop_Invariant
           (for all J in 1 .. I => Result (J) = Judge (List.Items (J).Timing, Item));
      end loop;
      return Result;
   end Judge_All;

   function Area (Mode : E.Timing) return Natural is (Mode.Width * Mode.Height);

   function Best (List : C.Mode_List; Verdicts : Verdict_Array) return Natural is
      Chosen : Natural := 0;
   begin
      for I in 1 .. List.Count loop
         if Verdicts (I) = Admitted then
            if List.Items (I).Preferred then
               return I;
            elsif Chosen = 0 or else Area (List.Items (I).Timing) > Area (List.Items (Chosen).Timing)
              or else (Area (List.Items (I).Timing) = Area (List.Items (Chosen).Timing) and then
                       List.Items (I).Timing.Clock > List.Items (Chosen).Timing.Clock)
            then
               Chosen := I;
            end if;
         end if;
         pragma Loop_Invariant (Chosen <= I and then (if Chosen /= 0 then Verdicts (Chosen) = Admitted));
      end loop;
      return Chosen;
   end Best;
end Intel_GPU_Mode_Admission;
