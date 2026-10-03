package body Compositor_Backdrop_Strips with SPARK_Mode is
   function Prepare (Plan : S.Layout; First : S.Index; Length : Count) return Samples is
      Result : Samples := (others => (Valid => False));
   begin
      for I in Slot range 0 .. Length - 1 loop
         Result (I) := S.Horizontal (Plan, S.Position ((First + I) * 256));
         pragma Loop_Invariant
           (for all J in Slot =>
             (if J <= I then Result (J) = S.Horizontal (Plan, S.Position ((First + J) * 256))
              else not Result (J).Valid));
      end loop;
      return Result;
   end Prepare;
end Compositor_Backdrop_Strips;
