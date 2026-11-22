package body Realtime_Admission with SPARK_Mode => On is

   function Utilization_Of
     (Budget : Microseconds; Period : Period_Microseconds) return Utilization
   is
      -- Integer arithmetic (no wraparound), in steps the provers follow.
      subtype Wide is Product;
      P      : constant Wide := Wide (Period);
      Scaled : constant Wide := Wide (Budget) * Parts_Per_Million;
      Top    : constant Wide := Scaled + P - 1;
      Share  : constant Wide := Top / P;
   begin
      pragma Assert (Top - Share * P < P);
      pragma Assert (Share * P > Top - P);
      pragma Assert (Top - P = Scaled - 1);
      pragma Assert (Share * P >= Scaled);
      pragma Assert (Wide (Budget) <= P);
      pragma Assert (Scaled <= P * Parts_Per_Million);
      pragma Assert (Share <= Parts_Per_Million);
      return Utilization (Share);
   end Utilization_Of;

   procedure Admit
     (Admitted : in out Total;
      Request  : Utilization;
      CPUs     : CPU_Count;
      Granted  : out Boolean) is
   begin
      Granted := Admitted <= Capacity (CPUs) and then
        Request <= Utilization (Realtime_Share) and then
        Total (Request) <= Capacity (CPUs) - Admitted;
      if Granted then
         Admitted := Admitted + Total (Request);
      end if;
   end Admit;

   procedure Release (Admitted : in out Total; Held : Utilization) is
   begin
      Admitted := Admitted - Total (Held);
   end Release;

end Realtime_Admission;
