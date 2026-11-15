package body Compositor_Pacing with SPARK_Mode is
   procedure Latched (S : in out State; Latch_Us, Period_Us : Time_Us) is
   begin
      S.Latch_Us := Latch_Us;
      S.Period_Us := (if Period_Us <= Maximum_Period_Us then Period_Us else 0);
   end Latched;

   procedure Frame_Cost (S : in out State; Started_Us, Armed_Us : Time_Us) is
      Decayed : constant Time_Us := S.Cost_Us - S.Cost_Us / Decay_Divisor;
   begin
      if Armed_Us < Started_Us or else Armed_Us - Started_Us > Maximum_Cost_Us then
         S.Cost_Us := Time_Us'Min (Decayed, Maximum_Cost_Us);
         return;
      end if;
      S.Cost_Us := Time_Us'Min (Time_Us'Max (Armed_Us - Started_Us, Decayed), Maximum_Cost_Us);
   end Frame_Cost;

   function Grid_Known (S : State; Now : Time_Us) return Boolean is
     (S.Period_Us in 1 .. Maximum_Period_Us and then S.Latch_Us /= 0 and then
      Now >= S.Latch_Us and then Now - S.Latch_Us <= Stale_Periods * S.Period_Us);

   function Next_Latch (S : State; Now : Time_Us) return Time_Us is
     (if not Grid_Known (S, Now) then Now
      else S.Latch_Us + ((Now - S.Latch_Us) / S.Period_Us + 1) * S.Period_Us);

   function Compose_At (S : State; Now : Time_Us) return Time_Us is
      Latch : Time_Us;
      Lead : constant Time_Us := S.Cost_Us + Margin_Us;
   begin
      if not Grid_Known (S, Now) or else S.Cost_Us > Maximum_Cost_Us then
         return Now;
      end if;
      Latch := Next_Latch (S, Now);
      if Latch < Lead or else Latch - Lead <= Now then
         return Now;
      end if;
      return Latch - Lead;
   end Compose_At;
end Compositor_Pacing;
