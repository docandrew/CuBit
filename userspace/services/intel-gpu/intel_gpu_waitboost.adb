pragma Ada_2022;
package body Intel_GPU_Waitboost with SPARK_Mode is

   function Elapsed (From, To : Microseconds) return Microseconds is
     (if To >= From then To - From else 0);

   procedure Add (Total : in out Microseconds; Amount : Microseconds) is
   begin
      Total := (if Total <= Microseconds'Last - Amount then Total + Amount
                else Microseconds'Last);
   end Add;

   procedure Count (Total : in out Unsigned_64) is
   begin
      if Total < Unsigned_64'Last then Total := Total + 1; end if;
   end Count;

   function Start (Floor, Boost : MHz; Now : Microseconds) return State is
     ((Current => Idle, Floor => Floor, Boost => Boost, Last_Demand => Now,
       Last_Hint => Now, Hinted => False, Sent => Floor, Since => Now,
       Ramping => False, Ramp_Since => Now, Boosts => 0, Hints => 0, Sends => 0,
       Boosted_Us => 0, Ramps => 0, Unreached => 0, Ramp_Total_Us => 0, Ramp_Max_Us => 0));

   procedure Step (S : in out State; Demand, Hint : Boolean; Now : Microseconds) is
   begin
      if Hint then
         S.Last_Hint := Now; S.Hinted := True;
         Count (S.Hints);
      end if;
      if Demand then
         S.Last_Demand := Now;
      end if;
      if Demand or Hint then
         if S.Current = Idle then
            S.Current := Boosted;
            S.Since := Now;
            Count (S.Boosts);
         end if;
      elsif S.Current = Boosted and then not Held (S.Last_Demand, Now, Work_Hold_Us) and then
        (not S.Hinted or else not Held (S.Last_Hint, Now, Input_Hold_Us))
      then
         Add (S.Boosted_Us, Elapsed (S.Since, Now));
         S.Current := Idle;
      end if;
   end Step;

   procedure Note_Sent (S : in out State; Value : MHz; Accepted : Boolean; Now : Microseconds) is
   begin
      if Accepted then
         S.Sent := Value;
         Count (S.Sends);
         if Value = S.Boost and then Value > S.Floor then
            S.Ramping := True; S.Ramp_Since := Now;
         end if;
      end if;
   end Note_Sent;

   procedure Note_Actual (S : in out State; Actual : MHz; Now : Microseconds) is
      Took : Microseconds;
   begin
      if not S.Ramping then return; end if;
      if S.Current = Boosted then
         if Actual + Reach_Tolerance_MHz >= S.Boost then
            Took := Elapsed (S.Ramp_Since, Now);
            Count (S.Ramps);
            Add (S.Ramp_Total_Us, Took);
            if Took > S.Ramp_Max_Us then S.Ramp_Max_Us := Took; end if;
            S.Ramping := False;
         end if;
      else
         Count (S.Unreached);
         S.Ramping := False;
      end if;
   end Note_Actual;

   procedure Take_Stats
     (S : in out State; Now : Microseconds;
      Boosts, Hints, Sends, Boosted_Us, Ramps, Unreached, Ramp_Avg_Us, Ramp_Max_Us : out Unsigned_64) is
   begin
      if S.Current = Boosted then
         Add (S.Boosted_Us, Elapsed (S.Since, Now));
         S.Since := Now;
      end if;
      Boosts := S.Boosts; Hints := S.Hints; Sends := S.Sends; Boosted_Us := S.Boosted_Us;
      Ramps := S.Ramps; Unreached := S.Unreached;
      Ramp_Avg_Us := (if S.Ramps = 0 then 0 else S.Ramp_Total_Us / S.Ramps);
      Ramp_Max_Us := S.Ramp_Max_Us;
      S.Boosts := 0; S.Hints := 0; S.Sends := 0; S.Boosted_Us := 0;
      S.Ramps := 0; S.Unreached := 0; S.Ramp_Total_Us := 0; S.Ramp_Max_Us := 0;
   end Take_Stats;

end Intel_GPU_Waitboost;
