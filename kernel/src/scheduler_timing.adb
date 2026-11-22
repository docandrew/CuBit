package body Scheduler_Timing with SPARK_Mode is
   function Start (Now : Tick_Count; Ticks_Per_Millisecond : Tick_Rate)
     return Clock_State is ((Last => Now, Period => Ticks_Per_Millisecond));
   procedure Advance (S : in out Clock_State; Now : Tick_Count;
                      Elapsed : out Tick_Count; Valid : out Boolean) is
      Delta_Ticks : Tick_Count;
   begin
      Elapsed := 0;
      Valid := Now >= S.Last;
      if Valid then
         Delta_Ticks := Now - S.Last;
         Elapsed := Delta_Ticks / S.Period;
         S.Last := Now - Delta_Ticks mod S.Period;
      end if;
   end Advance;
   procedure Prove_Split (Initial, Middle, Finish : Tick_Count; Ticks : Tick_Rate) is
      Whole : Clock_State := Start (Initial, Ticks);
      Split : Clock_State := Whole;
      All_Ticks, First, Second : Tick_Count;
      Valid : Boolean;
   begin
      Advance (Whole, Finish, All_Ticks, Valid);
      Advance (Split, Middle, First, Valid);
      Advance (Split, Finish, Second, Valid);
      pragma Assert (Valid);
      pragma Assert (All_Ticks = First + Second);
      pragma Assert (Whole = Split);
   end Prove_Split;
end Scheduler_Timing;
