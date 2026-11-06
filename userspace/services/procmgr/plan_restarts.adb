package body Plan_Restarts with SPARK_Mode is
   function Next_Delay (Previous : Unsigned_64) return Unsigned_64 is
     (if Previous < First_Delay_Ms then First_Delay_Ms
      elsif Previous >= Longest_Delay_Ms / 2 then Longest_Delay_Ms
      else Previous * 2)
     with Post => Next_Delay'Result in First_Delay_Ms .. Longest_Delay_Ms;

   function Later (Now, Delay_Ms : Unsigned_64) return Unsigned_64 is
     (if Now > Unsigned_64'Last - Delay_Ms then Unsigned_64'Last else Now + Delay_Ms);

   procedure Started (R : in out Run; Now : Unsigned_64) is
   begin
      R.State := Running;
      R.Started_At := Now;
      R.Due_At := 0;
   end Started;

   procedure Ended
     (R : in out Run; Now : Unsigned_64; Failed, Restartable : Boolean;
      Scheduled : out Boolean) is
      Lasted : constant Boolean := Now >= R.Started_At and then Now - R.Started_At >= Settled_Run_Ms;
   begin
      Scheduled := Failed and Restartable;
      if not Scheduled then
         R.State := Idle;
         R.Delay_Ms := 0;
         return;
      end if;
      R.Delay_Ms := (if Lasted then First_Delay_Ms else Next_Delay (R.Delay_Ms));
      R.Due_At := Later (Now, R.Delay_Ms);
      R.State := Waiting;
   end Ended;

   procedure Refused (R : in out Run; Now : Unsigned_64) is
   begin
      R.Delay_Ms := Next_Delay (R.Delay_Ms);
      R.Due_At := Later (Now, R.Delay_Ms);
   end Refused;
end Plan_Restarts;
