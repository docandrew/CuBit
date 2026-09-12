package body Scheduling_Budgets with SPARK_Mode is
   function Create
     (Budget : Allowance; Now : Time_Units;
      Dispatches : Dispatch_Allowance := 8;
      Interval : Period_Length := Period) return State is
     ((Interval => Interval, Ceiling => Budget, Left => Budget, Timestamp => Now,
       Executing => False, Missed_Stop => False,
       Dispatch_Ceiling => Dispatches, Dispatch_Remaining => Dispatches));

   procedure Claim_Dispatch (S : in out State; Accepted : out Boolean) is
   begin
      Accepted := Start_Allowed (S);
      if Accepted then
         S.Dispatch_Remaining := S.Dispatch_Remaining - 1;
      end if;
   end Claim_Dispatch;

   procedure Charge (S : in out State; Elapsed : Time_Units)
     with Post => S.Interval = S'Old.Interval and then
       S.Ceiling = S'Old.Ceiling and then
       S.Timestamp = S'Old.Timestamp and then
       S.Executing = S'Old.Executing and then
       S.Dispatch_Ceiling = S'Old.Dispatch_Ceiling and then
       S.Dispatch_Remaining = S'Old.Dispatch_Remaining and then
       S.Left = (if Elapsed >= S'Old.Left then 0 else S'Old.Left - Elapsed)
       and then S.Missed_Stop = (S'Old.Missed_Stop or Elapsed > S'Old.Left)
   is
   begin
      if Elapsed > S.Left then
         S.Missed_Stop := True;
         S.Left := 0;
      else
         S.Left := S.Left - Elapsed;
      end if;
   end Charge;

   procedure Account
     (S : in out State; Now : Time_Units; Execute : Boolean;
      Result : out Update_Result)
   is
      Period : constant Period_Length := S.Interval;
      Until_Boundary : constant Time_Units := Period - S.Timestamp mod Period;
      Elapsed : Time_Units;
   begin
      if Now < S.Timestamp then
         Result := Clock_Reversed;
         return;
      end if;
      Elapsed := Now - S.Timestamp;
      if Now / Period = S.Timestamp / Period then
         if S.Executing then
            Charge (S, Elapsed);
         end if;
      else
         -- Charge the old window BEFORE replenishing. A delayed timer cannot
         -- erase an overrun by returning after the replenishment boundary.
         if S.Executing then
            Charge (S, Until_Boundary);
            -- Any complete intervening period spent running exceeds our
            -- <=half-period allowance. No loop proportional to elapsed time.
            if (Elapsed - Until_Boundary) / Period > 0 then
               S.Missed_Stop := True;
            end if;
         end if;
         S.Left := S.Ceiling;
         S.Dispatch_Remaining := S.Dispatch_Ceiling;
         if S.Executing then
            Charge (S, Now mod Period);
         end if;
      end if;
      S.Timestamp := Now;
      S.Executing := Execute;
      Result := Updated;
   end Account;

   procedure Prove_Split_Charge
     (Original : State; Middle, Finish : Time_Units)
   is
      Whole, Split : State := Original;
      Result : Update_Result;
   begin
      Account (Whole, Finish, False, Result);
      Account (Split, Middle, True, Result);
      Account (Split, Finish, False, Result);
      pragma Assert (Remaining (Whole) = Remaining (Split));
      pragma Assert (Overrun (Whole) = Overrun (Split));
   end Prove_Split_Charge;
end Scheduling_Budgets;
