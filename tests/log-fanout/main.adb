with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Log_Protocol; use CuBit.Log_Protocol;
with Log_Fanout;
with Log_Budgets;
with CuBit.Authority_Policy; use CuBit.Authority_Policy;
with CuBit.Log_Records;
procedure Main is
   Store : Log_Fanout.Broker;
   First, Second, Other, Replacement, Lost : Unsigned_64;
   Result : Status;
   Value : Event;
   Expected : Event :=
     (Source => 30, Publication_Tag => Publisher_Authority_Tag,
      Monotonic_Ms => 123, Data => CuBit.Log_Records.Empty_Record);
   Empty_Value : constant Event := (others => <>);
   procedure Read (Caller, Handle : Unsigned_64) is
   begin
      Log_Fanout.Read_Next
        (Store, Caller, Observer_Authority_Tag, Handle, Value, Lost, Result);
   end Read;
begin
   declare
      Limits, Sustained : Log_Budgets.Limiter;
      Accepted : Boolean;
      Total : Natural := 0;
   begin
      pragma Assert (not Is_Publisher (Publisher_Tag_Base + 1));
      for Pool in Budget_Id loop
         for Serial in Publication_Issuance range 1 .. 100 loop
            pragma Assert (Is_Publisher (Publisher_Tag (Pool, Serial)));
            pragma Assert
              (Publication_Budget (Publisher_Tag (Pool, Serial)) = Pool);
         end loop;
         pragma Assert
           (Publication_Budget
              (Publisher_Tag (Pool, Publication_Issuance'Last)) = Pool);
      end loop;
      Log_Budgets.Advance_Time (Limits, 1000);
      --  Distinct launch/handle issuances still spend the same pool.
      for Serial in 1 .. Log_Budgets.Burst loop
         Log_Budgets.Admit
           (Limits, Publication_Budget
              (Publisher_Tag (Bootstrap_Budget, Unsigned_64 (Serial))), Accepted);
         pragma Assert (Accepted);
      end loop;
      Log_Budgets.Admit
        (Limits, Publication_Budget (Publisher_Tag (Bootstrap_Budget, 999)), Accepted);
      pragma Assert (not Accepted);
      pragma Assert (Log_Budgets.Rejected (Limits, Bootstrap_Budget) = 1);
      Log_Budgets.Admit (Limits, 2, Accepted);
      pragma Assert (Accepted);
      Log_Budgets.Advance_Time (Limits, 1099);
      pragma Assert (Log_Budgets.Remaining (Limits, Bootstrap_Budget) = 0);
      Log_Budgets.Advance_Time (Limits, 1100);
      pragma Assert (Log_Budgets.Remaining (Limits, Bootstrap_Budget) = 1);
      Log_Budgets.Admit (Limits, Bootstrap_Budget, Accepted);
      pragma Assert (Accepted);
      Log_Budgets.Advance_Time (Limits, 1050);
      Log_Budgets.Advance_Time (Limits, 1199);
      pragma Assert (Log_Budgets.Remaining (Limits, Bootstrap_Budget) = 0);
      Log_Budgets.Advance_Time (Limits, Unsigned_64'Last);
      pragma Assert
        (Log_Budgets.Remaining (Limits, Bootstrap_Budget) = Log_Budgets.Burst);
      for I in 1 .. Log_Budgets.Burst loop
         Log_Budgets.Admit (Limits, Bootstrap_Budget, Accepted);
         pragma Assert (Accepted);
      end loop;
      Log_Budgets.Advance_Time (Limits, 0);
      Log_Budgets.Advance_Time (Limits, Unsigned_64'Last);
      Log_Budgets.Admit (Limits, Bootstrap_Budget, Accepted);
      pragma Assert (not Accepted);
      --  Dense arrivals cannot replenish on each request or save credit past
      --  the burst cap. Each admitted attempt costs one, regardless of payload.
      for Tick in 0 .. 10_000 loop
         Log_Budgets.Advance_Time (Sustained, Unsigned_64 (Tick));
         Log_Budgets.Admit (Sustained, Bootstrap_Budget, Accepted);
         if Accepted then Total := Total + 1; end if;
         pragma Assert
           (Total <= Log_Budgets.Burst + Tick / Natural (Log_Budgets.Refill_Ms));
      end loop;
      pragma Assert (Total = Log_Budgets.Burst + 100);
      Put_Line ("PASS: shared producer budgets, copies, isolated pools, refill, backward/max time and sustained-rate bound");
   end;
   for Requested in Boolean loop
      for Installation in Boolean loop
         for Session in Boolean loop
            for Issuer in Boolean loop
               pragma Assert
                 ((Evaluate (Requested, Installation, Session, Issuer) = Approved) =
                  (Requested and Installation and Session and Issuer));
            end loop;
         end loop;
      end loop;
   end loop;
   pragma Assert (May_Invoke (Publisher_Authority_Tag, Publish));
   pragma Assert (not Is_Observer (Observer_Tag_Base));
   pragma Assert (not Is_Observer (Unsigned_64'Last));
   pragma Assert (Is_Observer (Observer_Tag_Base + Unsigned_64 (Unsigned_32'Last)));
   for Op in Subscribe .. Close loop
      pragma Assert (not May_Invoke (Publisher_Authority_Tag, Op));
      pragma Assert (May_Invoke (Observer_Authority_Tag, Op));
   end loop;
   pragma Assert (not May_Invoke (Observer_Authority_Tag, Publish));
   Log_Fanout.Publish (Store, Expected);
   Log_Fanout.Subscribe (Store, 30, Publisher_Authority_Tag, Other, Result);
   pragma Assert (Result = Denied and Other = 0);
   Log_Fanout.Subscribe (Store, 0, Observer_Authority_Tag, Other, Result);
   pragma Assert (Result = Denied and Other = 0);
   Log_Fanout.Subscribe (Store, 10, Observer_Authority_Tag, First, Result);
   pragma Assert (Result = OK and First /= 0);
   Log_Fanout.Subscribe (Store, 20, Observer_Authority_Tag, Second, Result);
   pragma Assert (Result = OK and Second /= First);
   Log_Fanout.Subscribe (Store, 10, Observer_Authority_Tag, Other, Result);
   pragma Assert (Result = OK and Other = First);
   --  Recycled PID with a different launch-issued observer tag cannot inherit
   --  the previous instance's subscription, even knowing its handle.
   Log_Fanout.Read_Next
     (Store, 10, Observer_Authority_Tag + 1, First, Value, Lost, Result);
   pragma Assert (Result = Denied and Value = Empty_Value and Lost = 0);
   Log_Fanout.Close (Store, 10, Observer_Authority_Tag + 1, First, Result);
   pragma Assert (Result = Denied);
   --  Knowledge of another subscriber's ID does not authorize use or close.
   Read (20, First);
   pragma Assert (Result = Denied and Value = Empty_Value and Lost = 0);
   Log_Fanout.Close (Store, 20, Observer_Authority_Tag, First, Result);
   pragma Assert (Result = Denied);
   --  Even the owner must invoke the observer authority, not its publisher one.
   Log_Fanout.Read_Next
     (Store, 10, Publisher_Authority_Tag, First, Value, Lost, Result);
   pragma Assert (Result = Denied and Value = Empty_Value and Lost = 0);
   Read (10, First); pragma Assert (Result = OK and Value = Expected);
   Read (20, Second); pragma Assert (Result = OK and Value = Expected);
   Read (10, First); pragma Assert (Result = Empty and Value = Empty_Value);
   --  One slow recipient does not advance another recipient's cursor.
   for I in 1 .. Log_Fanout.Capacity + 3 loop
      Expected.Monotonic_Ms := Unsigned_64 (I);
      Log_Fanout.Publish (Store, Expected);
      Read (10, First); pragma Assert (Result = OK and Value = Expected);
   end loop;
   Read (20, Second); pragma Assert (Result = Gap and Lost = 3 and Value = Empty_Value);
   for I in 4 .. Log_Fanout.Capacity + 3 loop
      Read (20, Second); pragma Assert (Result = OK and Value.Monotonic_Ms = Unsigned_64 (I));
   end loop;
   Read (20, Second); pragma Assert (Result = Empty);
   Log_Fanout.Close (Store, 10, Observer_Authority_Tag, First, Result);
   pragma Assert (Result = OK);
   Log_Fanout.Subscribe (Store, 10, Observer_Authority_Tag, Other, Result);
   pragma Assert (Result = OK and Other /= First and Other /= Second);
   Read (10, First); pragma Assert (Result = Denied);
   Log_Fanout.Close (Store, 10, Publisher_Authority_Tag, Other, Result);
   pragma Assert (Result = Denied);
   Read (10, Other); pragma Assert (Result = OK and Value.Monotonic_Ms = 4);
   Replacement := Other;
   --  Capacity rejection never returns a token or permits an unprivileged
   --  caller to distinguish capacity exhaustion from any other denial.
   for I in 3 .. Log_Fanout.Maximum_Subscribers loop
      Log_Fanout.Subscribe (Store, Unsigned_64 (I + 100), Observer_Authority_Tag, Other, Result);
      pragma Assert (Result = OK);
   end loop;
   Log_Fanout.Subscribe (Store, 99, Publisher_Authority_Tag, Other, Result);
   pragma Assert (Result = Denied and Other = 0);
   Log_Fanout.Subscribe (Store, 99, Observer_Authority_Tag, Other, Result);
   pragma Assert (Result = Exhausted and Other = 0);
   Log_Fanout.Advance_Time (Store, Log_Fanout.Subscription_Lease_Ms - 1);
   Read (20, Second); pragma Assert (Result = Empty);
   Log_Fanout.Advance_Time (Store, Log_Fanout.Subscription_Lease_Ms);
   Read (10, Replacement); pragma Assert (Result = Denied);
   Read (20, Second); pragma Assert (Result = Empty);
   Log_Fanout.Subscribe (Store, 99, Observer_Authority_Tag, Other, Result);
   pragma Assert (Result = OK and Other /= First);
   Log_Fanout.Advance_Time (Store, Unsigned_64'Last);
   Read (99, Other); pragma Assert (Result = Denied);
   Put_Line ("PASS: policy matrix, log fan-out gates, independent recipients, loss, launch tags, stale handles and lease expiration");
end Main;
