pragma Ada_2022;
package body GPIO_Polling_Loop is
   use type I.Tick, Admin.Phase, R.Channel_ID;
   Seen, Scheduled, Clock_Failed : Boolean := False;
   Previous : I.Tick := 0;
   Next_Poll : Time.Finite_Time := 0;
   Failure : Clock_Condition := Sampled;
   function Supervision (S : Admin.State) return Boolean is
     (Clock_Failed or else R.C.Observe.Faulted or else R.Observe.Layout_Changed or else
        R.Observe.Unresolved /= 0 or else Admin.Observe (S).At_Phase = Admin.Quarantined);
   procedure Turn (S : in out Admin.State; Result : out Turn_Report) is
      Now : I.Tick;
      Available : Boolean;
      Wake : Time.Finite_Time;
   begin
      Result := (others => <>);
      Clock (Now, Available);
      if not Clock_Failed and then (not Available or else (Seen and then Now < Previous) or else Now >= Time.Finite_Time'Last) then
         Clock_Failed := True; Failure := (if not Available then Unavailable elsif Seen and then Now < Previous then Reversed else Exhausted);
         R.Request_Stop (0); Admin.Request_Stop (S);
      end if;
      if Clock_Failed then
         Pump.Turn (S, Result.Work);
         Result.Clock_Status := Failure;
         Result.Supervision_Required := True;
         Result.Wait_For := (Time.Relative_Delay, Progress_Interval);
         return;
      end if;
      Seen := True; Previous := Now;
      if R.Polling_Needed and then (not Scheduled or else Now >= Next_Poll) then
         R.Poll_All (Time.After (Now, Poll_Window));
         Result.Poll_Started := True;
         Scheduled := True; Next_Poll := Time.After (Now, Poll_Interval);
      end if;
      Pump.Turn (S, Result.Work);
      Result.Supervision_Required := Supervision (S);
      if Result.Supervision_Required then
         Result.Wait_For := (Time.Relative_Delay, Progress_Interval);
      elsif Result.Work.Progress_Needed then
         Wake := Time.After (Now, Progress_Interval);
         if R.Polling_Needed then
            if not Scheduled then Wake := Time.Finite_Time'Min (Wake, Time.After (Now, 0));
            else Wake := Time.Finite_Time'Min (Wake, Next_Poll); end if;
         end if;
         Result.Wait_For := (Time.Until_Time, Wake);
      elsif R.Polling_Needed then
         Result.Wait_For := (Time.Until_Time, (if Scheduled then Next_Poll else Time.After (Now, 0)));
      else Result.Wait_For := (Kind => Time.Activity);
      end if;
   end Turn;
end GPIO_Polling_Loop;
