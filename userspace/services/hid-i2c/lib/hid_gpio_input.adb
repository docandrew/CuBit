pragma Ada_2022;
package body HID_GPIO_Input with SPARK_Mode is
   package body Owner is
      use type W.Word, Controls.Phase, Controls.Reason, Controls.Start_Result, W.Completion_Status;
      use type Device.Phase, Device.Request_Result;
      function Observe (S : State) return Report is
         Result : Report := S.Info;
      begin
         if S.Op /= No_Operation then Result.Control := Controls.Observe (S.Client); end if;
         return Result;
      end Observe;
      function Quiescent (S : State) return Boolean is
        (S.Info.At_Phase in Cold | Stopped and then S.Info.Lease = 0 and then
           S.Op = No_Operation and then Controls.Observe (S.Client).At_Phase = Controls.Idle);
      function Control_Token (S : State) return W.Word is
        (if S.Op = No_Operation then 0 else Controls.Observe (S.Client).Token);
      function Wake_Deadline (S : State; Now, Otherwise : W.Word) return W.Word is
      begin
         if S.Info.At_Phase in Cold | Stopped | Unresolved then return Otherwise;
         elsif S.Info.At_Phase = Armed and then S.Info.Buffered_Lease /= 0 then
            return W.Word'Min (Now, Otherwise);
         elsif S.Op /= No_Operation then
            return Controls.Wake_Deadline (S.Client, Now, Otherwise);
         else return Otherwise; end if;
      end Wake_Deadline;
      procedure Stop_Device (S : in out State; D : in out Device.State; Why : Failure) is
      begin
         S.Info.Stopping := True;
         if S.Info.Reason in None | Requested_Stop then S.Info.Reason := Why; end if;
         if not S.Device_Stopped then Device.Request_Stop (D); S.Device_Stopped := True; end if;
      end Stop_Device;
      procedure Limit (Now : W.Word; Span : Window; Available : Boolean; Until_Time : out W.Millisecond; OK : out Boolean) is
      begin
         Until_Time := 0; OK := Available and then Now <= W.Millisecond'Last - Span;
         if OK then Until_Time := Now + Span; end if;
      end Limit;
      procedure Submit (S : in out State; Item : W.Request; Op : Operation;
         Now : W.Word; Available : Boolean; Started : out Boolean) is
         Token : W.Word;
         Have_Token : Boolean;
         Result : Controls.Start_Result;
      begin
         Started := False; Next_Token (Token, Have_Token);
         if not Have_Token then S.Info.Reason := Token_Unavailable; return; end if;
         Controls.Start (S.Client, Item, Expected_Sender, Token, Now, Available, Result);
         S.Info.Control := Controls.Observe (S.Client);
         if Result = Controls.Started then S.Op := Op; Started := True;
         elsif Result = Controls.Not_Submitted then Controls.Acknowledge (S.Client); S.Info.Reason := Submission_Refused;
         elsif Result = Controls.Already_Owned then S.Info.Reason := Control_Uncertain;
         else S.Info.Reason := Control_Failed; end if;
      end Submit;
      procedure Release (S : in out State; D : in out Device.State; Now : W.Word; Available : Boolean) is
         Until_Time : W.Millisecond;
         OK, Started : Boolean;
      begin
         if S.Op /= No_Operation then return; end if;
         if S.Info.Lease = 0 then S.Info.At_Phase := Stopped; return; end if;
         if S.Info.Release_Attempted then S.Info.At_Phase := Unresolved; return; end if;
         S.Info.Release_Attempted := True;
         Limit (Now, Cleanup_Window, Available, Until_Time, OK);
         if not OK then Stop_Device (S, D, Cleanup_Refused); S.Info.At_Phase := Unresolved; return; end if;
         Submit (S, (W.Release_Pin, (W.Within, Until_Time), S.Info.Lease), Release_Operation, Now, Available, Started);
         S.Info.At_Phase := (if Started then Releasing else Unresolved);
      end Release;
      procedure Request_Stop (S : in out State; D : in out Device.State; Now : W.Word; Clock_Available : Boolean) is
      begin
         Stop_Device (S, D, Requested_Stop);
         if S.Info.At_Phase /= Unresolved then Release (S, D, Now, Clock_Available); end if;
      end Request_Stop;
      procedure Start (S : in out State; D : in out Device.State; Now : W.Word;
         Clock_Available : Boolean; Result : out Start_Result) is
         Until_Time : W.Millisecond;
         OK, Accepted : Boolean;
      begin
         Result := Already_Started; if S.Info.At_Phase /= Cold then return; end if;
         Result := Not_Ready; if not Device.Ready (D) then return; end if;
         Limit (Now, Control_Window, Clock_Available, Until_Time, OK);
         Result := Refused;
         if not OK then Stop_Device (S, D, (if Clock_Available then Time_Exhausted else Clock_Unavailable)); S.Info.At_Phase := Stopped; return; end if;
         S.Previous := Now; S.Seen_Clock := True;
         Submit (S, (W.Arm, (W.Within, Until_Time)), Arm_Operation, Now, Clock_Available, Accepted);
         if Accepted then S.Info.At_Phase := Arming; Result := Started;
         else Stop_Device (S, D, S.Info.Reason); S.Info.At_Phase := (if S.Op = No_Operation then Stopped else Unresolved); end if;
      end Start;
      procedure Receive_Control (S : in out State; Token : W.Word; Sender : CuBit.Process_IDs.Process_ID;
         Delivery : Controls.Delivery_Status; Label : W.Operation; Protocol : W.Word;
         Words : W.Message_Words; Now : W.Word; Clock_Available : Boolean; Result : out Controls.Receive_Result) is
      begin Controls.Receive (S.Client, Token, Sender, Delivery, Label, Protocol, Words, Now, Clock_Available, Result); end Receive_Control;
      procedure Receive_Event (S : in out State; Value : W.Event; Result : out Event_Result) is
      begin
         Result := Unrelated; if S.Info.Stopping then return; end if;
         if S.Info.At_Phase in Reading | Acknowledging and then Value.Lease = S.Info.Lease and then Value.Generation = S.Info.Generation
         then Result := Duplicate; return; end if;
         if S.Info.At_Phase not in Arming | Armed | Acknowledging then return; end if;
         if S.Info.At_Phase /= Arming and then
           (Value.Lease /= S.Info.Lease or else Value.Generation <= S.Info.Last_Acknowledged) then return; end if;
         if S.Info.Buffered_Lease /= 0 then
            if Value.Lease < S.Info.Buffered_Lease then return; end if;
            if Value.Lease = S.Info.Buffered_Lease and then Value.Generation <= S.Info.Buffered_Generation then Result := Duplicate; return; end if;
         end if;
         S.Info.Buffered_Lease := Value.Lease; S.Info.Buffered_Generation := Value.Generation; Result := Buffered;
      end Receive_Event;
      procedure Complete_Control (S : in out State; D : in out Device.State; Now : W.Word; Available : Boolean) is
         Info : constant Controls.Report := Controls.Observe (S.Client);
         Op : constant Operation := S.Op;
         Completed : Boolean;
      begin
         if Op = No_Operation then return; end if;
         if Op = Arm_Operation and then Info.Have_Completion and then Info.Completion.Lease /= 0 then S.Info.Lease := Info.Completion.Lease; end if;
         if Info.At_Phase = Controls.Quarantined then
            Stop_Device (S, D, (if Info.Outcome in Controls.Clock_Unavailable | Controls.Clock_Reversed | Controls.Deadline_Expired
               then Control_Timed_Out else Control_Uncertain));
            if Info.Have_Completion or else Info.Outcome not in Controls.Clock_Unavailable | Controls.Clock_Reversed | Controls.Deadline_Expired
            then S.Info.At_Phase := Unresolved; end if;
            return;
         end if;
         if Info.At_Phase /= Controls.Finished then return; end if;
         if Info.Late then Stop_Device (S, D, Control_Timed_Out); end if;
         S.Info.Control := Info; Controls.Acknowledge (S.Client); S.Op := No_Operation;
         Completed := Info.Have_Completion and then Info.Completion.Status = W.Completed;
         if not Completed then
            Stop_Device (S, D, Control_Failed);
            if Op = Release_Operation then S.Info.At_Phase := Unresolved;
            else Release (S, D, Now, Available); end if;
         elsif Op = Release_Operation then
            S.Info.Lease := 0; S.Info.Generation := 0; S.Info.At_Phase := Stopped;
         elsif S.Info.Stopping then Release (S, D, Now, Available);
         else
            if Op = Ack_Operation then S.Info.Last_Acknowledged := S.Info.Generation; S.Info.Generation := 0; end if;
            if S.Info.Buffered_Lease /= S.Info.Lease then S.Info.Buffered_Lease := 0; S.Info.Buffered_Generation := 0; end if;
            S.Info.At_Phase := Armed;
         end if;
      end Complete_Control;
      procedure Step (S : in out State; D : in out Device.State; Now : W.Word; Clock_Available : Boolean) is
         Until_Time : W.Millisecond;
         OK, Started : Boolean;
         Requested : Device.Request_Result;
      begin
         if S.Info.At_Phase in Cold | Stopped | Unresolved then return; end if;
         if not Clock_Available or else (S.Seen_Clock and then Now < S.Previous) then
            Stop_Device (S, D, (if Clock_Available then Clock_Reversed else Clock_Unavailable));
         elsif Now >= W.Millisecond'Last then Stop_Device (S, D, Time_Exhausted);
         else S.Seen_Clock := True; S.Previous := Now; end if;
         if not S.Info.Stopping and then Device.Observe (D).At_Phase in Device.Stopped | Device.Faulted | Device.Draining
         then Stop_Device (S, D, Device_Failed); end if;
         Controls.Step (S.Client, Now, Clock_Available); Complete_Control (S, D, Now, Clock_Available);
         if S.Info.At_Phase in Stopped | Unresolved then return; end if;
         if S.Info.Stopping then Release (S, D, Now, Clock_Available); return; end if;
         if S.Info.At_Phase = Reading then
            if Device.Ready (D) then
               Limit (Now, Control_Window, Clock_Available, Until_Time, OK);
               if OK then
                  Submit (S, (W.Acknowledge, (W.Within, Until_Time), S.Info.Lease, S.Info.Generation), Ack_Operation, Now, Clock_Available, Started);
                  if Started then S.Info.At_Phase := Acknowledging; return; end if;
               end if;
               Stop_Device (S, D, Control_Failed); Release (S, D, Now, Clock_Available);
            elsif Device.Observe (D).At_Phase /= Device.Read_Input then
               Stop_Device (S, D, Device_Failed); Release (S, D, Now, Clock_Available);
            end if;
         elsif S.Info.At_Phase = Armed and then S.Info.Buffered_Lease /= 0 then
            if S.Info.Buffered_Lease /= S.Info.Lease or else S.Info.Buffered_Generation <= S.Info.Last_Acknowledged then
               S.Info.Buffered_Lease := 0; S.Info.Buffered_Generation := 0; return;
            end if;
            S.Info.Generation := S.Info.Buffered_Generation; S.Info.Buffered_Lease := 0; S.Info.Buffered_Generation := 0;
            Limit (Now, Read_Window, Clock_Available, Until_Time, OK);
            if OK then
               Device.Request_Input (D, Device.W.Millisecond (Until_Time), Now, Clock_Available, Requested);
               if Requested = Device.Accepted then S.Info.At_Phase := Reading; return; end if;
            end if;
            Stop_Device (S, D, Input_Refused); Release (S, D, Now, Clock_Available);
         end if;
      end Step;
   end Owner;
end HID_GPIO_Input;
