pragma Ada_2022;
package body HID_Touchpad_Pump is
   use type W.Word, Native.Live.Owner.Phase, Native.App.Device.Phase;
   procedure Turn (Startup_Deadline : W.Word; Result : out Report) is
      Now : W.Word;
      Available, Completion_Found, Event_Found, Request_Found : Boolean;
      Publisher : CuBit.Process_IDs.Process_ID;
      Completion : CuBit.Messages.CompletionEntry;
      Event : CuBit.Messages.Message;
      Routed : Native.Completion_Report;
      Event_Result : Native.Event_Result;
      Stop, Handled, Saturated : Boolean := False;
      Started : Native.Live.Owner.Start_Result;
   begin
      Result := (others => <>);
      Sample_Time (Now, Available);
      if Available and then Seen_Time and then Now < Last_Time then
         Available := False;
      end if;
      if Available then Seen_Time := True; Last_Time := Now;
      else Stopping := True; end if;
      Cache_Time (Now, Available);
      if Available and then Now >= Startup_Deadline then Stopping := True; end if;
      if Stopping then Native.Live.Request_Stop (Now, Available); end if;
      for Round in 1 .. Rounds_Per_Turn loop
         Poll_Completion (Completion, Completion_Found);
         if Completion_Found then
            Result.Completions := Result.Completions + 1;
            Native.Receive_Completion (Completion, Now, Available, Routed);
         end if;
         Poll_Event (Publisher, Event, Event_Found);
         if Event_Found then
            Result.Events := Result.Events + 1;
            System_Event (Publisher, Event, Stop, Handled);
            if Stop then Stopping := True; end if;
            if not Handled then Native.Receive_Event (Publisher, Event, Event_Result); end if;
         end if;
         Drain_Request (Request_Found);
         if Request_Found then Result.Requests := Result.Requests + 1; end if;
         if Stopping then Native.Live.Request_Stop (Now, Available); end if;
         Native.Step (Now, Available);
         if Native.Live.Observe.Stopping or else Native.App.Observe.At_Phase in
           Native.App.Device.Draining | Native.App.Device.Faulted | Native.App.Device.Stopped
         then Stopping := True; end if;
         if not Stopping and then Native.App.Ready and then
           Native.Live.Observe.At_Phase = Native.Live.Owner.Cold
         then Native.Live.Start (Now, Available, Started); end if;
         Saturated := Completion_Found or Event_Found or Request_Found;
         exit when not Saturated;
      end loop;
      -- Recovery publication after a stop is still useful, but a missing clock
      -- cannot stamp a new consumer recovery observation.
      if Available then Output.Flush (Now, Result.Publication); end if;
      Result.Clock_Available := Available;
      Result.Stopping := Stopping;
      Result.Ready := not Stopping and then Native.Live.Ready;
      Result.Quiescent := Native.Live.Quiescent;
      if not Available then Result.Wait_For := Clock_Backoff;
      elsif Saturated then Result.Wait_For := Immediate;
      else
         Result.Deadline := Native.Wake_Deadline
           (Now, W.Word'Last, not Result.Publication.No_Consumer);
         if Result.Deadline <= Now then Result.Wait_For := Immediate;
         elsif Result.Deadline = W.Word'Last then Result.Wait_For := Activity;
         else Result.Wait_For := Until_Time; end if;
      end if;
   end Turn;
end HID_Touchpad_Pump;
