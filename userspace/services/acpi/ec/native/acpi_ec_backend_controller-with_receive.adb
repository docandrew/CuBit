pragma Ada_2022;
package body ACPI_EC_Backend_Controller.With_Receive is
   use type Progress.Action_Kind;
   Stopped : Stop_Reason := Serving;
   Entered : Boolean := False;
   Drain_Quarantined : Boolean := False;
   procedure Latch (Reason : Stop_Reason) is
   begin if Stopped = Serving then Stopped := Reason; end if; end Latch;
   procedure Observe (Result : in out Report) is
   begin
      Result.Reason := Stopped;
      Result.Field_Reply_Pending := Calls.Pending;
      Result.Notice_Retained := Events.Retained.Present;
      Result.Hardware_Recovery := Registers.Retained_For_Recovery;
   end Observe;
   procedure Turn (Result : out Report) is
      Event, Request : CuBit.Messages.Message;
      Sender : CuBit.Messages.Process_ID;
      Stop : Boolean;
      Local_Runnable, Receive_Request : Boolean := False;
      Hint : Progress.Report;
      procedure Check_Owner (Item : Progress.Report) is
      begin
         case Item.Action is
            when Progress.Owner_Fault => Latch (Owner_Fault);
            when Progress.Inconsistent_Owners => Latch (Inconsistent_Owners);
            when others => null;
         end case;
      end Check_Owner;
      procedure Execute is
      begin
         if Unexpected_Completion then Latch (Unexpected_Reply); return; end if;
         if Stopped /= Serving then return; end if;
         Result.Event_Seen := Poll_Event (Event, Sender);
         if Stopped /= Serving then return; end if;
         if Result.Event_Seen then
            Handle_Event (Sender, Event, Stop, Result.Event_Handled);
            if Stopped /= Serving then return; end if;
            if Stop then Registers.Request_Stop; Latch (Event_Stop); return; end if;
         end if;
         Advance (Result.Work, Result.Clock_Available);
         if Stopped /= Serving then return; end if;
         Check_Owner (Result.Work.Progress_Result);
         if Stopped /= Serving then return; end if;
         Check_Local_Work (Local_Runnable, Stop, Receive_Request);
         if Stopped /= Serving then return; end if;
         if Stop then Registers.Request_Stop; Latch (Local_Work_Stop); return; end if;
         if Receive_Request then Poll_Request (Sender, Request, Result.Request_Seen); end if;
         if Stopped /= Serving then return; end if;
         if Result.Request_Seen then
            Handle_Control (Sender, Request, Stop, Result.Control_Handled);
            if Stopped /= Serving then return; end if;
            if Stop then Registers.Request_Stop; Latch (Control_Stop); return; end if;
            if not Result.Control_Handled then
               Result.Field_Attempted := True;
               Begin_Call (Sender, Request, Result.Field);
            end if;
         end if;
         if Stopped /= Serving then return; end if;
         -- Advisory only. The engine reads its own fresh clock before IO.
         Hint := Progress.Next (0); Check_Owner (Hint);
         if Stopped /= Serving then return; end if;
         if not Result.Event_Seen and then not Result.Request_Seen
           and then not Local_Runnable
           and then Hint.Action = Progress.No_Work
         then
            Result.Waited := True;
            if not Wait_Idle then Latch (Wait_Unavailable); end if;
         end if;
      end Execute;
   begin
      Result := (others => <>);
      if Stopped /= Serving then Observe (Result); return; end if;
      if Entered then Latch (Reentered); Observe (Result); return; end if;
      Entered := True;
      begin Execute;
      exception when others => Latch (Adapter_Fault);
      end;
      Entered := False;
      Observe (Result);
   end Turn;
   procedure Drain (Result : out Drain_Report) is
   begin
      Result := (others => <>);
      if Stopped = Serving then
         Observe (Result.State); return;
      end if;
      if Stopped in Owner_Fault | Inconsistent_Owners | Adapter_Fault
        | Reentered | Unexpected_Reply
      then Drain_Quarantined := True; end if;
      if Entered then
         Drain_Quarantined := True;
         Latch (Reentered);
      end if;
      if Drain_Quarantined then
         Result.Status := Quarantined; Observe (Result.State); return;
      end if;
      Entered := True;
      begin
         Registers.Request_Stop;
         Advance (Result.State.Work, Result.State.Clock_Available);
         case Result.State.Work.Progress_Result.Action is
            when Progress.Owner_Fault | Progress.Inconsistent_Owners =>
               Drain_Quarantined := True;
            when others => null;
         end case;
      exception when others => Drain_Quarantined := True;
      end;
      Entered := False;
      Result.Status := (if Drain_Quarantined then Quarantined else Advanced);
      Observe (Result.State);
   end Drain;
   procedure Serve (Result : out Report) is
   begin
      loop Turn (Result); exit when Result.Reason /= Serving; end loop;
   end Serve;
end ACPI_EC_Backend_Controller.With_Receive;
