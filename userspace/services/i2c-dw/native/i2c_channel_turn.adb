pragma Ada_2022;
with I2C_Work_Policy;
package body I2C_Channel_Turn is
   package C renames Device.Controller;
   package RC renames I2C_Channel.RC;
   package W renames I2C_Transfer_Wire;
   package E renames CuBit.Control_Events;
   package PID renames CuBit.Process_IDs;
   use type PID.Process_ID, C.Phase, C.Collection, Device.Submission, Device.Releasing, Device.T.Ticket;
   use type W.Word, E.Event_Kind, E.Control_Kind;
   Info : Report;
   Current : Channel_Index := Channel_Index'First;
   Tag : RC.Token := 0;
   Ticket : Device.T.Ticket := Device.T.No_Ticket;
   Saved_End : E.Event;
   procedure Count (Value : in out W.Word) is
   begin if Value < W.Word'Last then Value := Value + 1; end if; end Count;
   function Observe return Report is (Info);
   procedure Publish (Index : Channel_Index; Number : RC.Token; Item : W.Completion) is
      Published : Boolean;
   begin
      Complete (Index, Number, W.Encode_Completion (Item), Published);
      if Published then Count (Info.Completed); else Info.At_Phase := Retained; end if;
   end Publish;
   procedure Turn is
      Found, Got, Closed : Boolean;
      Index : Channel_Index;
      Request : RC.Queues.Submission;
      Result : Device.Submission;
      Admission : C.Admission;
      Collection : C.Collection;
      Completion : C.Completion;
      Released : Device.Releasing;
      ID : Device.T.Ticket;
   begin
      if Info.At_Phase in Stopped | Retained then return; end if;
      if Info.Transfer_Pending then
         for Step in 1 .. I2C_Work_Policy.Work_Burst loop
            exit when Device.Observe.Completion_Pending or else Device.Observe.At_Phase = C.Faulted;
            Device.Advance;
            -- One idle sample is enough. The outer owner chooses IRQ/deadline
            -- wait or the named polling fallback; do not burn this whole burst.
            exit when Device.Observe.Waiting_For_Hardware;
         end loop;
         if not Device.Observe.Completion_Pending then
            if Device.Observe.At_Phase = C.Faulted then Info.At_Phase := Retained; end if;
            return;
         end if;
         Device.Take_Completion (Completion, Collection);
         if Collection /= C.Collected or else Completion.ID /= Ticket then
            Info.At_Phase := Retained; return;
         end if;
         Device.Release (Released);
         if Released not in Device.Released | Device.No_Payload then
            -- Do not publish an answer permitting reuse while the physical
            -- controller/lease is retained. Keep the channel and its owed tag.
            Info.At_Phase := Retained; return;
         end if;
         if Info.End_Pending then
            Close_Ending (Saved_End, Current, Closed);
            if not Closed then Info.At_Phase := Retained; return; end if;
         else
            Publish (Current, Tag,
              (W.Completed, W.Word (Completion.ID), Completion.Successful,
               Completion.Effects_Possible, W.Count (Completion.Write_Count),
               (if Completion.Successful then W.Count (Completion.Read_Count) else 0)));
            if Info.At_Phase = Retained then return; end if;
         end if;
         Info.Transfer_Pending := False; Info.End_Pending := False;
         Info.At_Phase := (if Info.Stop_Requested then Stopped else Idle);
         return;
      end if;
      if Device.Observe.At_Phase /= C.Available or else Device.Has_Payload then
         Info.At_Phase := Retained; return;
      end if;
      if Info.Stop_Requested then Info.At_Phase := Stopped; return; end if;
      Next_Channel (Found, Index);
      if not Found then return; end if;
      Take (Index, Request, Got);
      Served (Index);
      if not Got then return; end if;
      Count (Info.Submitted);
      Device.Submit (Badge_Of (Index), Request.Item, Input_Of (Index), Output_Of (Index), ID, Result, Admission);
      if Result = Device.Accepted then
         Current := Index; Tag := Request.Tag; Ticket := ID;
         Info.Transfer_Pending := True; Info.At_Phase := Running;
      elsif Result = Device.Retained then
         Info.At_Phase := Retained;
      else
         Count (Info.Rejected);
         Publish (Index, Request.Tag,
           ((case Result is
              when Device.Invalid_Metadata => W.Invalid_Frame,
              when Device.Invalid_Channel => W.Invalid_Channel,
              when Device.Unsupported_Channel => W.Unsupported_Channel,
              when Device.Invalid_Views => W.Input_Refused,
              when Device.Busy => W.Busy,
              when others => W.Admission_Refused),
             W.Word (ID), False, False, 0, 0));
      end if;
   end Turn;
   procedure On_Event (Publisher : PID.Process_ID; Event : E.Event) is
      Found, Closed : Boolean;
      Index : Channel_Index;
   begin
      if Publisher /= PID.No_Process then Count (Info.Ignored_Events); return; end if;
      if Event.Kind = E.Control then
         if Event.Control = E.Stop then Info.Stop_Requested := True;
         else Count (Info.Ignored_Events);
         end if;
         return;
      end if;
      if Event.Kind not in E.Grant_Revoked | E.Grant_Returned then
         Count (Info.Ignored_Events); return;
      end if;
      Find_Ending (Event, Found, Index);
      if not Found then Count (Info.Ignored_Events); return; end if;
      if Info.Transfer_Pending and then Index = Current then
         if not Info.End_Pending then Saved_End := Event; Info.End_Pending := True; end if;
      else
         Close_Ending (Event, Index, Closed);
         if not Closed then Info.At_Phase := Retained; end if;
      end if;
   end On_Event;
end I2C_Channel_Turn;
