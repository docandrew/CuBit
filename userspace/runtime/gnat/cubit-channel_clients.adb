------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Channel_Bells;
with CuBit.Messages;
with CuBit.Process_Events;

package body CuBit.Channel_Clients is

   package CC renames CuBit.Channel_Contracts;
   package CP renames CuBit.Channel_Protocol;
   use type CuBit.Channels.Open_Result;
   use type CP.Open_Refusal;
   use type Tags.Issue_Result, Tags.Match, Tags.Entry_State, Tags.Tag;
   use type CuBit.Channel_Bells.Watch_Result;

   function Channel_Contract return CC.Contract is
     ((Element with delta Kind => CC.Duplex, Policy => CC.Lossless,
                          Pages => RC.Client_Pages, Buffers => RC.Server_Pages));

   function Address_Of (Base : Unsigned_64) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (Base)));

   procedure Open
     (C : in out Client; Endpoint : CuBit.Handles.Endpoint;
      Deadline : CuBit.Deadlines.Deadline;
      Outcome : out Open_Outcome; Refusal : out CP.Open_Refusal)
   is
      Result : CuBit.Channels.Open_Result;
      Watched : CuBit.Channel_Bells.Watch_Result;
   begin
      C.Table := (others => <>);
      C.Strays := 0;
      CuBit.Channels.Open
        (Endpoint, Channel_Contract, CuBit.Channels.Producing, C.Channel, Result, Refusal,
         Deadline => CuBit.Deadlines.Kernel_Word (Deadline));
      case Result is
         when CuBit.Channels.Opened =>
            RC.Attach_Client (C.Ring, Address_Of (C.Channel.Own_Base), Address_Of (C.Channel.Peer_Base));
            CuBit.Channel_Bells.Watch (C.Channel.Peer_Grant, Watched);
            C.Routed := Watched = CuBit.Channel_Bells.Routed;
            C.Why := Open_Channel;
            Outcome := Connected;
         when CuBit.Channels.Refused =>
            C.Why := Closed;
            Outcome := (if Refusal = CP.Limit_Reached then Limit_Reached else Refused);
         when CuBit.Channels.Failed =>
            C.Why := Closed;
            --  The call's deadline passed or the server is gone: the open
            --  did not happen (a late answer finds no caller).
            Outcome := (if CuBit.Deadlines.Is_Forever (Deadline) then Failed else Timed_Out);
      end case;
   end Open;

   function Is_Open (C : Client) return Boolean is (C.Why = Open_Channel);
   function Reason (C : Client) return End_Reason is (C.Why);
   function Stray_Answers (C : Client) return Unsigned_64 is (C.Strays);

   procedure Finish (C : in out Client; Why : End_Reason);
   procedure Finish (C : in out Client; Why : End_Reason) is
   begin
      if C.Why /= Open_Channel then
         return;
      end if;
      Tags.Fail_All (C.Table);
      CuBit.Channel_Bells.Unwatch (C.Channel.Peer_Grant);
      RC.Detach_Client (C.Ring);
      CuBit.Channels.Close (C.Channel);
      C.Why := Why;
   end Finish;

   procedure Close (C : in out Client) is
   begin
      CuBit.Futex_Locks.Lock (C.Guard);
      Finish (C, Closed);
      CuBit.Futex_Locks.Unlock (C.Guard);
      --  Waiting threads see the end.
      CuBit.Futex_Locks.Signal (C.Answers_Ready);
   end Close;

   procedure Ring_Server (C : Client);
   procedure Ring_Server (C : Client) is
      Ignore : CuBit.Channels.Signal_Result;
   begin
      CuBit.Channels.Signal (C.Channel, Sync => False, Result => Ignore);
   end Ring_Server;

   --  Submit (caller holds Guard): the server is rung only when it sleeps
   --  and armed its waiting word, so a busy server costs no system call.
   procedure Submit_Item (C : in out Client; Item : RC.Request;
                          Number : out Tag; Result : out Submit_Result);
   procedure Submit_Item (C : in out Client; Item : RC.Request;
                          Number : out Tag; Result : out Submit_Result)
   is
      At_Entry : Tags.Entry_Index;
      Issued : Tags.Issue_Result;
      Submitted, Ring : Boolean;
   begin
      Number := Tags.No_Tag;
      if C.Why /= Open_Channel then
         Result := Ended;
         return;
      end if;
      if not RC.Can_Submit (C.Ring) then
         Result := Full;
         return;
      end if;
      Tags.Issue (C.Table, Number, At_Entry, Issued);
      if Issued /= Tags.Issued then
         Number := Tags.No_Tag;
         Result := Full;
         return;
      end if;
      RC.Submit (C.Ring, RC.Token (Number), Item, Submitted, Ring);
      if not Submitted then
         --  Can_Submit said yes a moment ago and only this thread writes:
         --  unreachable, but never leave a tag waiting for nothing.
         Tags.Fail_All (C.Table);
         Result := Full;
         return;
      end if;
      if Ring then
         Ring_Server (C);
      end if;
      Result := Queued;
   end Submit_Item;

   procedure Submit (C : in out Client; Item : RC.Request; Number : out Tag;
                     Result : out Submit_Result) is
   begin
      CuBit.Futex_Locks.Lock (C.Guard);
      Submit_Item (C, Item, Number => Number, Result => Result);
      CuBit.Futex_Locks.Unlock (C.Guard);
   end Submit;

   --  Reap every waiting answer into its tag (caller holds Guard). Reaped:
   --  any answer was filed.
   procedure Reap_All (C : in out Client; Reaped : out Boolean);
   procedure Reap_All (C : in out Client; Reaped : out Boolean) is
      Item : RC.Queues.Completion;
      Got : Boolean;
      At_Entry : Tags.Entry_Index;
      Matched : Tags.Match;
   begin
      Reaped := False;
      if C.Why /= Open_Channel then
         return;
      end if;
      loop
         RC.Reap (C.Ring, Item, Got);
         exit when not Got;
         Tags.Complete (C.Table, Tags.Tag (Item.Tag), At_Entry, Matched);
         if Matched = Tags.Matched then
            C.Answers (At_Entry) := Item.Answer;
            Reaped := True;
         elsif C.Strays < Unsigned_64'Last then
            C.Strays := C.Strays + 1;
         end if;
      end loop;
   end Reap_All;

   procedure Poll (C : in out Client) is
      Reaped : Boolean;
   begin
      CuBit.Futex_Locks.Lock (C.Guard);
      Reap_All (C, Reaped);
      CuBit.Futex_Locks.Unlock (C.Guard);
      if Reaped then
         CuBit.Futex_Locks.Signal (C.Answers_Ready);
      end if;
   end Poll;

   procedure Take_Answer (C : in out Client; Number : Tag; Answer : out RC.Result;
                          Found, Gone : out Boolean);
   procedure Take_Answer (C : in out Client; Number : Tag; Answer : out RC.Result;
                          Found, Gone : out Boolean) is
   begin
      Found := False;
      Gone := True;
      for I in Tags.Entry_Index loop
         if C.Table.Entries (I).State /= Tags.Free and then C.Table.Entries (I).Number = Number then
            Gone := False;
            case C.Table.Entries (I).State is
               when Tags.Answered =>
                  Answer := C.Answers (I);
                  Tags.Release (C.Table, I);
                  Found := True;
               when Tags.Ended =>
                  Tags.Release (C.Table, I);
                  Gone := True;
               when others =>
                  null;
            end case;
            return;
         end if;
      end loop;
   end Take_Answer;

   procedure Wait (C : in out Client; Number : Tag; Deadline : CuBit.Deadlines.Deadline;
                   Answer : out RC.Result; Outcome : out Wait_Outcome)
   is
      package FL renames CuBit.Futex_Locks;
      Found, Gone, Reaped, Armed, Wake_Followers : Boolean;
      Activity : CuBit.Messages.Activity_Result;
      Notice : aliased CuBit.Messages.Message;
      Woke : CuBit.Messages.Wake_Result;
      Generation : Unsigned_32;

      function To_Wake (A : CuBit.Messages.Activity_Result) return CuBit.Messages.Wake_Result is
        (case A is
            when CuBit.Messages.Work_Available   => CuBit.Messages.Work_Available,
            when CuBit.Messages.Deadline_Reached => CuBit.Messages.Deadline_Reached,
            when CuBit.Messages.Unavailable      => CuBit.Messages.Unavailable);
   begin
      Outcome := Ended;
      loop
         FL.Lock (C.Guard);
         Reap_All (C, Reaped);
         Take_Answer (C, Number, Answer, Found, Gone);
         Wake_Followers := Reaped and then C.Followers > 0;
         if Found or else Gone or else C.Why /= Open_Channel then
            FL.Unlock (C.Guard);
            if Wake_Followers then
               FL.Signal (C.Answers_Ready);
            end if;
            Outcome := (if Found then Answered else Ended);
            return;
         end if;
         if not C.Pumping then
            --  This thread sleeps for every waiter: declare itself asleep
            --  (the needs-wake word; Arm looks again after it), then wait for
            --  this process's activity. The server's wake is a channel wake
            --  event; Process_Events files it (and anything else) for its
            --  readers, so no reader drops another's event.
            C.Pumping := True;
            Armed := RC.Arm_For_Completions (C.Ring);
            FL.Unlock (C.Guard);
            if Wake_Followers then
               FL.Signal (C.Answers_Ready);
            end if;
            Activity := CuBit.Messages.Work_Available;
            if Armed then
               --  One call per wake: the server's channel wake (a kernel
               --  notice) comes back from the wait itself.
               if CuBit.Process_Events.Has_Room then
                  CuBit.Messages.Wait_For_Activity_Or_Event
                    (CuBit.Deadlines.Kernel_Word (Deadline), Notice, Woke);
               else
                  Woke := To_Wake (CuBit.Messages.Wait_For_Activity_Until
                                     (CuBit.Deadlines.Kernel_Word (Deadline)));
               end if;
               case Woke is
                  when CuBit.Messages.Event_Taken =>
                     CuBit.Process_Events.File (Notice);
                  when CuBit.Messages.Work_Available =>
                     --  Something other than a kernel notice (a published
                     --  event, a message): file events as before.
                     CuBit.Process_Events.Poll;
                  when CuBit.Messages.Deadline_Reached =>
                     Activity := CuBit.Messages.Deadline_Reached;
                  when CuBit.Messages.Unavailable =>
                     Activity := CuBit.Messages.Unavailable;
               end case;
            end if;
            FL.Lock (C.Guard);
            C.Pumping := False;
            RC.Disarm_For_Completions (C.Ring);
            Wake_Followers := C.Followers > 0;
            case Activity is
               when CuBit.Messages.Work_Available =>
                  FL.Unlock (C.Guard);
               when CuBit.Messages.Deadline_Reached =>
                  Reap_All (C, Reaped);
                  Take_Answer (C, Number, Answer, Found, Gone);
                  FL.Unlock (C.Guard);
                  if Wake_Followers or else Reaped then
                     FL.Signal (C.Answers_Ready);
                  end if;
                  Outcome := (if Found then Answered elsif Gone then Ended else Timed_Out);
                  return;
               when CuBit.Messages.Unavailable =>
                  Finish (C, Server_Ended);
                  FL.Unlock (C.Guard);
            end case;
            if Wake_Followers then
               FL.Signal (C.Answers_Ready);
            end if;
         else
            --  Another thread pumps: sleep until it files answers.
            C.Followers := C.Followers + 1;
            Generation := FL.Current (C.Answers_Ready);
            FL.Unlock (C.Guard);
            if not FL.Wait (C.Answers_Ready, Generation, Deadline) then
               FL.Lock (C.Guard);
               C.Followers := C.Followers - 1;
               Reap_All (C, Reaped);
               Take_Answer (C, Number, Answer, Found, Gone);
               FL.Unlock (C.Guard);
               Outcome := (if Found then Answered elsif Gone then Ended else Timed_Out);
               return;
            end if;
            FL.Lock (C.Guard);
            C.Followers := C.Followers - 1;
            FL.Unlock (C.Guard);
         end if;
      end loop;
   end Wait;

   procedure Handle_Event (C : in out Client; Event : CuBit.Control_Events.Event;
                           Ends : out Boolean) is
   begin
      CuBit.Futex_Locks.Lock (C.Guard);
      Ends := C.Why = Open_Channel and then CuBit.Channels.Ended (C.Channel, Event);
      if Ends then
         Finish (C, Server_Ended);
      end if;
      CuBit.Futex_Locks.Unlock (C.Guard);
      if Ends then
         CuBit.Futex_Locks.Signal (C.Answers_Ready);
      end if;
   end Handle_Event;

end CuBit.Channel_Clients;
