------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Channel_Protocol;
with CuBit.Handles.IPC;
with CuBit.Messages;
with CuBit.Published_Clock;
with Input_Stamps;

package body CuBit.Input_Source is

   package IR renames CuBit.Input_Rings;
   use type CuBit.Channels.Open_Result;
   use type CuBit.Input.Device_Class;

   function Delivery return CuBit.Input.Delivery_Class is
     (case Device is
        when CuBit.Input.KEYBOARD => CuBit.Input.ORDERED_TRANSITION,
        when CuBit.Input.RELATIVE_POINTER => CuBit.Input.ACCUMULABLE_DISPLACEMENT,
        when CuBit.Input.ABSOLUTE_POINTER => CuBit.Input.REPLACEABLE_STATE);

   --  Every source's reports are one generation: a new ring is a new
   --  source at Desktop, so nothing carries over to need another.
   Generation : constant CuBit.Input.Source_Generation := 1;

   function Address_Of (Base : Unsigned_64) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (Base)));

   function Now return Unsigned_64 is (CuBit.Published_Clock.Milliseconds);

   function Later (From, By : Unsigned_64) return Unsigned_64 is
     (if From > Unsigned_64'Last - By then Unsigned_64'Last else From + By);

   procedure Count_Up (Value : in out Unsigned_64) is
   begin
      if Value < Unsigned_64'Last then
         Value := Value + 1;
      end if;
   end Count_Up;

   --  The one place a source wakes Desktop (IPC-010 swaps this for a
   --  shared futex).
   procedure Wake_Desktop (S : Source) is
      Ignore : CuBit.Channels.Signal_Result;
   begin
      CuBit.Channels.Signal (S.Channel, Sync => False, Result => Ignore);
   end Wake_Desktop;

   procedure Forget_Desktop (S : in out Source) is
      Ignore : CuBit.Handles.IPC.Close_Result;
   begin
      if CuBit.Handles.Valid (S.Desktop) then
         Ignore := CuBit.Handles.IPC.Close (S.Desktop);
      end if;
      S.Desktop := CuBit.Handles.No_Endpoint;
   end Forget_Desktop;

   --  The ring ended (or never opened): let go of it; look again later.
   procedure End_Ring (S : in out Source) is
   begin
      if S.Open then
         IR.Channels.Detach_Producer (S.Ring);
         CuBit.Channels.Close (S.Channel);
      end if;
      S.Open := False;
      S.Room_Armed := False;
      Forget_Desktop (S);
      Reset (S.Retained);
      S.Retry_At := Later (Now, Establish_Retry_Ms);
   end End_Ring;

   procedure Flush (S : in out Source);

   --  devmgr's endpoint to Desktop for input sources, then the ring.
   procedure Establish (S : in out Source; Manager : CuBit.Handles.Endpoint) is
      Msg : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Returned : CuBit.Messages.MessageTag;
      Carried : CuBit.Handles.IPC.Carried;
      Result : CuBit.Channels.Open_Result;
      Refusal : CuBit.Channel_Protocol.Open_Refusal;
   begin
      S.Retry_At := Later (Now, Establish_Retry_Ms);
      if not CuBit.Handles.Valid (S.Desktop) then
         Msg.tag := (label => IR.Binding_Label, length => 0, protocol => 0, ipc => <>, reserved => 0);
         Returned := CuBit.Handles.IPC.Call_Accepting
           (Manager, Msg, CuBit.Messages.Deadline_After (Open_Limit_Ms), Carried);
         if Returned.label /= IR.Binding_Label or else Msg.words (0) /= IR.Bound then
            return;
         end if;
         declare
            Accepted : constant CuBit.Handles.IPC.Accepted_Endpoint :=
              CuBit.Handles.IPC.Accept_Endpoint (Carried);
         begin
            if not Accepted.OK then
               return;
            end if;
            S.Desktop := Accepted.Value;
         end;
      end if;
      CuBit.Channels.Open
        (S.Desktop, IR.Contract, CuBit.Channels.Producing, S.Channel, Result, Refusal,
         Deadline => CuBit.Messages.Deadline_After (Open_Limit_Ms));
      if Result /= CuBit.Channels.Opened then
         --  Refused or unanswered: perhaps a Desktop that is gone. Ask
         --  devmgr again next time.
         Forget_Desktop (S);
         return;
      end if;
      IR.Channels.Attach_Producer
        (S.Ring, Address_Of (S.Channel.Own_Base), Address_Of (S.Channel.Peer_Base));
      S.Open := True;
      S.Room_Armed := False;
      --  What the device reported while its first ring was being set up
      --  goes now, in order (a keyboard plugged in and typed on at once).
      Flush (S);
   end Establish;

   --  Push what was retained, oldest first, until the ring is full.
   procedure Flush (S : in out Source) is
      Item : Input_Pending.Item;
      Pushed, Ring, Rung : Boolean := False;
      Words : CuBit.Input.Report_Words;
   begin
      if not S.Open then
         return;
      end if;
      for Attempt in 1 .. Input_Pending.Capacity loop
         exit when Count (S.Retained) = 0;
         Item := Element (S.Retained, 0);
         Words := CuBit.Input.Encode_Words
           ((sourceIdentity => 0,
             sequence => Item.Sequence,
             generation => Generation,
             device => Device,
             delivery => Delivery,
             flags => [CuBit.Input.RESYNCHRONIZE => Item.Recover],
             payload => Item.Payload,
             snapshot => Input_Stamps.Snapshot (Buttons (Item), Item)),
            Item.Observed_Us);
         IR.Channels.Push (S.Ring, Words, Pushed, Ring);
         Rung := Rung or else Ring;
         if not Pushed then
            --  Back pressure: Desktop rings back when it takes. Room that
            --  appeared while arming is used at once.
            S.Room_Armed := IR.Channels.Arm_For_Room (S.Ring);
            exit when S.Room_Armed;
         else
            Acknowledge (S.Retained);
            Count_Up (S.Pushed);
         end if;
      end loop;
      if S.Room_Armed and then Count (S.Retained) = 0 then
         IR.Channels.Disarm_For_Room (S.Ring);
         S.Room_Armed := False;
      end if;
      if Rung then
         Wake_Desktop (S);
      end if;
   end Flush;

   procedure Publish (S : in out Source; R : Report; Observed_Us : Unsigned_64) is
      Lost : Boolean;
   begin
      Append (S.Retained, R, Observed_Us, Lost);
      if Lost then
         Count_Up (S.Lost);
         if S.Open then
            IR.Channels.Mark_Overflow (S.Ring);
         end if;
      end if;
      --  Without a ring yet, the report waits in the retention (bounded:
      --  its overflow marks the next report) until Service opens one.
      Flush (S);
   end Publish;

   procedure Device_Lost (S : in out Source) is
   begin
      Mark_Lost (S.Retained);
   end Device_Lost;

   procedure Service (S : in out Source; Manager : CuBit.Handles.Endpoint) is
   begin
      if not S.Open then
         if Now >= S.Retry_At then
            Establish (S, Manager);
         end if;
         return;
      end if;
      if S.Room_Armed then
         IR.Channels.Disarm_For_Room (S.Ring);
         S.Room_Armed := False;
      end if;
      Flush (S);
   end Service;

   function Next_Deadline (S : Source) return Unsigned_64 is
     (if not S.Open then S.Retry_At else Unsigned_64'Last);

   procedure Withdraw (S : in out Source) is
   begin
      if S.Open then
         Flush (S);
      end if;
      End_Ring (S);
      --  At once when the device comes back.
      S.Retry_At := 0;
   end Withdraw;

   procedure Note_Event
     (S : in out Source; Event : CuBit.Control_Events.Event; Ends : out Boolean) is
   begin
      Ends := S.Open and then CuBit.Channels.Ended (S.Channel, Event);
      if Ends then
         End_Ring (S);
      end if;
   end Note_Event;

   function Established (S : Source) return Boolean is (S.Open);
   function Published (S : Source) return Unsigned_64 is (S.Pushed);
   function Overflows (S : Source) return Unsigned_64 is (S.Lost);

end CuBit.Input_Source;
