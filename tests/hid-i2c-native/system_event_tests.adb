pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with Channel_Wake_ABI;
with CuBit.Channel_Bells;
with CuBit.Control_Events;
with CuBit.Grant_References;
with CuBit.Messages;
with CuBit.Process_IDs;
with HID_System_Events;
with HID_Native_System_Events;
with HID_Native_Clock;
with HID_Native_Queues;
with HID_Log_Lifetime;
procedure System_Event_Tests is
   package M renames CuBit.Messages;
   package P renames CuBit.Process_IDs;
   package CV renames CuBit.Control_Events;
   package G renames CuBit.Grant_References;
   package Policy renames HID_System_Events;
   use type Policy.Action, P.Process_ID, Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_32;
   use type M.IPC_Flags, M.MessageWords;
   Checks, Grant_Calls : Natural := 0;
   Peer : constant P.Process_ID := P.From_Word (16#F123_4567_0000_ABCD#);
   Replacement : constant P.Process_ID := P.From_Word (16#E123_4567_0000_ABCD#);
   Last_Grant : CV.Event;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   procedure Grant_Notice (Item : CV.Event) is
   begin Grant_Calls := Grant_Calls + 1; Last_Grant := Item; end Grant_Notice;
   package Events is new HID_Native_System_Events (Grant_Notice);
   Item : M.Message := M.NULL_MESSAGE;
   Stop, Handled : Boolean;
   type Labels is array (Positive range <>) of Interfaces.Unsigned_32;
   Known : constant Labels := [Channel_Wake_ABI.Wake_Label,
      CV.Grant_Revoked_Label, CV.Grant_Returned_Label, CV.Control_Label];
   Expected : Policy.Action;
begin
   Check (Policy.Classify (Peer, Item) = Policy.Not_System);
   Events.Handle (Peer, Item, Stop, Handled); Check (not Stop and then not Handled);
   for Label of Known loop
      for Length in 0 .. 255 loop
         Item := M.NULL_MESSAGE; Item.tag.label := Label;
         Item.tag.length := Interfaces.Unsigned_8 (Length);
         if Label = Channel_Wake_ABI.Wake_Label then
            Item.words := [others => G.Encode ((1, 1))];
            Expected := (if Length in 1 .. Channel_Wake_ABI.Wakes_Per_Event then Policy.Channel_Wake else Policy.Rejected);
         elsif Label = CV.Control_Label then
            Item.words (0) := 1; Item.words (1) := P.To_Word (Peer);
            Expected := (if Length = CV.Control_Words then Policy.Stop_After_Drain else Policy.Rejected);
         else
            Item.words (0) := 1; Item.words (1) := 1; Item.words (2) := P.To_Word (Peer);
            Expected := (if Length = CV.Grant_Event_Words then Policy.Grant_Notice else Policy.Rejected);
         end if;
         Check (Policy.Classify (P.No_Process, Item) = Expected);
         Check (Policy.Classify (Peer, Item) = Policy.Rejected);
         Check (Policy.Classify (Replacement, Item) = Policy.Rejected);
         Item.tag.protocol := 1; Check (Policy.Classify (P.No_Process, Item) = Policy.Rejected); Item.tag.protocol := 0;
         Item.tag.reserved := 1; Check (Policy.Classify (P.No_Process, Item) = Policy.Rejected); Item.tag.reserved := 0;
         Item.tag.ipc.Capability_Offered := True;
         Check (Policy.Classify (P.No_Process, Item) = Policy.Rejected);
      end loop;
   end loop;
   Item := M.NULL_MESSAGE; Item.tag.label := CV.Control_Label; Item.tag.length := CV.Control_Words;
   for Kind in Interfaces.Unsigned_64 range 0 .. 4 loop
      Item.words (0) := Kind; Item.words (1) := P.To_Word (Peer);
      Events.Handle (P.No_Process, Item, Stop, Handled);
      Check (Handled and then Stop = (Kind = 1));
   end loop;
   for Label of Known (2 .. 3) loop
      Item := M.NULL_MESSAGE; Item.tag.label := Label; Item.tag.length := CV.Grant_Event_Words;
      Item.words := [G.Global_Slot'Last, G.Generation'Last, P.To_Word (Peer), 0];
      Events.Handle (P.No_Process, Item, Stop, Handled);
      Check (Handled and then not Stop and then Last_Grant.Peer = Peer);
      Check (Last_Grant.Slot = G.Global_Slot'Last and then Last_Grant.Generation = G.Generation'Last);
      Item.words (1) := 0; Check (Policy.Classify (P.No_Process, Item) = Policy.Rejected);
      Item.words (1) := G.Generation'Last + 1; Check (Policy.Classify (P.No_Process, Item) = Policy.Rejected);
      Item.words (1) := 1; Item.words (0) := G.Global_Slot'Last + 1;
      Check (Policy.Classify (P.No_Process, Item) = Policy.Rejected);
   end loop;
   Check (Grant_Calls = 2);
   Item := M.NULL_MESSAGE; Item.tag.label := Channel_Wake_ABI.Wake_Label;
   Item.tag.length := Channel_Wake_ABI.Wakes_Per_Event; Item.words := [others => G.Encode ((1, 1))];
   Events.Handle (P.No_Process, Item, Stop, Handled);
   Check (Handled and then not Stop and then CuBit.Channel_Bells.Calls = 1);
   for Index in Item.words'Range loop
      Item.words (Index) := 0; Events.Handle (P.No_Process, Item, Stop, Handled);
      Check (Handled and then not Stop and then CuBit.Channel_Bells.Calls = 1);
      Item.words (Index) := G.Encode ((1, 1));
   end loop;
   Check (Grant_Calls = 2);
   declare
      Now : Interfaces.Unsigned_64;
      Available, Found : Boolean;
      Publisher : P.Process_ID;
      Message : M.Message;
      Completion : M.CompletionEntry;
   begin
      HID_Native_Clock.Cached_Time (Now, Available); Check (not Available);
      M.Clock_Value := 50; HID_Native_Clock.Sample_Time (Now, Available);
      Check (Available and then Now = 50);
      HID_Native_Clock.Cache_Time (Now, False);
      HID_Native_Clock.Cached_Time (Now, Available); Check (Now = 50 and then not Available);
      M.Clock_Value := Interfaces.Unsigned_64'Last;
      HID_Native_Clock.Sample_Time (Now, Available); Check (not Available);
      M.Clock_Value := 0; HID_Native_Clock.Sample_Time (Now, Available);
      Check (Available and then Now = 0);
      HID_Native_Clock.Cache_Time (Now, Available);
      HID_Native_Clock.Cached_Time (Now, Available); Check (Available and then Now = 0);
      M.Queued_Peer := Peer; M.Queued_Message := Item; M.Event_Present := True;
      HID_Native_Queues.Poll_Event (Publisher, Message, Found);
      Check (Found and then Publisher = Peer and then Message.words = Item.words);
      HID_Native_Queues.Poll_Event (Publisher, Message, Found);
      Check (not Found and then Publisher = P.No_Process);
      M.Queued_Peer := Replacement; M.Queued_Message.tag.ipc.Capability_Offered := True;
      M.Request_Present := True; M.Reply_Status := 0;
      HID_Native_Queues.Drain_Request (Found);
      Check (Found and then M.Reply_Calls = 1 and then M.Reply_Peer = Replacement);
      Check (M.Reply_Message.tag.ipc = M.No_IPC_Flags and then Natural (M.Reply_Message.tag.length) = 0);
      Check (HID_Native_Queues.Last_Reply_Status = 0);
      HID_Native_Queues.Drain_Request (Found); Check (not Found and then M.Reply_Calls = 1);
      M.Request_Present := True; M.Reply_Status := Interfaces.Unsigned_64'Last;
      HID_Native_Queues.Drain_Request (Found);
      Check (Found and then M.Reply_Calls = 2 and then HID_Native_Queues.Last_Reply_Status = Interfaces.Unsigned_64'Last);
      M.Completion_Count := 0; HID_Native_Queues.Poll_Completion (Completion, Found);
      Check (not Found and then not Completion.valid);
      M.Completion_Count := 1; HID_Native_Queues.Poll_Completion (Completion, Found);
      Check (Found and then not Completion.valid); -- model leaves entry zeroed
   end;
   declare
      type Peers is array (Positive range <>) of P.Process_ID;
      Known : constant Peers := [P.No_Process, Peer, Replacement];
      function Notice (Kind : CV.Event_Kind; From : P.Process_ID) return CV.Event is
      begin
         case Kind is
            when CV.Grant_Revoked => return (CV.Grant_Revoked, 0, 1, From);
            when CV.Grant_Returned => return (CV.Grant_Returned, 0, 1, From);
            when CV.Control => return (CV.Control, CV.Stop, From);
            when CV.Not_Ours => return (Kind => CV.Not_Ours);
         end case;
      end Notice;
   begin
      for Bound of Known loop
         for From of Known loop
            for Kind in CV.Event_Kind loop
               Check (HID_Log_Lifetime.Related (Bound, Notice (Kind, From)) =
                  (P.Is_Process (Bound) and then Bound = From and then
                   Kind in CV.Grant_Revoked | CV.Grant_Returned));
            end loop;
         end loop;
      end loop;
      -- Exact high lifetime matters even for the same apparent low slot.
      Check (not HID_Log_Lifetime.Related (Peer, (CV.Grant_Returned, 0, 1, Replacement)));
      Check (HID_Log_Lifetime.Related (Peer, (CV.Grant_Returned, 0, 1, Peer)));
   end;
   Ada.Text_IO.Put_Line ("HID system event checks" & Checks'Image);
end System_Event_Tests;
