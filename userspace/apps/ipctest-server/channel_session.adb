------------------------------------------------------------------------------
--  ipctest-server's channel session (CAP-002 step 3): serves IPCtest_Channel
--  channels through CuBit.Channel_Servers until OP_CHANNEL_END, then
--  reports what it served, refused and saw end.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Limits;
with CuBit.Channel_Servers;
with CuBit.Control_Events;
with CuBit.Deadlines;
with CuBit.Doorbells;
with CuBit.System_Facts;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Process_Events;
with CuBit.Handles;
with CuBit.Handles.IPC;
with System.Machine_Code;
with IPCtest_Channel;

procedure Channel_Session is
   package IC renames IPCtest_Channel;


   --  Every sender may open the one offer (the limits are what is tested).
   function Admit_All (Offer : Positive; From : Process_ID; Badge : Unsigned_64) return Boolean;
   function Admit_All (Offer : Positive; From : Process_ID; Badge : Unsigned_64) return Boolean is
      pragma Unreferenced (Badge);
   begin
      return Offer = 1 and then From /= No_Process;
   end Admit_All;

   --  Server_Limit: this test server can always serve one more.
   function Always (Offer : Positive) return Boolean is (Offer = 1);

   package Servers is new CuBit.Channel_Servers
     (RC => IC.RC, Offer_Count => 1,
      Admit => Admit_All, Can_Serve_Another => Always);

   --  Requests served per channel per batch.
   Budget : constant := 8;

   S : Servers.Server;
   Served_Count, Ended_Count : Unsigned_64 := 0;
   Held_Valid : Boolean := False;
   Held_Index : Servers.Index := Servers.Index'First;
   Held_Tag : IC.RC.Token := 0;
   Held_Value : Unsigned_64 := 0;

   function TSC return Unsigned_64 is
      Low, High : Unsigned_32;
   begin
      System.Machine_Code.Asm ("rdtsc", Outputs => (Unsigned_32'Asm_Output ("=a", Low),
                                                    Unsigned_32'Asm_Output ("=d", High)),
                               Volatile => True);
      return Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low);
   end TSC;

   type Sample_Array is array (1 .. IC.Samples) of Unsigned_64;
   Wake_Samples : Sample_Array := [others => 0];
   Wake_Count : Natural := 0;


   procedure Answer (I : Servers.Index; Tag : IC.RC.Token; Value : Unsigned_64) is
   begin
      Servers.Complete (S, I, Tag, (Value => Value * IC.Multiplier, Echoed => Value));
      Served_Count := Served_Count + 1;
   end Answer;

   procedure Serve (I : Servers.Index) is
      Item : IC.RC.Queues.Submission;
      Got : Boolean;
      Ignore : Unsigned_64;
   begin
      for K in 1 .. Budget loop
         Servers.Take (S, I, Item, Got);
         exit when not Got;
         if Item.Item.Kind = IC.Wake_Stamp then
            if Wake_Count < IC.Samples then
               Wake_Count := Wake_Count + 1;
               Wake_Samples (Wake_Count) := TSC - Item.Item.Value;
            end if;
            Answer (I, Item.Tag, Item.Item.Value);
         elsif Item.Item.Kind = IC.Ring_Stamp then
            Ignore := syscall (SYSCALL_SLEEP, IC.Hold_Ms);
            --  The answer carries the stamp taken just before the ring; the
            --  client reads it only after the ring wakes it.
            Servers.Complete (S, I, Item.Tag, (Value => TSC, Echoed => Item.Item.Value));
            Servers.Ring_Now (S, I);
            Served_Count := Served_Count + 1;
         elsif Item.Item.Kind = IC.Delayed_Stamp then
            Ignore := syscall (SYSCALL_SLEEP, IC.Hold_Ms);
            Servers.Complete (S, I, Item.Tag, (Value => TSC, Echoed => Item.Item.Value));
            Served_Count := Served_Count + 1;
         elsif Item.Item.Kind = IC.Silent and then not Held_Valid then
            Held_Valid := True;
            Held_Index := I;
            Held_Tag := Item.Tag;
            Held_Value := Item.Item.Value;
         elsif Item.Item.Kind = IC.Release then
            if Held_Valid then
               Answer (Held_Index, Held_Tag, Held_Value);
               Held_Valid := False;
            end if;
            Answer (I, Item.Tag, Item.Item.Value);
         else
            Answer (I, Item.Tag, Item.Item.Value);
         end if;
      end loop;
      Servers.Served (S, I);
   end Serve;

   Event : CuBit.Control_Events.Event;
   Found, Is_Open, Opened, Ended, Configured : Boolean;
   I : Servers.Index;

   --  Channel ends (grant notices) and rung grants.
   procedure Take_Events is
   begin
      loop
         CuBit.Process_Events.Next (Event, Found);
         exit when not Found;
         Servers.Handle_Event (S, Event, Ended, I);
         if Ended then
            Ended_Count := Ended_Count + 1;
            if Held_Valid and then Held_Index = I then
               Held_Valid := False;
            end if;
         end if;
      end loop;
   end Take_Events;
   From : Process_ID;
   Msg, Reply_Msg : Message;
   Woke : Servers.Wake := (Kind => Servers.Work_Waiting, others => <>);
   Ignore : Unsigned_64;

   function Thread_Calls return Unsigned_64 is
      Value : CuBit.System_Facts.SP.Statistics;
      Read : Boolean;
   begin
      CuBit.System_Facts.Read_Statistics (Value, Read);
      return (if Read then Value.Thread_System_Calls else 0);
   end Thread_Calls;
   Calls_At_Start : constant Unsigned_64 := Thread_Calls;

begin
   Servers.Configure
     (S, [1 => (Element => IC.Element,
                Per_Client => (Kind => CuBit.Channel_Limits.At_Most, Count => IC.Per_Client),
                Total => (Kind => CuBit.Channel_Limits.Server_Limit),
                Service => CuBit.Channel_Limits.Urgent)], Configured);
   if not Configured then
      return;
   end if;
   loop
      case Woke.Kind is
         when Servers.Message_Received =>
            From := Woke.From;
            Msg := Woke.Msg;
            if From = No_Process then
               --  A kernel event: a channel's end (grant notices).
               declare
                  Event : constant CuBit.Control_Events.Event :=
                    CuBit.Control_Events.Decode (Msg.tag.label, Msg.tag.length,
                                                 Msg.words (0), Msg.words (1), Msg.words (2));
               begin
                  Servers.Handle_Event (S, Event, Ended, I);
                  if Ended then
                     Ended_Count := Ended_Count + 1;
                     if Held_Valid and then Held_Index = I then
                        Held_Valid := False;
                     end if;
                  end if;
               end;
            else
            if Msg.tag.label = IC.OP_CHANNEL_STATS then
               --  Sort the samples (few: insertion sort).
               for A in 2 .. Wake_Count loop
                  declare
                     V : constant Unsigned_64 := Wake_Samples (A);
                     B : Natural := A - 1;
                  begin
                     while B >= 1 and then Wake_Samples (B) > V loop
                        Wake_Samples (B + 1) := Wake_Samples (B);
                        B := B - 1;
                     end loop;
                     Wake_Samples (B + 1) := V;
                  end;
               end loop;
               Reply_Msg := NULL_MESSAGE;
               Reply_Msg.tag := (label => 16#F000#, length => 3, protocol => 0, ipc => <>, reserved => 0);
               if Wake_Count > 0 then
                  Reply_Msg.words (0) := Wake_Samples (1);
                  Reply_Msg.words (1) := Wake_Samples ((Wake_Count + 1) / 2);
                  Reply_Msg.words (2) := Wake_Samples (Natural'Max (1, (Wake_Count * 99) / 100));
               end if;
               Wake_Count := 0;
               Ignore := reply (From, Reply_Msg);
               goto Next_Request;
            end if;
            if Msg.tag.label = IC.OP_CHANNEL_DELAYED then
               declare
                  Saved : constant CuBit.Handles.IPC.Saved_Reply := CuBit.Handles.IPC.Save_Reply;
                  Late : Message := NULL_MESSAGE;
               begin
                  if Saved.Saved then
                     Ignore := syscall (SYSCALL_SLEEP, IC.Hold_Ms);
                     Late.tag := (label => 16#F000#, length => 1, protocol => 0, ipc => <>, reserved => 0);
                     Late.words (0) := TSC;
                     Ignore := replyCap (Saved.Handle, Late);
                  end if;
               end;
               goto Next_Request;
            end if;
            if Msg.tag.label = IC.OP_CHANNEL_PING then
               Reply_Msg := NULL_MESSAGE;
               Reply_Msg.tag := (label => 16#F000#, length => 1, protocol => 0, ipc => <>, reserved => 0);
               Reply_Msg.words (0) := Msg.words (0) * IC.Multiplier;
               Ignore := reply (From, Reply_Msg);
               goto Next_Request;
            end if;
            if Msg.tag.label = IC.OP_CHANNEL_COUNT then
               Reply_Msg := NULL_MESSAGE;
               Reply_Msg.tag := (label => 16#F000#, length => 1, protocol => 0, ipc => <>, reserved => 0);
               Reply_Msg.words (0) := Thread_Calls - Calls_At_Start;
               Ignore := reply (From, Reply_Msg);
               goto Next_Request;
            end if;
            if Msg.tag.label = IC.OP_CHANNEL_END then
               --  Ends the client caused before it asked count too.
               Take_Events;
               Reply_Msg := NULL_MESSAGE;
               Reply_Msg.tag := (label => 16#F000#, length => 4, protocol => 0, ipc => <>, reserved => 0);
               Reply_Msg.words (IC.Report_Server_Calls) := Thread_Calls - Calls_At_Start;
               Reply_Msg.words (IC.Report_Served) := Served_Count;
               Reply_Msg.words (IC.Report_Limit) := Servers.Refusals (S).Limit;
               Reply_Msg.words (IC.Report_Ended) := Ended_Count;
               Ignore := reply (From, Reply_Msg);
               return;
            end if;
            Servers.Handle_Open (S, From, Msg, Is_Open, Reply_Msg, Opened, I);
            if not Is_Open then
               Reply_Msg := NULL_MESSAGE;
               Reply_Msg.tag := (label => 16#F001#, length => 1, protocol => 0, ipc => <>, reserved => 0);
               Reply_Msg.words (0) := Unsigned_64 (Msg.tag.label);
            end if;
            Ignore := reply (From, Reply_Msg);
            <<Next_Request>>
            null;
            end if;
         when others =>
            null;
      end case;
      Servers.Refresh (S);
      loop
         Servers.Next (S, Found, I);
         exit when not Found;
         Serve (I);
      end loop;
      --  No idle deadline: every request, open and end wakes the session
      --  (and the test profile publishes no clock, so computing one would
      --  cost a GETTIME per sleep).
      Servers.Sleep (S, CuBit.Deadlines.Forever, Woke);
   end loop;
end Channel_Session;
