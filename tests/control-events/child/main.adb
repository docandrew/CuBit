------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The child of tests/control-events. It reports "ready" on the outlet its
--  launcher lent it, then waits for two things in order: the launcher's
--  revoke of that ring, which the runtime answers by itself (the outlet
--  closes and the mapping is returned), and a Stop control message.
--  First it reads control-producer.app's outlet through a channel (the
--  ipc-test service), closes it and opens it again.
--  Then it attacks the producer as a hostile peer would: forged kernel
--  events, malformed or mismatched opens, closes of channels it does not
--  hold; its own channel must keep working.
--  Exit codes: 0 both seen, 2 the ring stayed open, 3 no Stop, 4 the ring
--  was not lent, 5 the launcher's outlet could not be read, 6 an attack
--  succeeded.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CCL_Manifest_Bindings;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Child_Exits;
with CuBit.Channels;
with CuBit.Control_Events;
with CuBit.Outlet_Channels;
with CuBit.Protocols;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Process_Events;
with CuBit.Streams;

procedure Main is
   package CV renames CuBit.Control_Events;
   use type CV.Event_Kind;
   use type CV.Control_Kind;
   use type CuBit.Streams.StreamId;

   Seen_Both : constant := 0;
   Ring_Stayed_Open : constant := 2;
   No_Stop : constant := 3;
   Not_Lent : constant := 4;
   Unread : constant := 5;
   Attacked : constant := 6;
   Poll_Milliseconds : constant := 10;
   Wait_Milliseconds : constant := 10_000;
   Microseconds_Per_Millisecond : constant := 1_000;

   Ready : constant String := "ready";
   Out_Stream : CuBit.Streams.StreamId;
   Item : CV.Event;
   Found : Boolean;
   Waited : Natural := 0;

   procedure Pause is
      Ignore : Unsigned_64;
   begin
      Ignore := CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Sleep_Until_Monotonic_Microsecond,
         CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Read_Monotonic_Microseconds)
           + Poll_Milliseconds * Microseconds_Per_Millisecond);
      Waited := Waited + Poll_Milliseconds;
   end Pause;

   procedure Finish (Code : Unsigned_64) is
      Ignore : Unsigned_64;
   begin
      debugPrint ("control-child: exit" & Code'Image & ASCII.LF);
      Ignore := CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Exit_Process, Code);
   end Finish;

   function Open return Boolean is
     (CuBit.Streams.streamWrite
        (Out_Stream, Ready'Address, Ready'Length, CuBit.Streams.TYPE_TEXT_LINE) /= 0);
   --  Open a channel on the producer's outlet 1 and take one record.
   function Read_Launcher return Boolean is
      use type CuBit.Channels.Open_Result;
      use type CuBit.Channels.Take_Result;
      Link : CuBit.Channels.Channel;
      Opened : CuBit.Channels.Open_Result;
      Ignore_Refusal : CuBit.Channel_Protocol.Open_Refusal;
      Buffer : String (1 .. 64);
      Length : Natural;
      Taken : CuBit.Channels.Take_Result;
   begin
      CuBit.Channels.Open
        (CuBit.Messages.CapabilitySlot (CCL_Manifest_Bindings.Slot_ipc_test),
         CuBit.Outlet_Channels.Offer (CuBit.Protocols.TEXT_LINE_CONTRACT),
         CuBit.Channels.Consuming, Link, Opened, Ignore_Refusal, Connector => 1);
      if Opened /= CuBit.Channels.Opened then
         return False;
      end if;
      Waited := 0;
      loop
         CuBit.Channels.Take (Link, Buffer'Address, Buffer'Length, Length, Taken);
         exit when Taken = CuBit.Channels.Taken;
         Pause;
         if Waited > Wait_Milliseconds then
            CuBit.Channels.Close (Link);
            return False;
         end if;
      end loop;
      CuBit.Channels.Close (Link);
      return Buffer (1 .. Length) = "from the producer";
   end Read_Launcher;
   --  The attacks (docs/data-plane.md). True when all were refused and the
   --  child's own channel still reads afterwards.
   function Attacks_Refused return Boolean is
      use type CuBit.Channels.Open_Result;
      use type CuBit.Channels.Take_Result;
      package CP renames CuBit.Channel_Protocol;
      Endpoint : constant CuBit.Messages.CapabilitySlot :=
        CuBit.Messages.CapabilitySlot (CCL_Manifest_Bindings.Slot_ipc_test);
      Producer : constant Unsigned_64 := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_IPCTEST);
      Offer : constant CuBit.Channel_Contracts.Contract :=
        CuBit.Outlet_Channels.Offer (CuBit.Protocols.TEXT_LINE_CONTRACT);
      Link, Probe : CuBit.Channels.Channel;
      Opened : CuBit.Channels.Open_Result;
      Refusal : CuBit.Channel_Protocol.Open_Refusal;
      Buffer : String (1 .. 64);
      Length : Natural;
      Taken : CuBit.Channels.Take_Result;
      M : Message;
      Tag : MessageTag;
      Ignore : Boolean;

      --  SEND_EVENT with a kernel label: the kernel must refuse it.
      function Forged (Label : Unsigned_32) return Boolean is
         Length_Shift : constant := 32;
         Words : constant := 3;
      begin
         return syscall (SYSCALL_SEND_EVENT, Producer,
                         Unsigned_64 (Label) or Shift_Left (Unsigned_64 (Words), Length_Shift),
                         1, 1, Producer, 0) /= Unsigned_64'Last;
      end Forged;

      function Say (Text : String; Refused : Boolean) return Boolean is
      begin
         debugPrint ("control-child: " & Text & (if Refused then " refused" else " ACCEPTED") & ASCII.LF);
         return Refused;
      end Say;
      All_Refused : Boolean := True;
      type Foreign_Numbers is array (1 .. 3) of Unsigned_64;
   begin
      CuBit.Channels.Open (Endpoint, Offer, CuBit.Channels.Consuming, Link, Opened, Refusal,
                           Connector => 1);
      if Opened /= CuBit.Channels.Opened or else Producer = 0 then
         return False;
      end if;
      --  Forged kernel events.
      All_Refused := Say ("forged grant-returned event",
                          not Forged (CuBit.Control_Events.Grant_Returned_Label)) and All_Refused;
      All_Refused := Say ("forged grant-revoked event",
                          not Forged (CuBit.Control_Events.Grant_Revoked_Label)) and All_Refused;
      All_Refused := Say ("forged Stop control message",
                          not Forged (CuBit.Control_Events.Control_Label)) and All_Refused;
      All_Refused := Say ("forged child exit",
                          not Forged (CuBit.Child_Exits.Event_Label)) and All_Refused;
      --  Malformed and mismatched opens.
      M := NULL_MESSAGE;
      M.tag := (label => CP.OP_OPEN_CONSUMING, length => CP.Open_Words, flags => 0, reserved => 1);
      M.words := [Unsigned_64'Last, Unsigned_64'Last, Unsigned_64'Last, 0];
      Tag := capCall (Endpoint, M, Wait_Forever);
      All_Refused := Say ("malformed contract", Tag.label /= CuBit.Kernel_ABI.Reply_OK) and All_Refused;
      CuBit.Channels.Open (Endpoint, Offer, CuBit.Channels.Consuming, Probe, Opened, Refusal,
                           Connector => 9);
      All_Refused := Say ("unknown connector", Opened /= CuBit.Channels.Opened) and All_Refused;
      CuBit.Channels.Open (Endpoint, (Offer with delta Policy => CuBit.Channel_Contracts.Lossless),
                           CuBit.Channels.Consuming, Probe, Opened, Refusal, Connector => 1);
      All_Refused := Say ("lossless open of a broadcast outlet", Opened /= CuBit.Channels.Opened)
                       and All_Refused;
      CuBit.Channels.Open (Endpoint, Offer, CuBit.Channels.Producing, Probe, Opened, Refusal,
                           Connector => 1);
      All_Refused := Say ("producing into someone's outlet", Opened /= CuBit.Channels.Opened)
                       and All_Refused;
      --  Closes of channels this process does not hold (they must change
      --  nothing; there is no reply to judge by).
      for Number of Foreign_Numbers'(0, Link.Peer_Number + 1, Unsigned_64'Last) loop
         M := NULL_MESSAGE;
         M.tag := (label => CP.OP_CLOSE, length => 1, flags => 0, reserved => 0);
         M.words (0) := Number;
         Ignore := capSubmit (Endpoint, M, NO_COMPLETION_TOKEN);
      end loop;
      --  Its own channel still reads.
      Waited := 0;
      loop
         CuBit.Channels.Take (Link, Buffer'Address, Buffer'Length, Length, Taken);
         exit when Taken = CuBit.Channels.Taken;
         Pause;
         if Waited > Wait_Milliseconds then
            CuBit.Channels.Close (Link);
            return False;
         end if;
      end loop;
      CuBit.Channels.Close (Link);
      return All_Refused and then Buffer (1 .. Length) = "from the producer";
   end Attacks_Refused;
   --  A flood cannot crowd out the kernel's events: take every reader slot,
   --  fill the producer's mailbox with junk, let go of the slots (the
   --  closes cannot get in; only the kernel's grant-returned events tell
   --  the producer), then open again.
   function Flood_Survived return Boolean is
      use type CuBit.Channels.Open_Result;
      Endpoint : constant CuBit.Messages.CapabilitySlot :=
        CuBit.Messages.CapabilitySlot (CCL_Manifest_Bindings.Slot_ipc_test);
      Offer : constant CuBit.Channel_Contracts.Contract :=
        CuBit.Outlet_Channels.Offer (CuBit.Protocols.TEXT_LINE_CONTRACT);
      Links : array (1 .. CuBit.Streams.MAX_SUBSCRIBERS) of CuBit.Channels.Channel;
      Opened : CuBit.Channels.Open_Result;
      Refusal : CuBit.Channel_Protocol.Open_Refusal;
      Junk : Message := NULL_MESSAGE;
      Junk_Label : constant := 16#7E57#;
      Flooded : Natural := 0;
      Flood_Limit : constant := 256;
      Own_Credit : constant := 16;
      Passes_Per_Junk : constant := 3;
      Drain_Margin : constant := 10;
   begin
      for L of Links loop
         CuBit.Channels.Open (Endpoint, Offer, CuBit.Channels.Consuming, L, Opened, Refusal,
                              Connector => 1);
         if Opened /= CuBit.Channels.Opened then
            return False;
         end if;
      end loop;
      Junk.tag := (label => Junk_Label, length => 0, flags => 0, reserved => 0);
      while Flooded < Flood_Limit and then capSubmit (Endpoint, Junk, NO_COMPLETION_TOKEN) loop
         Flooded := Flooded + 1;
      end loop;
      debugPrint ("control-child: flooded the producer's mailbox with" & Flooded'Image &
                  " requests" & ASCII.LF);
      for L of Links loop
         CuBit.Channels.Close (L);
      end loop;
      --  Every slot comes back: the kernel keeps each reader's end for the
      --  producer until it reads it, however full its mailbox
      --  (docs/ipc-delivery.md).
      Waited := 0;
      for L of Links loop
         loop
            CuBit.Channels.Open (Endpoint, Offer, CuBit.Channels.Consuming, L, Opened, Refusal,
                                 Connector => 1);
            exit when Opened = CuBit.Channels.Opened;
            Pause;
            if Waited > Wait_Milliseconds then
               return False;
            end if;
         end loop;
      end loop;
      for L of Links loop
         CuBit.Channels.Close (L);
      end loop;
      --  Let the producer drain this process's junk before the next steps:
      --  it takes one request per pass, alternating with the launcher's.
      for Drain in 1 .. Passes_Per_Junk * Flooded + Drain_Margin loop
         Pause;
      end loop;
      --  Isolation (docs/ipc-delivery.md, step 3): the launcher keeps its
      --  own ring at the producer full all along, yet this process's own
      --  ring took a full credit (the kernel's QUEUE_CREDIT). A shared
      --  queue would have refused it almost at once.
      return Flooded >= Own_Credit and then Flooded < Flood_Limit;
   end Flood_Survived;
begin
   if not Flood_Survived then
      Finish (Attacked);
      return;
   end if;
   debugPrint ("control-child: a flooded producer still freed every reader slot" & ASCII.LF);
   if not Attacks_Refused then
      Finish (Attacked);
      return;
   end if;
   debugPrint ("control-child: every attack on the producer was refused" & ASCII.LF);
   if not Read_Launcher then
      Finish (Unread);
      return;
   end if;
   debugPrint ("control-child: read the producer's outlet through a channel" & ASCII.LF);
   --  Each close frees its slot at the producer: more opens than it has
   --  reader slots all succeed.
   for Again in 1 .. CuBit.Streams.MAX_SUBSCRIBERS + 1 loop
      if not Read_Launcher then
         Finish (Unread);
         return;
      end if;
   end loop;
   debugPrint ("control-child: closed and reopened the channel past the reader limit" & ASCII.LF);
   Out_Stream := CuBit.Streams.Open_Outlet ("unix.stdout");
   if Out_Stream = CuBit.Streams.NO_STREAM or else not Open then
      Finish (Not_Lent);
      return;
   end if;
   --  The revoke: handled by the runtime; the outlet then refuses writes.
   while Open loop
      CuBit.Process_Events.Poll;
      Pause;
      if Waited > Wait_Milliseconds then
         Finish (Ring_Stayed_Open);
         return;
      end if;
   end loop;
   debugPrint ("control-child: the ring was revoked and returned" & ASCII.LF);
   --  The launcher sent an Interrupt, a Reload and a Stop before this
   --  reads any: all three are kept until read (docs/ipc-delivery.md).
   Waited := 0;
   declare
      Seen : array (CV.Control_Kind) of Boolean := [others => False];
   begin
      loop
         CuBit.Process_Events.Next (Item, Found);
         if Found and then Item.Kind = CV.Control then
            Seen (Item.Control) := True;
         end if;
         exit when (for all K in CV.Control_Kind => Seen (K));
         if not Found then
            Pause;
         end if;
         if Waited > Wait_Milliseconds then
            Finish (No_Stop);
            return;
         end if;
      end loop;
   end;
   debugPrint ("control-child: Interrupt, Reload and Stop all received" & ASCII.LF);
   debugPrint ("control-child: Stop received" & ASCII.LF);
   Finish (Seen_Both);
end Main;
