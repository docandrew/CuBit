--  The log-authority guest test (tests/headless, log-authority): typed
--  logging's authority and its publisher rings (docs/logstore-architecture.md,
--  step 1; backlog LOG-001). Started trusted, it is an observer as well as a
--  publisher; it then launches itself as an ordinary child, which is only a
--  publisher.
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Logging;
with CuBit.Memory_Grants;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Grant_References;
with CuBit.Kernel_ABI;
with CuBit.Log_Publish_Rings;

procedure Main is
   package P renames CuBit.Log_Protocol;
   package L renames CuBit.Log_Records;
   package G renames CuBit.Memory_Grants;
   use type P.Status;
   --  Records published in the delivery check, all read back in order.
   Delivered : constant := 19;
   --  A burst well past logstore's per-pool budget (Log_Budgets.Burst,
   --  2048): logstore sheds the excess and says so.
   Burst_Records : constant := 3_000;
   Reader : CuBit.Logging.Reader;
   Writer : CuBit.Logging.Publisher;
   Value : P.Event;
   Result : P.Status;
   Lost, Ignore : Unsigned_64;
   Msg : Message;
   Tag : MessageTag;
   Submitted, Drained, Created : Boolean;
   type Page is array (Positive range 1 .. 4096) of Unsigned_8
     with Alignment => 4096;
   Buffer : Page := [others => 0];
   Grant : G.Grant_Reference;
   Never_Issued_Handle : constant Unsigned_64 := 16#DEAD_0000#;
   Saw_Clock : Boolean := False;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         debugPrint ("TEST: FAIL log-authority: " & Name & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT, 1);
         loop
            Ignore := syscall (SYSCALL_SLEEP, 1000);
         end loop;
      end if;
   end Check;

   function Request (Op : P.Operation) return Message is
      Item : Message := NULL_MESSAGE;
   begin
      Item.tag := (P.Operation'Enum_Rep (Op), 4, 0, 0);
      return Item;
   end Request;

   package CP renames CuBit.Channel_Protocol;

   --  A raw request to open a publishing log channel, with Grant (wire form)
   --  as the publisher's data region.
   function Open_Publishing (Grant : Unsigned_64) return Message is
      Words : constant CuBit.Channel_Contracts.Words :=
        CuBit.Channel_Contracts.Encode (CuBit.Log_Publish_Rings.CONTRACT);
      Item : Message := NULL_MESSAGE;
   begin
      Item.tag := (CP.OP_OPEN_PRODUCING, CP.Open_Words, 0, 0);
      Item.words := [Words (0), Words (1), Words (2), Grant];
      return Item;
   end Open_Publishing;

   --  One record into the channel: no IPC after the first (the open), no
   --  waiting.
   procedure Publish_Record (Text : String := "native log check") is
      Record_Value : constant L.Decoded := L.Make (Text);
   begin
      Check (Record_Value.Success, "make record");
      CuBit.Logging.Emit (Writer, Record_Value.Value, Submitted);
      Check (Submitted, "record written into the ring");
   end Publish_Record;

   --  Far more records than the budget admits, as fast as possible: the
   --  publisher never waits, and what logstore cannot keep is shed and
   --  counted, never refused back.
   procedure Check_Burst is
      Record_Value : constant L.Decoded := L.Make ("burst check");
      Start : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Written : Natural := 0;
   begin
      for I in 1 .. Burst_Records loop
         CuBit.Logging.Emit (Writer, Record_Value.Value, Submitted);
         if Submitted then
            Written := Written + 1;
         end if;
      end loop;
      debugPrint ("log-check: burst of" & Natural'Image (Burst_Records) & " records in" &
                  Unsigned_64'Image (syscall (SYSCALL_GETTIME) - Start) & " ms, ring shed" &
                  Unsigned_64'Image (CuBit.Logging.Dropped (Writer)) & ASCII.LF);
      Check (Written + Natural (CuBit.Logging.Dropped (Writer)) = Burst_Records,
             "every burst record written or counted");
      CuBit.Logging.Flush (Writer, Drained, Wait_Ms => 2_000);
      Check (Drained, "logstore drains the ring");
      debugPrint ("TEST: PASS log-burst" & ASCII.LF);
   end Check_Burst;

   procedure Check_Disconnect is
      Logger : CuBit.Logging.Publisher;
      Fresh : CuBit.Logging.Publisher;
      Record_Value : constant L.Decoded := L.Make ("disconnect check");
      Done : Boolean;
      Deadline : constant Unsigned_64 := syscall (SYSCALL_GETTIME) + 2000;
   begin
      CuBit.Logging.Disconnect (Fresh, Done);
      Check (Done, "unused publisher disconnects immediately");
      CuBit.Logging.Emit (Fresh, Record_Value.Value, Submitted);
      Check (not Submitted, "disconnected publisher cannot reconnect");
      CuBit.Logging.Emit (Logger, Record_Value.Value, Submitted);
      Check (Submitted, "disconnect fixture written");
      loop
         --  logstore drains and returns the ring (Detach); then the grant
         --  retires.
         CuBit.Logging.Disconnect (Logger, Done);
         exit when Done;
         Check (syscall (SYSCALL_GETTIME) < Deadline, "disconnect timeout");
         Ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;
      CuBit.Logging.Emit (Logger, Record_Value.Value, Submitted);
      Check (not Submitted, "disconnect stops new publication");
      CuBit.Logging.Disconnect (Logger, Done);
      Check (Done, "disconnect idempotent");
      debugPrint ("TEST: PASS log-disconnect" & ASCII.LF);
   end Check_Disconnect;

   --  Slot 18 is log-retire (the fault fixture): it maps the ring region
   --  and exits without answering Attach.
   procedure Check_Collector_Death is
      Logger : CuBit.Logging.Publisher (Slot => 18);
      Stale : CuBit.Logging.Publisher (Slot => 18);
      Record_Value : constant L.Decoded := L.Make ("collector death check");
      Done : Boolean;
      Deadline : constant Unsigned_64 := syscall (SYSCALL_GETTIME) + 2000;
   begin
      CuBit.Logging.Emit (Logger, Record_Value.Value, Submitted);
      Check (not Submitted and then CuBit.Logging.Dropped (Logger) = 1,
             "a collector dying in Attach costs one counted record, no wait");
      --  Its mapping retires with it; then the grant does.
      loop
         CuBit.Logging.Disconnect (Logger, Done);
         exit when Done;
         Check (syscall (SYSCALL_GETTIME) < Deadline, "retirement timeout");
         Ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;
      CuBit.Logging.Emit (Stale, Record_Value.Value, Submitted);
      Check (not Submitted, "dead endpoint cannot take a new ring");
      CuBit.Logging.Disconnect (Stale, Done);
      Check (Done, "failed fresh binding has no grant to retire");
      debugPrint ("TEST: PASS log-collector-death" & ASCII.LF);
   end Check_Collector_Death;
begin
   --  The ordinary child requests exactly the same ELF manifest, but cannot
   --  opt into trusted-startup approval. Its observer endpoint stays absent.
   CuBit.Logging.Subscribe (Reader, Result);
   if Result /= P.OK then
      Check (Result = P.Unavailable, "ordinary launch has no observer");
      Msg := Request (P.Subscribe);
      Msg.authorityTag := P.Observer_Authority_Tag;
      Tag := capCall (P.Publisher_Slot, Msg, CuBit.Messages.Wait_Forever);
      Check (Tag.label = P.Status'Enum_Rep (P.Denied) and then
             Msg.words = [0, 0, 0, 0], "forged observer tag denied");
      Publish_Record;
      Check_Burst;
      Check_Disconnect;
      Check_Collector_Death;
      debugPrint ("TEST: PASS log-unapproved" & ASCII.LF);
      return;
   end if;

   loop
      CuBit.Logging.Read_Next (Reader, Value, Lost, Result);
      exit when Result = P.Empty;
      Check (Result = P.OK, "initial retained records");
      if L.Text (Value.Data) = "clock: service ready" then
         Saw_Clock := Value.Source =
           To_Word (Registered_Driver (DRIVER_CLOCK)) and then
           P.Is_Publisher (Value.Publication_Tag);
      end if;
   end loop;
   Check (Saw_Clock, "native clock emitted authenticated diagnostics");

   Msg := Request (P.Subscribe);
   Msg.authorityTag := P.Observer_Authority_Tag;
   Tag := capCall (P.Publisher_Slot, Msg, CuBit.Messages.Wait_Forever);
   Check (Tag.label = P.Status'Enum_Rep (P.Denied) and then
          Msg.words = [0, 0, 0, 0], "publisher cannot observe with forged tag");
   Msg := Open_Publishing (0);
   Tag := capCall (P.Observer_Slot, Msg, CuBit.Messages.Wait_Forever);
   Check (Tag.label = CuBit.Kernel_ABI.Reply_Error, "observer cannot publish");
   --  Subscribing again renews the lease and keeps the stream.
   CuBit.Logging.Subscribe (Reader, Result);
   Check (Result = P.OK, "idempotent subscribe");

   --  A region that is not the channel's: refused before it is used.
   G.Create_Via_Capability
     (P.Publisher_Slot, Buffer'Address, 1, False, Grant, Created);
   Check (Created, "malformed-input grant created");
   Msg := Open_Publishing (CuBit.Grant_References.Encode (Grant));
   Tag := capCall (P.Publisher_Slot, Msg, CuBit.Messages.Wait_Forever);
   Check (Tag.label = CuBit.Kernel_ABI.Reply_Error
          and then Msg.words (0) = CP.Open_Refusal'Enum_Rep (CP.Bad_Grant),
          "a one-page region is not the channel's ring");
   Msg := Open_Publishing
     (CuBit.Grant_References.Encode ((Grant.slot, Grant.generation + 1)));
   Tag := capCall (P.Publisher_Slot, Msg, CuBit.Messages.Wait_Forever);
   Check (Tag.label = CuBit.Kernel_ABI.Reply_Error
          and then Msg.words (0) = CP.Open_Refusal'Enum_Rep (CP.Bad_Grant),
          "wrong grant generation rejected");
   G.Revoke (Grant, Created);
   Check (Created, "malformed-input grant revoked");

   Msg := Request (P.Subscribe);
   Msg.tag.length := 3;
   Tag := capCall (P.Observer_Slot, Msg, CuBit.Messages.Wait_Forever);
   Check (Tag.label = P.Status'Enum_Rep (P.Invalid_Request),
          "malformed IPC header rejected");
   Msg := Request (P.Subscribe);
   Msg.tag.label := 16#0800#;
   Tag := capCall (P.Observer_Slot, Msg, CuBit.Messages.Wait_Forever);
   Check (Tag.label = P.Status'Enum_Rep (P.Denied), "retired query rejected");

   --  Every record arrives, in order, from this publisher, with no gap.
   for I in 1 .. Delivered loop
      Publish_Record;
   end loop;
   CuBit.Logging.Flush (Writer, Drained);
   Check (Drained, "logstore took the records");
   for I in 1 .. Delivered loop
      CuBit.Logging.Read_Next (Reader, Value, Lost, Result);
      Check (Result = P.OK and then
             Value.Source = syscall (SYSCALL_GETPID) and then
             P.Is_Publisher (Value.Publication_Tag) and then
             L.Text (Value.Data) = "native log check",
             "source identity and typed payload");
   end loop;
   CuBit.Logging.Read_Next (Reader, Value, Lost, Result);
   Check (Result = P.Empty, "bounded queue drained");
   CuBit.Logging.Close (Reader, Result);
   Check (Result = P.OK, "close reader");
   --  A handle logstore never issued (or no longer holds) names nothing.
   Msg := Request (P.Close);
   Msg.words (0) := Never_Issued_Handle;
   Tag := capCall (P.Observer_Slot, Msg, CuBit.Messages.Wait_Forever);
   Check (Tag.label = P.Status'Enum_Rep (P.Denied), "stale handle rejected");

   --  Launch the same executable through ordinary OP_SPAWN, not startup.
   declare
      Filename : constant String := "log-check.app";
   begin
      Buffer := [others => 0];
      for I in Filename'Range loop
         Buffer (I) := Character'Pos (Filename (I));
      end loop;
      G.Create_Via_Capability
        (12, Buffer'Address, 1, False, Grant, Created);
      Check (Created, "spawn name grant created");
      Msg := NULL_MESSAGE;
      Msg.tag := (16#0100#, Filename'Length, 0, 0);
      Msg.words := [CuBit.Grant_References.Encode (Grant), 5, 0, 0];
      Tag := capCall (12, Msg, CuBit.Messages.Wait_Forever);
      Check (Tag.label = 16#F000#, "ordinary child launched");
   end;
   debugPrint ("TEST: PASS log-authority" & ASCII.LF);
end Main;
