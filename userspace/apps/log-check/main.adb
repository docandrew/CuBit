with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Logging;
with CuBit.Memory_Grants;

procedure Main is
   package P renames CuBit.Log_Protocol;
   package L renames CuBit.Log_Records;
   package G renames CuBit.Memory_Grants;
   use type P.Status;
   Reader : CuBit.Logging.Reader;
   Writer : CuBit.Logging.Publisher;
   Value : P.Event;
   Result : P.Status;
   Lost, Ignore : Unsigned_64;
   Msg : Message;
   Tag : MessageTag;
   Completion : CompletionEntry;
   Submitted, Handled, Created : Boolean;
   Token : Unsigned_64 := 1;
   type Page is array (Positive range 1 .. 4096) of Unsigned_8
     with Alignment => 4096;
   Buffer : Page := [others => 0];
   Grant : G.Grant_Reference;
   Last_Handle : Unsigned_64;
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

   procedure Publish_Record is
      Record_Value : constant L.Decoded := L.Make ("native log check");
      Deadline : constant Unsigned_64 := syscall (SYSCALL_GETTIME) + 2000;
      Activity : Activity_Result;
      Before : constant Unsigned_64 := CuBit.Logging.Dropped (Writer);
   begin
      Check (Record_Value.Success, "make record");
      CuBit.Logging.Emit (Writer, Record_Value.Value, Token, Submitted);
      Check (Submitted, "publication submitted");
      --  Busy publication must drop locally, without recycling the live page.
      CuBit.Logging.Emit (Writer, Record_Value.Value, Token + 1, Submitted);
      Check (not Submitted, "busy publication is nonblocking");
      loop
         if Poll_Completion (Completion'Address) = 1 then
            CuBit.Logging.Complete (Writer, Completion, Handled);
            Check (Handled, "completion correlation");
         end if;
         exit when not CuBit.Logging.Pending (Writer);
         Check (syscall (SYSCALL_GETTIME) < Deadline, "publication timeout");
         Activity := Wait_For_Activity_Until (Deadline);
         Check (Activity /= Unavailable, "completion wait available");
      end loop;
      Check (CuBit.Logging.Dropped (Writer) = Before + 1,
             "exactly the busy record was dropped");
      Token := Token + 1;
   end Publish_Record;

   procedure Check_Quota is
      Record_Value : constant L.Decoded := L.Make ("quota check");
      Before : Unsigned_64;
      Throttled : Boolean := False;
      Deadline : Unsigned_64;
      Activity : Activity_Result;
   begin
      --  The client is still asynchronous: only this test waits for each CQE.
      --  A normal event loop simply forwards it and continues other work.
      for Attempt in 1 .. 256 loop
         Before := CuBit.Logging.Dropped (Writer);
         CuBit.Logging.Emit (Writer, Record_Value.Value, Token, Submitted);
         Check (Submitted, "publisher remains usable");
         Deadline := syscall (SYSCALL_GETTIME) + 2000;
         loop
            if Poll_Completion (Completion'Address) = 1 then
               CuBit.Logging.Complete (Writer, Completion, Handled);
               Check (Handled, "quota completion correlation");
               exit;
            end if;
            Check (syscall (SYSCALL_GETTIME) < Deadline, "quota timeout");
            Activity := Wait_For_Activity_Until (Deadline);
            Check (Activity /= Unavailable, "quota wait available");
         end loop;
         Token := Token + 1;
         Throttled := Completion.msg.tag.label =
           P.Status'Enum_Rep (P.Rate_Limited);
         if Throttled then
            Check (CuBit.Logging.Dropped (Writer) = Before + 1,
                   "rate-limited record counted exactly once");
            exit;
         end if;
         Check (Completion.msg.tag.label = P.Status'Enum_Rep (P.OK) and then
                CuBit.Logging.Dropped (Writer) = Before,
                "admitted quota record accepted");
      end loop;
      Check (Throttled, "native shared budget enforced");
      --  A producer cannot move to a fresh pool by forging the authority tag.
      --  Invalid grants still consume admission credits before acquisition.
      Throttled := False;
      for Attempt in 1 .. 16 loop
         Msg := Request (P.Publish);
         Msg.authorityTag := P.Publisher_Tag
           (P.Budget_Id (2 + Attempt mod 14), Unsigned_64 (Attempt));
         Msg.words := [G.MAXIMUM_GLOBAL_SLOT, G.MAXIMUM_GENERATION,
                       L.Header_Bytes, 0];
         Tag := capCall (P.Publisher_Slot, Msg);
         Throttled := Tag.label = P.Status'Enum_Rep (P.Rate_Limited);
         exit when Throttled;
      end loop;
      Check (Throttled and then Msg.words = [0, 0, 0, 0],
             "forged budget cannot bypass rate limit");
      Ignore := syscall (SYSCALL_SLEEP, 250);
      Publish_Record;
      debugPrint ("TEST: PASS log-quota" & ASCII.LF);
   end Check_Quota;

   procedure Check_Disconnect is
      Logger : CuBit.Logging.Publisher;
      Fresh : CuBit.Logging.Publisher;
      Record_Value : constant L.Decoded := L.Make ("disconnect check");
      Done : Boolean;
      Deadline : constant Unsigned_64 := syscall (SYSCALL_GETTIME) + 2000;
   begin
      CuBit.Logging.Disconnect (Fresh, Done);
      Check (Done, "unused publisher disconnects immediately");
      CuBit.Logging.Emit (Fresh, Record_Value.Value, 9000, Submitted);
      Check (not Submitted, "disconnected publisher cannot reconnect");
      CuBit.Logging.Emit (Logger, Record_Value.Value, 9001, Submitted);
      Check (Submitted, "disconnect fixture submitted");
      CuBit.Logging.Disconnect (Logger, Done);
      Check (not Done, "outstanding completion prevents premature release");
      CuBit.Logging.Emit (Logger, Record_Value.Value, 9002, Submitted);
      Check (not Submitted, "disconnect stops new publication");
      loop
         if Poll_Completion (Completion'Address) = 1 then
            CuBit.Logging.Complete (Logger, Completion, Handled);
            Check (Handled, "disconnect drains original completion");
         end if;
         CuBit.Logging.Disconnect (Logger, Done);
         exit when Done;
         Check (syscall (SYSCALL_GETTIME) < Deadline, "disconnect timeout");
         Ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;
      CuBit.Logging.Disconnect (Logger, Done);
      Check (Done, "disconnect idempotent");
      --  Both local objects may now leave scope: their pages are unshared and
      --  no completion belonging to them remains in the application's queue.
      debugPrint ("TEST: PASS log-disconnect" & ASCII.LF);
   end Check_Disconnect;

   procedure Check_Collector_Death is
      Logger : CuBit.Logging.Publisher (Slot => 18);
      Stale : CuBit.Logging.Publisher (Slot => 18);
      Record_Value : constant L.Decoded := L.Make ("collector death check");
      Done : Boolean;
      Deadline : constant Unsigned_64 := syscall (SYSCALL_GETTIME) + 2000;
   begin
      CuBit.Logging.Emit (Logger, Record_Value.Value, 9010, Submitted);
      Check (Submitted, "death fixture submitted");
      loop
         if Poll_Completion (Completion'Address) = 1 then
            Check (Completion.token = 9010 and then
                   Completion.status = COMPLETION_TARGET_DIED,
                   "collector death completes pending publication");
            CuBit.Logging.Complete (Logger, Completion, Handled);
            Check (Handled and then CuBit.Logging.Dropped (Logger) = 1,
                   "dead collector loss counted once");
            exit;
         end if;
         Check (syscall (SYSCALL_GETTIME) < Deadline, "death timeout");
         Ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;
      --  TARGET_DIED need not imply mapping retirement has completed yet.
      loop
         CuBit.Logging.Disconnect (Logger, Done);
         exit when Done;
         Check (syscall (SYSCALL_GETTIME) < Deadline, "retirement timeout");
         Ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;
      CuBit.Logging.Emit (Stale, Record_Value.Value, 9011, Submitted);
      Check (not Submitted, "dead endpoint cannot create a new grant");
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
      Tag := capCall (P.Publisher_Slot, Msg);
      Check (Tag.label = P.Status'Enum_Rep (P.Denied) and then
             Msg.words = [0, 0, 0, 0], "forged observer tag denied");
      Publish_Record;
      Check_Quota;
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
           getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_CLOCK) and then
           P.Is_Publisher (Value.Publication_Tag);
      end if;
   end loop;
   Check (Saw_Clock, "native clock emitted authenticated diagnostics");

   Msg := Request (P.Subscribe);
   Msg.authorityTag := P.Observer_Authority_Tag;
   Tag := capCall (P.Publisher_Slot, Msg);
   Check (Tag.label = P.Status'Enum_Rep (P.Denied) and then
          Msg.words = [0, 0, 0, 0], "publisher cannot observe with forged tag");
   Msg := Request (P.Publish);
   Tag := capCall (P.Observer_Slot, Msg);
   Check (Tag.label = P.Status'Enum_Rep (P.Denied),
          "observer cannot publish");
   Msg := Request (P.Subscribe);
   Tag := capCall (P.Observer_Slot, Msg);
   Check (Tag.label = P.Status'Enum_Rep (P.OK), "idempotent subscribe");
   Last_Handle := Msg.words (0);

   G.Create_Via_Capability
     (P.Publisher_Slot, Buffer'Address, 1, False, Grant, Created);
   Check (Created, "malformed-input grant created");
   Msg := Request (P.Publish);
   Msg.words := [Grant.slot, Grant.generation, L.Header_Bytes, 0];
   Tag := capCall (P.Publisher_Slot, Msg);
   Check (Tag.label = P.Status'Enum_Rep (P.Invalid_Request),
          "malformed wire record rejected");
   Msg := Request (P.Publish);
   Msg.words := [Grant.slot, Grant.generation + 1, L.Header_Bytes, 0];
   Tag := capCall (P.Publisher_Slot, Msg);
   Check (Tag.label = P.Status'Enum_Rep (P.Invalid_Request),
          "wrong grant generation rejected");
   Msg := Request (P.Read_Next);
   Msg.words := [Last_Handle, Grant.slot, Grant.generation,
                 Unsigned_64 (L.Wire_Count'Last)];
   Tag := capCall (P.Observer_Slot, Msg);
   Check (Tag.label = P.Status'Enum_Rep (P.Invalid_Request),
          "read-only output grant rejected");
   G.Revoke (Grant, Created);
   Check (Created, "malformed-input grant revoked");

   Msg := Request (P.Subscribe);
   Msg.tag.length := 3;
   Tag := capCall (P.Observer_Slot, Msg);
   Check (Tag.label = P.Status'Enum_Rep (P.Invalid_Request),
          "malformed IPC header rejected");
   Msg := Request (P.Subscribe);
   Msg.tag.label := 16#0800#;
   Tag := capCall (P.Observer_Slot, Msg);
   Check (Tag.label = P.Status'Enum_Rep (P.Denied), "retired query rejected");

   for I in 1 .. 19 loop
      Publish_Record;
   end loop;
   CuBit.Logging.Read_Next (Reader, Value, Lost, Result);
   Check (Result = P.Gap and then Lost = 3, "explicit overflow gap");
   for I in 1 .. 16 loop
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
   Msg := Request (P.Close);
   Msg.words (0) := Last_Handle;
   Tag := capCall (P.Observer_Slot, Msg);
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
      Msg.words := [Grant.slot, 5, 0, 0];
      Tag := capCall (12, Msg);
      Check (Tag.label = 16#F000#, "ordinary child launched");
   end;
   debugPrint ("TEST: PASS log-authority" & ASCII.LF);
end Main;
