------------------------------------------------------------------------------
--  WAIT (ABI-002 slice 2) and the readiness page (KERN-013).
--
--  Each source class polled with Now, empty and then with an entry this
--  program queued to itself (a request by capSubmit to its own endpoint, a
--  completion by answering it, an event sent to itself); a deadline that
--  expires (Messages and Ready forms); the timer; the forms the kernel does
--  not offer refused; and the readiness counters: each moves by exactly one
--  per entry queued and never goes down.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Console_Route;
with System.Storage_Elements;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles; use CuBit.Handles;
with CuBit.Handles.Startup;
with CuBit.Process_Calls;
with CuBit.Startup_Tables;
with CuBit.Readiness;
with CuBit.Monotonic;
with CuBit.Waits;
with Wait_ABI;
with Channel_Wake_ABI;

procedure Main is
   package W renames Wait_ABI;
   package R renames CuBit.Readiness;
   package ST renames CuBit.Startup_Tables;
   use type R.Count;
   LF : constant Character := ASCII.LF;
   Passed : Boolean := True;
   Probe_Label : constant Unsigned_32 := 16#0A17#;
   Probe_Token : constant Unsigned_64 := 16#0A17_0001#;
   Expiry_Ms   : constant := 30;
   Timer_Us    : constant := 5_000;
   Self : Endpoint;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      debugPrint ("wait-check: " & Name & (if Condition then " PASS" else " FAIL") & LF);
      Passed := Passed and Condition;
   end Check;

   function Address_Of (M : aliased Message) return Unsigned_64 is
     (Unsigned_64 (System.Storage_Elements.To_Integer (M'Address)));

   function Probe return Message is
     ((tag => (label => Probe_Label, length => 1, protocol => 0, ipc => <>, reserved => 0),
       authorityTag => 0, words => [1, 0, 0, 0]));

   M : aliased Message := NULL_MESSAGE;
   C : aliased CompletionEntry;
   Identity : aliased Unsigned_64 := 0;
   Word : aliased Unsigned_32 := 7;
   Result : Unsigned_64;
   Before, After : R.Count;
   Started : Unsigned_64;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   Self := CuBit.Handles.Startup.Endpoint_Named (ST.Self_Text);

   --  Every class empty: Now looks once and finds nothing.
   Check (CuBit.Waits.Wait (W.Requests_Sources, W.Now, Address_Of (M)) = W.Nothing,
          "Now on requests, empty");
   Check (CuBit.Waits.Wait (W.Events_Sources, W.Now, Address_Of (M)) = W.No_Event,
          "Now on events, empty");
   Check (CuBit.Waits.Wait (W.Messages_Sources, W.Now, Address_Of (M)) = W.Nothing,
          "Now on messages, empty");
   Check (CuBit.Waits.Wait (W.Completions_Sources, W.Now,
                            Unsigned_64 (System.Storage_Elements.To_Integer (C'Address)), 1, 0) = 0,
          "Now on completions, empty");
   Check (CuBit.Waits.Wait (W.Ready_Sources, W.Now) = W.Ready_Deadline,
          "Now on readiness, nothing ready");
   Check (CuBit.Waits.Futex_Wait (Word'Address, 6, W.Now) = W.Futex_Retry,
          "Now on a futex word holding another value");
   Check (CuBit.Waits.Futex_Wait (Word'Address, 7, W.Now) = W.Futex_Timed_Out,
          "Now on a futex word holding the value");
   Check (ABI.Is_Refusal_Word (CuBit.Waits.Wait (W.Offered_Sources, W.Now, To_Word (Own_Process))),
          "Now on an offered capability, no call");

   --  Forms the kernel does not offer.
   Check (CuBit.Waits.Wait (W.Timer_Sources, W.Now, 0) = W.Unsupported_Form,
          "timer with a deadline refused");
   Check (CuBit.Waits.Wait (W.Requests_Sources, W.Forever, Address_Of (M)) = W.Unsupported_Form,
          "requests alone with Forever refused");
   Check (CuBit.Waits.Wait (W.Messages_Sources + W.Timer_Sources, W.Now) = W.Unsupported_Form,
          "mixed sources refused");
   Check (CuBit.Waits.Wait (16#4000#, W.Now) = W.Unsupported_Form, "unknown source refused");
   Check (CuBit.Waits.Wait (W.Messages_Sources + W.Wake_First, W.Now) = W.Unsupported_Form,
          "wake-first outside the futex form refused");
   Check (CuBit.Waits.Wait (W.Futex_Sources + W.Wake_Shared, W.Now) = W.Unsupported_Form,
          "a shared wake without wake-first refused");

   --  Deadlines expire.
   Started := CuBit.Monotonic.Milliseconds;
   Result := CuBit.Waits.Wait (W.Messages_Sources, Started + Expiry_Ms, Address_Of (M));
   Check (Result = W.Nothing and then CuBit.Monotonic.Milliseconds >= Started + Expiry_Ms,
          "a messages deadline expires");
   Started := CuBit.Monotonic.Milliseconds;
   Check (CuBit.Waits.Wait (W.Ready_Sources, Started + Expiry_Ms) = W.Ready_Deadline
          and then CuBit.Monotonic.Milliseconds >= Started + Expiry_Ms,
          "a readiness deadline expires");
   Started := CuBit.Monotonic.Microseconds;
   CuBit.Waits.Sleep_Until (Started + Timer_Us);
   Check (CuBit.Monotonic.Microseconds >= Started + Timer_Us, "the timer sleeps until its instant");
   Started := CuBit.Monotonic.Microseconds;
   CuBit.Waits.Sleep_Until (Started / 2);
   Check (CuBit.Monotonic.Microseconds - Started < Timer_Us, "a past instant returns at once");

   --  A request queued to itself: the requests counter moves by one, the
   --  readiness wait reports work, Now takes it.
   Before := R.Produced (R.Requests);
   Check (capSubmit (Self, Probe, Probe_Token), "request submitted to itself");
   After := R.Produced (R.Requests);
   Check (After = Before + 1, "requests counter moved by one");
   Check (CuBit.Waits.Wait (W.Ready_Sources, W.Now) = W.Ready_Work, "readiness wait sees the request");
   M := NULL_MESSAGE;
   Result := CuBit.Waits.Wait (W.Requests_Sources, W.Now, Address_Of (M));
   Check (Result /= W.Nothing and then M.tag.label = Probe_Label, "Now on requests takes it");
   Check (R.Produced (R.Requests) = After, "taking does not move the counter");

   --  Answering it queues a completion to itself.
   Before := R.Produced (R.Completions);
   M.tag := (label => Probe_Label, length => 0, protocol => 0, ipc => <>, reserved => 0);
   Check (CuBit.Messages.reply (From_Word (Result), M) /= Unsigned_64'Last, "request answered");
   Check (R.Produced (R.Completions) = Before + 1, "completions counter moved by one");
   Check (CuBit.Waits.Wait (W.Completions_Sources, W.Now,
                            Unsigned_64 (System.Storage_Elements.To_Integer (C'Address)), 1, 0) = 1
          and then C.token = Probe_Token,
          "Now on completions takes it");

   --  An event sent to itself.
   Before := R.Produced (R.Events);
   Check (trySendEvent (Own_Process, Probe, Self), "event sent to itself");
   Check (R.Produced (R.Events) = Before + 1, "events counter moved by one");
   M := NULL_MESSAGE;
   Check (CuBit.Waits.Wait (W.Events_Sources, W.Now, Address_Of (M),
                            Unsigned_64 (System.Storage_Elements.To_Integer (Identity'Address))) = W.Taken
          and then M.tag.label = Probe_Label and then Identity = To_Word (Own_Process),
          "Now on events takes it, with its publisher");

   --  The tracker: pending exactly while unconsumed.
   declare
      T : R.Tracker;
      Seen : R.Count;
   begin
      Seen := R.Produced (R.Requests);
      R.Caught_Up (T, R.Requests, Seen);
      Check (not R.Pending (T, R.Requests), "caught up: requests not pending");
      Check (capSubmit (Self, Probe, Probe_Token), "second request submitted");
      Check (R.Pending (T, R.Requests), "a new request is pending");
      Result := CuBit.Waits.Wait (W.Requests_Sources, W.Now, Address_Of (M));
      R.Took (T, R.Requests);
      Check (not R.Pending (T, R.Requests), "taking every counted request needs no empty poll");
      Check (CuBit.Messages.reply (From_Word (Result), M) /= Unsigned_64'Last, "second request answered");
      Result := CuBit.Waits.Wait (W.Completions_Sources, W.Now,
                                  Unsigned_64 (System.Storage_Elements.To_Integer (C'Address)), 1, 0);
   end;

   --  Ready_Word (IPC-010 option B): the activity wait with a wake word.
   declare
      Cell : aliased Unsigned_32 := 0 with Volatile;
      At_Word : constant Unsigned_64 := Unsigned_64 (System.Storage_Elements.To_Integer (Cell'Address));
      Wake_Message : Message := NULL_MESSAGE;
      Before_Events : R.Count;
   begin
      Check (CuBit.Waits.Wait (W.Ready_Word_Sources, W.Now, 0, At_Word, 0) = W.Ready_Deadline,
             "Ready_Word with nothing pending: deadline");
      Check (CuBit.Waits.Wait (W.Ready_Word_Sources, W.Now, 0, At_Word, 1) = W.Ready_Word,
             "Ready_Word: the word no longer holds the value");
      Before_Events := R.Produced (R.Events);
      Check (CuBit.Waits.Futex_Wake (CuBit.Waits.Private_Word (Cell'Address), 1) = 1,
             "a futex wake of the wake word wakes it");
      Check (R.Produced (R.Events) = Before_Events + 1, "the wake counts one event");
      Check (CuBit.Waits.Wait (W.Ready_Word_Sources, W.Now, 0, At_Word, 0) = W.Ready_Word,
             "the next Ready_Word wait takes the wake");
      Check (CuBit.Waits.Wait (W.Ready_Word_Sources, W.Now, 0, At_Word, 0) = W.Ready_Deadline,
             "a wake is taken once");
      Check (CuBit.Waits.Wait (W.Ready_Word_Sources, CuBit.Messages.Deadline_After (30), 0, At_Word, 0)
               = W.Ready_Deadline,
             "a Ready_Word deadline expires");
      Check (CuBit.Waits.Wait (W.Ready_Word_Sources, W.Now, 0, At_Word, W.Word_Value_Last + 1)
               = W.Unsupported_Form,
             "Ready_Word with a value past 32 bits refused");
      Check (CuBit.Waits.Wait (W.Ready_Word_Sources, W.Now, 0, At_Word + 1, 0) = W.Unsupported_Form,
             "Ready_Word with an unaligned word refused");
      --  A wake taken by a receive instead: the wake notice.
      Check (CuBit.Waits.Futex_Wake (CuBit.Waits.Private_Word (Cell'Address), 1) = 1,
             "a second wake");
      M := NULL_MESSAGE;
      Check (CuBit.Waits.Wait (W.Events_Sources, W.Now, Address_Of (M)) = W.Taken
               and then M.tag.label = Channel_Wake_ABI.Wake_Label,
             "an event poll takes the wake as the wake notice");
      --  Nobody can send the notice.
      Wake_Message.tag := (label => Channel_Wake_ABI.Wake_Label, length => 1, protocol => 0,
                           ipc => <>, reserved => 0);
      Check (not trySendEvent (Own_Process, Wake_Message, Self), "a sent wake notice is refused");
   end;

   debugPrint ((if Passed then "wait-check: ALL PASS" else "wait-check: SOME FAIL") & LF);
   CuBit.Process_Calls.Exit_Process ((if Passed then CuBit.Process_Calls.Success else CuBit.Process_Calls.Failure));
end Main;
