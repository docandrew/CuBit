------------------------------------------------------------------------------
--  The launch gate check (CAP-012, docs/authority-model.md 7.5: Launch-into-S;
--  headless --test launch-gate).
--
--  A requester holding procmgr's endpoint and a Launch badge into the user
--  session S checks, with raw requests (so it builds before and after):
--
--  1. procmgr's retired OP_SPAWN (16#0100#) starts nothing;
--  2. OP_LAUNCH through procmgr's plain endpoint (no badge) is refused,
--     even for capability-test.app, which is in its table and asks for
--     nothing;
--  3. through the badge, launch-gate-child starts and gets what S's envelope
--     allows of its requests (the clock), not what it does not (clock
--     control, the framebuffer: optional, withheld). The child says so;
--  4. launch-gate-greedy, which requires clock control, is refused with
--     Request_Withheld naming "clock-control";
--  5. the launch hands back a launch handle (CuBit.Launch_Handles), the
--     requester's only way to the child, whose kernel parent is procmgr:
--     the requester's own kernel control message to it is refused; Observe
--     says it runs; the handle reaches no other procmgr operation, and a
--     handle operation without a handle is refused; a handle narrowed to
--     Observe may not debug or stop; Debug mints Debug_Observe on the
--     child (its threads), Pause is withheld; Stop ends it, procmgr's exit
--     notice arrives, Observe then gives the outcome and Debug answers
--     Child_Ended; after Release the handle answers nothing;
--  6. the session's debug authority (debug-session, optional) observes a
--     system service (clock.svc), never procmgr itself;
--  7. launching put nothing in this requester's handle table: once the
--     handles are let go it has as many free handle places as before.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

with CuBit.Child_Exits;
with CuBit.Control_Events;
with CuBit.Grant_References;
with CuBit.Kernel_Calls;
with CuBit.Handles.Debug;
with CuBit.Launch_Handles;
with CuBit.Monotonic;
with CuBit.Process_Events;
with CuBit.Process_IDs;
with CuBit.Waits;
with Debug_ABI;
with CuBit.Handles; use CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Services;
with CuBit.Handles.Startup;
with CuBit.Kernel_ABI;
with CuBit.Launch_Arguments;
with CuBit.Launch_Grants;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Startup_Tables;
with Startup;

procedure Main is
   package IPC renames CuBit.Handles.IPC;
   package LA renames CuBit.Launch_Arguments;
   package LH renames CuBit.Launch_Handles;
   package HD renames CuBit.Handles.Debug;
   use type ABI.Refusal;
   use type LH.Outcome, LH.Rights, IPC.Carried_Kind;

   Process_Manager : constant Endpoint :=
     CuBit.Handles.Services.Process_Manager.Handle (Startup.Process_Manager);
   Launch_Badge : constant Endpoint :=
     (if Startup.Launch.Present
      then CuBit.Handles.Services.Process_Manager.Handle (Startup.Launch.Value)
      else No_Endpoint);
   Self : constant Endpoint :=
     CuBit.Handles.Startup.Endpoint_Named (CuBit.Startup_Tables.Self_Text);

   --  procmgr's retired OP_SPAWN label, and the code and label words of the
   --  Request_Withheld refusal (CuBit.Launch_Arguments, CAP-012), named
   --  here so this check builds against the runtime before that refusal.
   Retired_Spawn_Label    : constant Unsigned_32 := 16#0100#;
   Request_Withheld_Code  : constant Unsigned_64 := 18;
   Withheld_Label_Bytes   : constant := 24;
   Answer_Ms : constant := 10_000;
   Priority  : constant := 3;

   Passed : Boolean := True;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      debugPrint ("launch-gate: " & Name & (if Condition then " PASS" else " FAIL") & ASCII.LF);
      Passed := Passed and then Condition;
   end Check;

   type Page is array (Positive range 1 .. 4096) of Unsigned_8 with Alignment => 4096;
   Name_Page : Page := [others => 0];

   --  A raw launch request (Label: OP_LAUNCH or the retired OP_SPAWN) for
   --  Name, through To; the reply's tag and words.
   procedure Request
     (To : Endpoint; Label : Unsigned_32; Name : String; Response : out Message; Answer : out MessageTag;
      Received : out IPC.Carried; Accepting : Boolean := False)
   is
      Grant : CuBit.Memory_Grants.Grant_Reference;
      Created, Revoked : Boolean;
   begin
      Name_Page := [others => 0];
      for I in Name'Range loop
         Name_Page (I - Name'First + 1) := Character'Pos (Name (I));
      end loop;
      Response := NULL_MESSAGE;
      Answer := (label => 0, others => <>);
      Received := (Kind => IPC.Absent);
      CuBit.Memory_Grants.Create_Via_Capability (To, Name_Page'Address, 1, False, Grant, Created);
      if not Created then
         return;
      end if;
      if Label = Retired_Spawn_Label then
         Response.tag := (label => Label, length => Name'Length, protocol => 0, ipc => <>, reserved => 0);
         Response.words := [CuBit.Grant_References.Encode (Grant), Priority, 0, 0];
      else
         Response.tag := (label => Label, length => LA.Launch_Request_Words,
                       protocol => 0, ipc => <>, reserved => 0);
         Response.words := [CuBit.Grant_References.Encode (Grant), Name'Length, 0,
                         CuBit.Launch_Grants.Request_Word (Priority, 0)];
      end if;
      if Accepting then
         Answer := IPC.Call_Accepting (To, Response, Deadline_After (Answer_Ms), Received);
      else
         Answer := capCall (To, Response, Deadline_After (Answer_Ms));
      end if;
      CuBit.Memory_Grants.Revoke (Grant, Revoked);
   end Request;

   procedure Request
     (To : Endpoint; Label : Unsigned_32; Name : String; Response : out Message; Answer : out MessageTag)
   is
      Ignore : IPC.Carried;
   begin
      Request (To, Label, Name, Response, Answer, Ignore);
   end Request;

   ---------------------------------------------------------------------------
   --  Through a launch handle (or not)
   ---------------------------------------------------------------------------
   --  One call: Label with W0, W1; the outcome word's meaning, the reply's
   --  words, and what the reply carried.
   procedure Handle_Call
     (Through : Endpoint; Label : Unsigned_32; Length : Unsigned_8; W0, W1 : Unsigned_64;
      Outcome : out LH.Outcome; Reply : out Message; Received : out IPC.Carried)
   is
      Answer : MessageTag;
   begin
      Reply := NULL_MESSAGE;
      Reply.tag := (label => Label, length => Length, protocol => 0, ipc => <>, reserved => 0);
      Reply.words := [W0, W1, 0, 0];
      Answer := IPC.Call_Accepting (Through, Reply, Deadline_After (Answer_Ms), Received);
      Outcome := (if Answer.label = CuBit.Kernel_ABI.Reply_OK and then LH.Is_Outcome_Word (Reply.words (0))
                  then LH.To_Outcome (Reply.words (0)) else LH.Failed);
   end Handle_Call;

   function Op (Through : Endpoint; Which : LH.Operation; W0 : Unsigned_64 := 0) return LH.Outcome is
      Outcome : LH.Outcome;
      Reply : Message;
      Received : IPC.Carried;
      Ignore : IPC.Close_Result;
   begin
      Handle_Call (Through, LH.Label_Of (Which), LH.Request_Words (Which), W0, 0, Outcome, Reply,
                   Received);
      Ignore := IPC.Close (Received);
      return Outcome;
   end Op;

   --  A Debug_Observe session's threads: how many it reported (0 on any refusal).
   Thread_Buffer : array (1 .. 16 * Debug_ABI.Thread_Entry_Bytes) of Unsigned_8 := [others => 0];
   function Threads_Seen (S : HD.Session) return Unsigned_64 is
      R : constant HD.Reply := HD.Threads (S, Thread_Buffer'Address, Thread_Buffer'Length);
   begin
      return (if HD.Outcome_Is (R, Debug_ABI.Done) then R.W1 else 0);
   end Threads_Seen;

   --  The kernel's own control message (an Interrupt, which the child
   --  ignores) from this process to Child_Word: 0 when the kernel took it.
   function Kernel_Send_Control (Child_Word : Unsigned_64) return Unsigned_64 is
     (CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Send_Control, Child_Word,
         CuBit.Control_Events.Control_Kind'Enum_Rep (CuBit.Control_Events.Interrupt)));

   --  The identity a Describe of Item names (an endpoint: its server).
   function Server_Of (Item : Endpoint) return Unsigned_64 is
      D : constant IPC.Description := IPC.Describe (Item);
   begin
      return (if D.Current then D.Object else 0);
   end Server_Of;

   --  A debug session minted for this process through the session's debug
   --  authority on Target: the outcome; the session (closed by the caller).
   procedure Session_Debug
     (Target : Unsigned_64; Kind : Debug_ABI.Debug_Kind; Outcome : out LH.Outcome;
      Session : out HD.Session)
   is
      Reply : Message;
      Received : IPC.Carried;
      Ignore : IPC.Close_Result;
      Badge : constant Endpoint :=
        (if Startup.Debug_Session.Present
         then CuBit.Handles.Services.Process_Manager.Handle (Startup.Debug_Session.Value)
         else No_Endpoint);
   begin
      Session := HD.From_Word (0);
      Handle_Call (Badge, LH.Session_Debug_Operation, LH.Session_Debug_Words, Target,
                   Debug_ABI.Kind_Word (Kind), Outcome, Reply, Received);
      if Outcome = LH.Done then
         declare
            Taken : constant HD.Accepted_Session := HD.Accept_Session (Received, Kind);
         begin
            if Taken.OK then
               Session := Taken.Value;
            else
               Outcome := LH.Failed;
            end if;
         end;
      else
         Ignore := IPC.Close (Received);
      end if;
   end Session_Debug;

   function Started (Answer : MessageTag) return Boolean is
     (Answer.label = CuBit.Kernel_ABI.Reply_OK);

   --  The label in a Request_Withheld reply's words (1) .. (3).
   function Withheld_Label (Response : Message) return String is
      Text : String (1 .. Withheld_Label_Bytes) := [others => ' '];
      Length : Natural := 0;
      Byte : Unsigned_64;
   begin
      for I in 0 .. Withheld_Label_Bytes - 1 loop
         Byte := Shift_Right (Response.words (1 + I / 8), 8 * (I mod 8)) and 16#FF#;
         exit when Byte = 0;
         Text (I + 1) := Character'Val (Byte);
         Length := I + 1;
      end loop;
      return Text (1 .. Length);
   end Withheld_Label;

   ---------------------------------------------------------------------------
   --  Free handle places: saved replies to its own requests, until the
   --  handle limit refuses one; then all answered (which frees them).
   ---------------------------------------------------------------------------
   Probe_Label : constant Unsigned_32 := 16#7E5A#;
   Token : Unsigned_64 := 16#1A0_0000#;

   function Free_Places return Natural is
      Saved : array (1 .. 16) of CuBit.Handles.Reply := [others => No_Reply];
      Count : Natural := 0;
      From : Process_ID;
      Got : Message;
      Found : Boolean;
      Ignore : Boolean;
      Completion : aliased CompletionEntry;
   begin
      for Attempt in Saved'Range loop
         declare
            Probe : Message := NULL_MESSAGE;
         begin
            Probe.tag.label := Probe_Label;
            Token := Token + 1;
            exit when not capSubmit (Self, Probe, Token);
            Poll_Service_Request (From, Got, Found);
            exit when not Found or else Got.tag.label /= Probe_Label;
            declare
               Result : constant IPC.Saved_Reply := IPC.Save_Reply;
            begin
               if not Result.Saved then
                  --  Answer it now, so its completion is not lost.
                  Ignore := CuBit.Messages.reply (From, NULL_MESSAGE) /= Unsigned_64'Last;
                  exit;
               end if;
               Count := Count + 1;
               Saved (Attempt) := Result.Handle;
            end;
         end;
      end loop;
      for S of Saved (1 .. Count) loop
         Ignore := IPC.Answer (S, NULL_MESSAGE);
      end loop;
      while Poll_Completion (Completion'Address) /= 0 loop
         null;
      end loop;
      return Count;
   end Free_Places;

   Response : Message;
   Answer : MessageTag;
   Before, After : Natural;

   --  Section 5: the child's launch handle.
   procedure Check_Handle (Launched : Message; Received : IPC.Carried) is
      Child_Word : constant Unsigned_64 := Launched.words (0);
      Taken : constant IPC.Accepted_Endpoint := IPC.Accept_Endpoint (Received);
      Handle : constant Endpoint := (if Taken.OK then Taken.Value else No_Endpoint);
      Reply : Message;
      Carried : IPC.Carried;
      Outcome : LH.Outcome;
      Ignore : IPC.Close_Result;
      Ignore_Closed : Boolean;
      Session : HD.Session := HD.From_Word (0);
      Narrow_Handle : Endpoint := No_Endpoint;
      Ended : CuBit.Child_Exits.Report;
      Has_Ended : Boolean := False;
      Notice_Wait_Ms : constant := 10_000;
      Poll_Ms : constant := 5;
      Until_Time : Unsigned_64;
   begin
      Check (Taken.OK and then Launched.words (1) = LH.Word_Of (LH.All_Rights)
             and then Launched.words (2) = LH.Outcome_Word (LH.Done),
             "the launch hands back a launch handle with Observe, Stop, Attach, Introduce, Debug");
      if not Taken.OK then
         return;
      end if;
      --  The requester is not the child's kernel parent any more: its own
      --  control message is refused (before: the kernel took it).
      Check (Kernel_Send_Control (Child_Word) /= 0,
             "the requester's own control message to the child is refused (procmgr is its parent)");
      Handle_Call (Handle, LH.Label_Of (LH.Observe_Op), 0, 0, 0, Outcome, Reply, Carried);
      Check (Outcome = LH.Done and then Reply.words (1) = Child_Word
             and then Reply.words (2) = LH.State_Word (LH.Running),
             "Observe through the handle: the child runs");
      --  The handle is never a way to procmgr's other operations.
      declare
         Through_Handle : Message := NULL_MESSAGE;
         Answer : MessageTag;
      begin
         Through_Handle.tag := (label => LA.Launch_Operation, length => LA.Launch_Request_Words,
                                protocol => 0, ipc => <>, reserved => 0);
         Answer := capCall (Handle, Through_Handle, Deadline_After (Answer_Ms));
         Check (Answer.label /= CuBit.Kernel_ABI.Reply_OK,
                "a launch handle reaches no other procmgr operation (OP_LAUNCH refused)");
      end;
      Check (Op (Process_Manager, LH.Observe_Op) = LH.Not_A_Handle,
             "a handle operation without a launch handle is refused");
      --  Narrowed to Observe: no Debug, no Stop.
      Handle_Call (Handle, LH.Label_Of (LH.Narrow_Op), 1,
                   LH.Word_Of ([LH.Observe => True, others => False]), 0, Outcome, Reply, Carried);
      declare
         Narrowed : constant IPC.Accepted_Endpoint := IPC.Accept_Endpoint (Carried);
      begin
         Check (Outcome = LH.Done and then Narrowed.OK
                and then Reply.words (1) = LH.Word_Of ([LH.Observe => True, others => False]),
                "Narrow gives a handle with Observe alone");
         if Narrowed.OK then
            Narrow_Handle := Narrowed.Value;
            Check (Op (Narrow_Handle, LH.Observe_Op) = LH.Done, "the narrowed handle observes");
            Check (Op (Narrow_Handle, LH.Debug_Op, Debug_ABI.Kind_Word (Debug_ABI.Observe))
                     = LH.Right_Missing,
                   "the narrowed handle may not debug (Right_Missing)");
            Check (Op (Narrow_Handle, LH.Control_Op, LH.Control_Word (LH.Ask_Stop)) = LH.Right_Missing,
                   "the narrowed handle may not stop the child (Right_Missing)");
         end if;
      end;
      Check (Op (Handle, LH.Attach_Op, 0) = LH.Not_Yet, "Attach has its shape: Not_Yet");
      Check (Op (Handle, LH.Debug_Op, Debug_ABI.Kind_Word (Debug_ABI.Pause)) = LH.Kind_Withheld,
             "Debug_Pause is withheld by the session's debug envelope");
      --  Debug: Debug_Observe on the child, from its Control in procmgr.
      Handle_Call (Handle, LH.Label_Of (LH.Debug_Op), 1, Debug_ABI.Kind_Word (Debug_ABI.Observe), 0,
                   Outcome, Reply, Carried);
      declare
         Minted : constant HD.Accepted_Session := HD.Accept_Session (Carried, Debug_ABI.Observe);
      begin
         Check (Outcome = LH.Done and then Minted.OK and then Reply.words (1) = Child_Word,
                "Debug through the handle hands over Debug_Observe on the child");
         if Minted.OK then
            Session := Minted.Value;
            declare
               D : constant HD.Reply := HD.Call (Session, Debug_ABI.Describe);
            begin
               Check (HD.Outcome_Is (D, Debug_ABI.Done) and then D.W2 = Child_Word,
                      "the debug session names the child");
            end;
            Check (Threads_Seen (Session) >= 1, "the debug session lists the child's threads");
         end if;
      end;
      --  Stop: the child ends, procmgr's exit notice arrives.
      Check (Op (Handle, LH.Control_Op, LH.Control_Word (LH.Ask_Stop)) = LH.Done,
             "Stop through the handle is sent");
      Until_Time := CuBit.Monotonic.Milliseconds + Notice_Wait_Ms;
      loop
         CuBit.Process_Events.Take_Exit (CuBit.Process_IDs.From_Word (Child_Word), Has_Ended, Ended);
         exit when Has_Ended or else CuBit.Monotonic.Milliseconds >= Until_Time;
         CuBit.Waits.Sleep_For (Poll_Ms);
      end loop;
      Check (Has_Ended, "procmgr's exit notice for the child arrives");
      Handle_Call (Handle, LH.Label_Of (LH.Observe_Op), 0, 0, 0, Outcome, Reply, Carried);
      Check (Outcome = LH.Done and then Reply.words (2) = LH.State_Word (LH.Exited)
             and then Reply.words (3) = 0,
             "after it ends, Observe gives its outcome (exited, code 0)");
      Check (Op (Handle, LH.Debug_Op, Debug_ABI.Kind_Word (Debug_ABI.Observe)) = LH.Child_Ended,
             "the handle dies with the child: Debug answers Child_Ended");
      if HD.Valid (Session) then
         Check (not HD.Outcome_Is (HD.Call (Session, Debug_ABI.Describe), Debug_ABI.Done),
                "the debug session ended with its target");
         Ignore_Closed := HD.Close (Session);
      end if;
      --  Release: revoked, with the handle narrowed from it.
      Check (Op (Handle, LH.Revoke_Op) = LH.Done, "Release revokes the handle");
      Check (Op (Handle, LH.Observe_Op) /= LH.Done, "a revoked handle answers nothing");
      if Valid (Narrow_Handle) then
         Check (Op (Narrow_Handle, LH.Observe_Op) /= LH.Done,
                "revoking reaches the handle narrowed from it");
         Ignore := IPC.Close (Narrow_Handle);
      end if;
      Ignore := IPC.Close (Handle);
   end Check_Handle;

   --  Section 6: the session's debug authority.
   procedure Check_Session_Debug is
      Outcome : LH.Outcome;
      Session : HD.Session;
      Ignore_Closed : Boolean;
      Clock : constant Unsigned_64 := Server_Of (Startup.Clock);
   begin
      if not Startup.Debug_Session.Present then
         Check (False, "no debug-session badge");
         return;
      end if;
      Session_Debug (Clock, Debug_ABI.Observe, Outcome, Session);
      Check (Clock /= 0 and then Outcome = LH.Done and then HD.Valid (Session),
             "the session's debug authority observes clock.svc, a system service");
      if HD.Valid (Session) then
         Check (Threads_Seen (Session) >= 1, "clock.svc's threads, through the kernel");
         Ignore_Closed := HD.Close (Session);
      end if;
      Session_Debug (Clock, Debug_ABI.Modify, Outcome, Session);
      Check (Outcome = LH.Kind_Withheld, "Debug_Modify on a system service is withheld");
      Session_Debug (Server_Of (Process_Manager), Debug_ABI.Observe, Outcome, Session);
      Check (Outcome = LH.Never_Debuggable, "procmgr itself is never debuggable");
   end Check_Session_Debug;
begin
   debugPrint ("launch-gate: starting" & ASCII.LF);
   Before := Free_Places;

   Request (Process_Manager, Retired_Spawn_Label, "capability-test.app", Response, Answer);
   Check (not Started (Answer), "procmgr's endpoint alone starts nothing (OP_SPAWN is retired)");

   Request (Process_Manager, LA.Launch_Operation, "capability-test.app", Response, Answer);
   Check (not Started (Answer), "a launch without a Launch badge is refused");

   declare
      Received : IPC.Carried;
   begin
      Request (Launch_Badge, LA.Launch_Operation, "launch-gate-child.app", Response, Answer,
               Received, Accepting => True);
      Check (Started (Answer), "with the badge, a child starts into the user session");
      Check_Handle (Response, Received);
   end;

   Request (Launch_Badge, LA.Launch_Operation, "launch-gate-greedy.app", Response, Answer);
   Check (not Started (Answer) and then Response.words (0) = Request_Withheld_Code
          and then Withheld_Label (Response) = "clock-control",
          "a required request the session withholds is refused by name (clock-control)");

   Check_Session_Debug;

   After := Free_Places;
   Check (Before > 0 and then After = Before,
          "launching left the requester's handle table as it was (" &
          Natural'Image (Before) & " free places before," & Natural'Image (After) & " after)");
   debugPrint ("launch-gate: checks " & (if Passed then "done" else "failed") & ASCII.LF);
end Main;
