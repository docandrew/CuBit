--  BUG-003 hosted model tests: nvme.drv's command identifier tracker, queue
--  cursors and recovery state machine, driven against a simulated
--  controller completion queue (phase tags, wrap-around, late and foreign
--  entries). These are Linux-hosted regression tests of the pure units the
--  driver uses unchanged; the native path is covered by the nvme-timeout
--  and nvme-stuck headless tests.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions;
with Interfaces; use Interfaces;
with NVMe_Protocol; use NVMe_Protocol;
with NVMe_Layout; use NVMe_Layout;
with NVMe_Queue_Cursors; use NVMe_Queue_Cursors;
with NVMe_Command_Ids; use NVMe_Command_Ids;
with NVMe_Recovery; use NVMe_Recovery;

procedure Main is
   Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Put_Line ("FAIL: " & What);
         raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Check;

   ---------------------------------------------------------------------------
   --  A simulated controller: it writes completion entries into a zeroed
   --  queue with its own tail and phase, as NVMe 4.6 describes.
   ---------------------------------------------------------------------------
   type Entry_Record is record
      Queue  : Queue_Identifier := 0;
      Id     : Command_Identifier := 0;
      Status : Status_Field := 0;
   end record;

   Ring : array (Queue_Index) of Entry_Record;
   Controller_Tail : Queue_Index := 0;
   Controller_Phase : Phase_Tag := First_Pass_Phase;
   Host : Cursor := Initial;
   Consumed : Natural := 0;

   procedure Fresh_Queue is
   begin
      Ring := [others => (others => <>)];
      Controller_Tail := 0;
      Controller_Phase := First_Pass_Phase;
      Host := Initial;
      Consumed := 0;
   end Fresh_Queue;

   procedure Post (Id : Command_Identifier; Code : Status_Code := Successful_Completion;
                   Queue : Queue_Identifier := IO_Queue) is
   begin
      Ring (Controller_Tail) :=
        (Queue => Queue, Id => Id,
         Status => Status_Field (Code) * 2 ** Status_Code_Shift or
                   Status_Field (Controller_Phase));
      if Controller_Tail = Queue_Index'Last then
         Controller_Tail := 0;
         Controller_Phase := Controller_Phase + 1;
      else
         Controller_Tail := Controller_Tail + 1;
      end if;
   end Post;

   --  The driver's settle step: take the entry at the cursor if posted and
   --  classify it; the cursor advances whatever the entry names.
   procedure Take
     (Table : in out Tracker; Taken : out Boolean; Result : out Match;
      Status : out Status_Field)
   is
      Item : Entry_Record;
   begin
      Result := No_Match;
      Status := 0;
      Taken := Is_Posted (Host, Ring (Host.Head).Status);
      if not Taken then
         return;
      end if;
      Item := Ring (Host.Head);
      Host := Advance (Host);
      Consumed := Consumed + 1;
      Check (Host = Cursor_After (Consumed), "cursor follows the consumed count");
      Status := Item.Status;
      if Item.Queue = IO_Queue then
         Retire (Table, Item.Id, Result);
      end if;
   end Take;

   function Distinct_Outstanding (Table : Tracker) return Boolean is
     (for all A in Flight_Slot =>
        (for all B in Flight_Slot =>
           (if A /= B and then Outstanding (Table, A) and then Outstanding (Table, B)
            then Identifier (Table, A) /= Identifier (Table, B))));

   Table : Tracker;
   Id : Command_Identifier;
   Ids : array (Flight_Slot) of Command_Identifier;
   Taken : Boolean;
   Result : Match;
   Status : Status_Field;
begin
   ---------------------------------------------------------------------------
   --  Cursor arithmetic: many passes, never a stale entry, never a skip.
   ---------------------------------------------------------------------------
   Fresh_Queue;
   Table := Empty;
   for Round in 1 .. 10 * Queue_Depth + 3 loop
      Issue (Table, 0, Id);
      Post (Id);
      Take (Table, Taken, Result, Status);
      Check (Taken and then Result.Kind = Current and then Result.Slot = 0,
             "each completion is taken once, as the current command");
      Take (Table, Taken, Result, Status);
      Check (not Taken, "an entry from the previous pass is not taken again");
   end loop;
   Check (Advance ((Head => Queue_Index'Last, Phase => 1)) = (Head => 0, Phase => 0),
          "wrap flips the phase");
   for N in 0 .. 5 * Queue_Depth loop
      Check (Cursor_After (N + Queue_Depth).Phase /= Cursor_After (N).Phase,
             "a pass flips the phase");
   end loop;
   Check (Occupied (3, 3) = 0 and then Occupied (2, 3) = Queue_Depth - 1
          and then Full (2, 3) and then not Full (3, 3), "SQ occupancy");

   ---------------------------------------------------------------------------
   --  Late completion: a command times out, the others complete, then the
   --  timed-out command's completion arrives. It is consumed (cursor in
   --  step), names its own slot as Late, and the slot is free again. Before
   --  BUG-003's fix the driver latched on the timeout; a late entry taken
   --  during a later request was "foreign" and fatal.
   ---------------------------------------------------------------------------
   Fresh_Queue;
   Table := Empty;
   for Slot in 0 .. 2 loop
      Issue (Table, Slot, Ids (Slot));
   end loop;
   Expire (Table, 1);
   Check (Outstanding (Table, 1), "an expired command still owns its slot");
   Post (Ids (0));
   Post (Ids (2));
   Take (Table, Taken, Result, Status);
   Check (Taken and then Result.Kind = Current and then Result.Slot = 0, "slot 0 current");
   Take (Table, Taken, Result, Status);
   Check (Taken and then Result.Kind = Current and then Result.Slot = 2, "slot 2 current");
   Check (not All_Free (Table) and then not Any_Busy (Table),
          "only the expired command is outstanding");
   Post (Ids (1), Command_Abort_Requested);
   Take (Table, Taken, Result, Status);
   Check (Taken and then Result.Kind = Late and then Result.Slot = 1,
          "the late completion names its expired slot");
   Check (Was_Aborted (Status), "an aborted status is recognised");
   Check (All_Free (Table), "the late completion frees the slot");
   --  The slot's next command has a new identifier; a duplicate of the
   --  old completion is foreign and changes nothing.
   Issue (Table, 1, Id);
   Check (Id /= Ids (1), "a reissued slot gets a fresh identifier");
   Post (Ids (1));
   declare
      Before : constant Tracker := Table;
   begin
      Take (Table, Taken, Result, Status);
      Check (Taken and then Result.Kind = Foreign and then Table = Before,
             "a stale duplicate is foreign and leaves the tracker alone");
   end;
   Post (Id);
   Take (Table, Taken, Result, Status);
   Check (Taken and then Result.Kind = Current and then Result.Slot = 1,
          "the new command still completes after the stale entry");
   --  An entry for another queue is not ours either.
   Issue (Table, 4, Id);
   Post (Id, Queue => Admin_Queue);
   Take (Table, Taken, Result, Status);
   Check (Taken and then Result.Kind = Foreign and then Outstanding (Table, 4),
          "another queue's entry does not retire an I/O command");

   ---------------------------------------------------------------------------
   --  Identifier reuse: an expired slot cannot take a new command until its
   --  completion arrives or the controller is reset; outstanding
   --  identifiers are always distinct.
   ---------------------------------------------------------------------------
   Table := Empty;
   Issue (Table, 3, Id);
   Expire (Table, 3);
   declare
      Refused : Boolean := False;
      Again : Command_Identifier;
   begin
      begin
         Issue (Table, 3, Again);
      exception
         when Ada.Assertions.Assertion_Error => Refused := True;
      end;
      Check (Refused, "issuing on an expired slot violates the precondition");
   end;
   Reset_All (Table);
   Check (All_Free (Table), "a reset frees every slot");
   Retire (Table, Id, Result);
   Check (Result.Kind = Foreign, "a pre-reset identifier no longer matches");
   Issue (Table, 3, Ids (3));
   Check (Ids (3) /= Id, "the slot's first command after a reset is new");
   --  Every generation of every slot, all slots busy at once.
   Table := Empty;
   for Round in 1 .. Natural (Generation'Last) + 3 loop
      for Slot in Flight_Slot loop
         if Outstanding (Table, Slot) then
            Retire (Table, Identifier (Table, Slot), Result);
            Check (Result.Kind = Current, "retire the slot's own command");
         end if;
         Issue (Table, Slot, Id);
         Check (Slot_Of (Id) = Slot, "the identifier carries its slot");
      end loop;
      Check (Distinct_Outstanding (Table), "outstanding identifiers differ");
   end loop;

   ---------------------------------------------------------------------------
   --  The recovery state machine and the per-request bound.
   ---------------------------------------------------------------------------
   Check (Next (Serving, Commands_Expired) = Aborting, "expiry aborts");
   Check (Next (Aborting, Abort_Settled) = Serving, "a settled abort serves");
   Check (Next (Aborting, Abort_Unsettled) = Resetting, "an unsettled abort resets");
   Check (Next (Resetting, Reset_Succeeded) = Serving, "a good reset restores service");
   Check (Next (Resetting, Reset_Failed) = Offline, "a failed reset is offline");
   Check (Next (Offline, Request_Arrived) = Resetting,
          "offline is not a latch: the next request retries a reset");
   --  A command that never completes: every pass expires, aborts are
   --  declined, every reset succeeds. The request ends Timed_Out after
   --  Command_Attempts passes.
   declare
      Phase : Recovery_Phase := Serving;
      Pass : Attempt := Attempt'First;
      Passes : Natural := 0;
      Decision : Verdict;
   begin
      loop
         Passes := Passes + 1;
         Phase := Next (Phase, Commands_Expired);
         Phase := Next (Phase, Abort_Unsettled);
         Phase := Next (Phase, Reset_Succeeded);
         Decision := Decide (Pass, Pass_Recovered, Failed => False);
         exit when Decision.Final;
         Pass := Decision.Next_Attempt;
      end loop;
      Check (Decision.Outcome = Timed_Out and then Passes = Command_Attempts
             and then Phase = Serving,
             "a stuck command ends Timed_Out after the bounded passes, serving");
      --  The next request on the same controller completes.
      Phase := Next (Phase, Request_Arrived);
      Decision := Decide (Attempt'First, Pass_Complete, Failed => False);
      Check (Phase = Serving and then Decision.Final
             and then Decision.Outcome = Completed,
             "the next request is served after the timeout");
   end;
   --  One slow command: its pass recovers by reset, the re-issue completes.
   declare
      Decision : Verdict := Decide (Attempt'First, Pass_Recovered, Failed => False);
   begin
      Check (not Decision.Final and then Decision.Next_Attempt = 2, "re-issue once");
      Decision := Decide (Decision.Next_Attempt, Pass_Complete, Failed => False);
      Check (Decision.Final and then Decision.Outcome = Completed, "the re-issue completes");
   end;
   Check (Decide (1, Pass_Offline, False) = (Final => True, Outcome => Device_Offline),
          "an offline controller fails the request as offline");
   Check (Decide (1, Pass_Complete, True) = (Final => True, Outcome => NVMe_Recovery.Device_Error),
          "a status error is a device error");

   ---------------------------------------------------------------------------
   --  Wire values.
   ---------------------------------------------------------------------------
   Check (Abort_Dword_10 (IO_Queue, 16#1234#) = 16#1234_0001#, "Abort CDW10");
   Check (Abort_Performed (0) and then not Abort_Performed (1), "Abort DW0 bit 0");
   Check (Ready_Timeout (0) = Ready_Timeout_Unit_Microseconds, "CAP.TO 0 is one unit");
   Check (Ready_Timeout (20 * 2 ** Ready_Timeout_Shift) = 20 * Ready_Timeout_Unit_Microseconds,
          "CAP.TO = 20 is 10 s");
   Check (Succeeded (1) and then not Succeeded (2#10#), "phase bit is not status");
   Check (Flight_Slots = 7, "seven 128 KiB slices in the data area");

   Put_Line ("nvme-recovery: PASS" & Natural'Image (Checks) & " checks");
end Main;
