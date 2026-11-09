------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What nvme.drv does when a command outlives its deadline (BUG-003), in
--  Linux's order: abort the expired commands; if they do not all complete,
--  reset the controller and re-create its queues; re-issue what was lost a
--  bounded number of times, then fail only that request with a typed
--  outcome. A reset that succeeds restores service; one that fails leaves
--  the controller Offline, and the next request tries one reset again
--  instead of latching.
--
--  Deadlines are explicit and named (no hidden defaults). The NVMe spec
--  bounds no I/O command's duration, so the values are Linux's
--  (drivers/nvme/host/core.c: NVME_IO_TIMEOUT 30 s, NVME_ADMIN_TIMEOUT
--  60 s). The old 1.02 s bound had no source: a throttled host or a busy
--  device writing back its cache exceeds it routinely.
------------------------------------------------------------------------------
package NVMe_Recovery with SPARK_Mode, Pure is

   type Microseconds is range 0 .. 2 ** 62;
   subtype Deadline is Microseconds range 1 .. Microseconds'Last;

   --  An I/O command not completed this long after submission expires.
   IO_Command_Deadline : constant Deadline := 30_000_000;

   --  An admin command (Identify, Create I/O Queue, Abort) likewise.
   Admin_Command_Deadline : constant Deadline := 60_000_000;

   --  After the Abort commands complete: how long the expired commands get
   --  to post their own completions before the controller is reset. A
   --  controller that performs an abort posts the aborted command's entry
   --  promptly (5.1); one that declines (QEMU always does) leaves the
   --  command to the reset.
   Abort_Settle_Deadline : constant Deadline := 1_000_000;

   --  Passes a request gets: the first, then one re-issue after recovery.
   --  A command still not done is failed after about two I/O deadlines,
   --  so a client waits at most about a minute for a typed error.
   Command_Attempts : constant := 2;
   subtype Attempt is Positive range 1 .. Command_Attempts;

   ---------------------------------------------------------------------------
   --  The controller's recovery state machine.
   ---------------------------------------------------------------------------
   type Recovery_Phase is
     (Serving,    --  queues live, commands accepted
      Aborting,   --  commands expired; Abort sent, waiting for them
      Resetting,  --  disabling, re-enabling, re-creating the queues
      Offline);   --  the last reset failed: commands are refused

   type Recovery_Event is
     (Commands_Expired,  --  a command outlived its deadline
      Abort_Settled,     --  every outstanding command completed
      Abort_Unsettled,   --  some did not (or an Abort itself failed)
      Reset_Succeeded,   --  queues re-created: the controller holds nothing
      Reset_Failed,      --  the controller did not disable or re-enable
      Request_Arrived);  --  a client asked for I/O

   --  Whether Event can happen in Phase.
   function Expected (Phase : Recovery_Phase; Event : Recovery_Event)
     return Boolean
   is (case Phase is
         when Serving   => Event in Commands_Expired | Request_Arrived,
         when Aborting  => Event in Abort_Settled | Abort_Unsettled,
         when Resetting => Event in Reset_Succeeded | Reset_Failed,
         when Offline   => Event = Request_Arrived);

   function Next (Phase : Recovery_Phase; Event : Recovery_Event)
     return Recovery_Phase
     with Pre => Expected (Phase, Event),
          Post =>
            (case Phase is
               when Serving =>
                 (if Event = Commands_Expired then Next'Result = Aborting
                  else Next'Result = Serving),
               when Aborting =>
                 (if Event = Abort_Settled then Next'Result = Serving
                  else Next'Result = Resetting),
               when Resetting =>
                 (if Event = Reset_Succeeded then Next'Result = Serving
                  else Next'Result = Offline),
               when Offline => Next'Result = Resetting);

   --  Only a successful reset or a settled abort returns to Serving, and
   --  Offline is left only by trying a reset.
   procedure Lemma_Recovery_Paths (Phase : Recovery_Phase; Event : Recovery_Event)
     with Ghost,
          Pre  => Expected (Phase, Event),
          Post =>
            (if Phase /= Serving and then Next (Phase, Event) = Serving
             then Event in Abort_Settled | Reset_Succeeded)
            and then (if Phase = Offline then Next (Phase, Event) = Resetting)
            and then (if Next (Phase, Event) = Offline
                      then Phase = Resetting and then Event = Reset_Failed);

   ---------------------------------------------------------------------------
   --  A request's fate after one pass.
   ---------------------------------------------------------------------------
   type Pass_Result is
     (Pass_Complete,     --  every command completed (successfully or not)
      Pass_Recovered,    --  commands expired; recovery ended Serving
      Pass_Offline);     --  recovery left the controller Offline

   type Request_Outcome is
     (Completed,        --  every command completed successfully
      Device_Error,     --  the controller failed a command (its status)
      Timed_Out,        --  still not done after Command_Attempts passes
      Device_Offline);  --  the controller could not be reset

   type Verdict (Final : Boolean := True) is record
      case Final is
         when True  => Outcome : Request_Outcome;
         when False => Next_Attempt : Attempt;
      end case;
   end record;

   --  After pass Current ended with Result (Failed: some command completed
   --  with an error status): finish the request or run another pass.
   function Decide
     (Current : Attempt; Result : Pass_Result; Failed : Boolean) return Verdict
   is (case Result is
         when Pass_Complete =>
           (Final => True,
            Outcome => (if Failed then Device_Error else Completed)),
         when Pass_Recovered =>
           (if Failed then (Final => True, Outcome => Device_Error)
            elsif Current < Command_Attempts
            then (Final => False, Next_Attempt => Current + 1)
            else (Final => True, Outcome => Timed_Out)),
         when Pass_Offline => (Final => True, Outcome => Device_Offline))
     with Post =>
       --  Another pass only after a recovery that ended Serving, and with
       --  a strictly later attempt: a request runs at most
       --  Command_Attempts passes, so every request ends.
       (if not Decide'Result.Final
        then Result = Pass_Recovered and then not Failed
             and then Decide'Result.Next_Attempt = Current + 1)
       and then (if Decide'Result.Final and then Decide'Result.Outcome = Completed
                 then Result = Pass_Complete and then not Failed)
       and then (if Current = Command_Attempts then Decide'Result.Final);

end NVMe_Recovery;
