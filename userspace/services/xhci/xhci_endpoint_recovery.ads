------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What an interrupt-IN pipe does with each transfer completion, and the
--  escalation ladder that recovers a halted endpoint.
--
--  @description
--  A Stall, Babble, USB Transaction Error or Split Transaction Error halts
--  the endpoint in the controller (xHCI 1.2, 4.10.2.1, 4.10.2.3-4,
--  4.10.2.7): it stops servicing the ring, so re-queueing TRBs does
--  nothing. Each Transaction Error event already stands for CErr (3)
--  consecutive bus errors. Recovery escalates, as Linux does:
--
--    1. Soft retry (transaction and split errors only, Soft_Limit times in
--       a row): Reset Endpoint with TSP=1 (4.6.8.1) keeps the host data
--       toggle and the queued TRBs; ringing the doorbell retries the same
--       transfer (Linux xhci-ring.c, "soft retry").
--    2. Hard reset (Hard_Limit times in a row): Reset Endpoint with TSP=0
--       (the host toggle returns to DATA0), Set TR Dequeue Pointer past the
--       abandoned TRBs (4.6.10), and CLEAR_FEATURE(ENDPOINT_HALT) to the
--       device for EVERY halting error, which also returns the device's
--       toggle to DATA0 (USB 2.0 9.4.5); without it the two toggles
--       disagree after a transaction error and every later report is
--       dropped as a duplicate. Then fresh TRBs.
--    3. Port reset: the driver releases the device's slots and the port
--       machine resets and re-enumerates it, the software replug
--       (XHCI_Port_Machine.Recover); Port_Reset_Limit times, with backoff
--       Port_Reset_Delay, counted per port by the driver.
--  Missed Service (4.10.3.2) only needs the TRB replaced. A delivered
--  report clears the soft and hard counts.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with XHCI_Registers; use XHCI_Registers;

package XHCI_Endpoint_Recovery with SPARK_Mode => On, Pure is

   Soft_Limit : constant := 3;
   Hard_Limit : constant := 2;
   subtype Soft_Count is Natural range 0 .. Soft_Limit;
   subtype Hard_Count is Natural range 0 .. Hard_Limit;

   Port_Reset_Limit : constant := 3;
   subtype Port_Reset_Count is Natural range 0 .. Port_Reset_Limit;
   Port_Reset_Base_Delay_Ms : constant Unsigned_64 := 1_000;
   --  A port whose device delivered for this long since its last port
   --  reset starts its count afresh.
   Port_Reset_Forget_Ms : constant Unsigned_64 := 60_000;
   --  Backoff before the Count+1-th port reset: 1 s, 2 s, 4 s.
   function Port_Reset_Delay (Count : Port_Reset_Count) return Unsigned_64 is
     (Port_Reset_Base_Delay_Ms * 2 ** Count)
   with Post => Port_Reset_Delay'Result >= Port_Reset_Base_Delay_Ms;

   type Disposition is
     (Deliver,        --  data arrived (success or short packet): decode, re-queue
      Replace,        --  no data, endpoint still running: re-queue
      Recover,        --  endpoint halted: run the ladder
      Ignore);        --  endpoint stopped by a command: the TRB stays queued

   function Halting (Code : Completion_Code) return Boolean is
     (Code = CC_STALL or else Code = CC_BABBLE or else Code = CC_TRANSACTION_ERROR
      or else Code = CC_SPLIT_TRANSACTION_ERROR);
   function Soft_Retryable (Code : Completion_Code) return Boolean is
     (Code = CC_TRANSACTION_ERROR or else Code = CC_SPLIT_TRANSACTION_ERROR);

   --  Context_Halted: the endpoint context's EP State reads Halted.
   function Classify (Code : Completion_Code; Context_Halted : Boolean) return Disposition is
     (if Code = CC_SUCCESS or else Code = CC_SHORT_PACKET then Deliver
      elsif Halting (Code) then Recover
      elsif Code = CC_STOPPED or else Code = CC_STOPPED_LENGTH_INVALID
        or else Code = CC_STOPPED_SHORT_PACKET then Ignore
      elsif Context_Halted then Recover
      else Replace);

   type Step is
     (Running,
      Need_Soft_Reset,   --  Reset Endpoint TSP=1
      Await_Soft_Reset,
      Need_Doorbell,     --  ring the doorbell: retry the same transfer
      Need_Reset,        --  Reset Endpoint TSP=0
      Await_Reset,
      Need_Dequeue,      --  Set TR Dequeue Pointer
      Await_Dequeue,
      Need_Clear_Halt,   --  CLEAR_FEATURE(ENDPOINT_HALT) on EP0
      Await_Clear_Halt,
      Need_Requeue,      --  fresh TRBs and the doorbell
      Need_Port_Reset);  --  the driver resets and re-enumerates the port

   type Pipe_Recovery is record
      Current : Step := Running;
      Soft    : Soft_Count := 0;
      Hard    : Hard_Count := 0;
   end record;

   function Awaiting (R : Pipe_Recovery) return Boolean is
     (R.Current in Await_Soft_Reset | Await_Reset | Await_Dequeue | Await_Clear_Halt);
   function Issuing (R : Pipe_Recovery) return Boolean is
     (R.Current in Need_Soft_Reset | Need_Reset | Need_Dequeue | Need_Clear_Halt);

   --  A completion with Disposition Recover arrived.
   procedure Halted (R : in out Pipe_Recovery; Code : Completion_Code)
   with Pre => R.Current = Running,
     Post =>
       (if Soft_Retryable (Code) and then R'Old.Soft < Soft_Limit then
          R.Current = Need_Soft_Reset and then R.Soft = R'Old.Soft + 1
          and then R.Hard = R'Old.Hard
        elsif R'Old.Hard < Hard_Limit then
          R.Current = Need_Reset and then R.Hard = R'Old.Hard + 1
        else
          R.Current = Need_Port_Reset);

   procedure Issued (R : in out Pipe_Recovery)
   with Pre => Issuing (R),
     Post => R.Current = (case R'Old.Current is
                            when Need_Soft_Reset => Await_Soft_Reset,
                            when Need_Reset => Await_Reset,
                            when Need_Dequeue => Await_Dequeue,
                            when others => Await_Clear_Halt)
       and then R.Soft = R'Old.Soft and then R.Hard = R'Old.Hard;

   --  The awaited command or control transfer completed with Code. Reset
   --  Endpoint on an endpoint that was not halted completes with Context
   --  State Error (4.6.8): stopped either way, so that is success too. A
   --  refused step escalates to the port reset.
   procedure Completed (R : in out Pipe_Recovery; Code : Completion_Code)
   with Pre => Awaiting (R),
     Post =>
       (case R'Old.Current is
          when Await_Soft_Reset =>
            R.Current = (if Code = CC_SUCCESS or else Code = CC_CONTEXT_STATE_ERROR
                         then Need_Doorbell else Need_Port_Reset),
          when Await_Reset =>
            R.Current = (if Code = CC_SUCCESS or else Code = CC_CONTEXT_STATE_ERROR
                         then Need_Dequeue else Need_Port_Reset),
          when Await_Dequeue =>
            R.Current = (if Code = CC_SUCCESS then Need_Clear_Halt else Need_Port_Reset),
          when others =>
            --  A device that refuses CLEAR_FEATURE may still deliver; the
            --  controller's side is clean, so re-queue regardless.
            R.Current = Need_Requeue)
       and then R.Soft = R'Old.Soft and then R.Hard = R'Old.Hard;

   procedure Rang (R : in out Pipe_Recovery)
   with Pre => R.Current in Need_Doorbell | Need_Requeue,
     Post => R.Current = Running and then R.Soft = R'Old.Soft and then R.Hard = R'Old.Hard;

   --  A report was delivered: the endpoint is healthy again.
   procedure Delivered (R : in out Pipe_Recovery)
   with Pre => R.Current = Running,
     Post => R.Current = Running and then R.Soft = 0 and then R.Hard = 0;

   --  The command or transfer timed out: straight to the port reset.
   procedure Escalate (R : in out Pipe_Recovery)
   with Post => R.Current = Need_Port_Reset;

end XHCI_Endpoint_Recovery;
