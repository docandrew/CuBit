------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  intel-gpu's answer to its supervisor's Stop (PWR-003, docs/data-plane.md
--  "Control messages"): when the driver ends, and why.
--
--  @description
--  The main loop reads the kernel's events at the top of every turn
--  (Service_Kernel_Events); a Stop there is Noted. From then on the loop
--  keeps turning (GPU work already submitted is collected, requests still
--  answered) until Decide says End_Now: at once when the GPU queues are
--  idle, or Drain_Ms after the Stop whatever is in flight. The driver
--  touches no display register on the way out: pipes, planes and the last
--  frame stay as they are, so what is on the screen stays there.
--  For a power-off devmgr keeps intel-gpu to the end (Power_Tail), so the
--  screen holds until S5; this answers the Stop of a reboot, a halt or a
--  supervisor's restart.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Intel_GPU_Stop_Policy with SPARK_Mode, Pure is

   --  The longest the driver waits for submitted GPU work after a Stop,
   --  well inside devmgr's driver grace (power.svc's driver_grace_ms).
   Drain_Ms : constant := 100;

   --  The kernel's control message: label, length, Stop's kind (kernel
   --  ipc_labels.ads EVENT_CONTROL; the same as CuBit.Control_Events).
   Control_Label  : constant := 16#010E#;
   Control_Length : constant := 2;
   Stop_Kind      : constant := 1;

   --  Whether an event lane entry is the kernel's Stop (only the kernel
   --  posts the label: it refuses a process's events carrying it).
   function Is_Stop (Label : Unsigned_32; Length : Unsigned_8; Kind : Unsigned_64) return Boolean is
     (Label = Control_Label and then Length = Control_Length and then Kind = Stop_Kind);

   type State is record
      Requested : Boolean := False;
      Since     : Unsigned_64 := 0;   --  when the first Stop was read
   end record;

   --  A Stop was read at Now. A second one changes nothing: the drain
   --  bound counts from the first.
   procedure Note_Stop (S : in out State; Now : Unsigned_64)
   with Post => S.Requested
                and then S.Since = (if S'Old.Requested then S'Old.Since else Now);

   type Verdict is (Run, Drain, End_Now);

   function Decide (S : State; GPU_Idle : Boolean; Now : Unsigned_64) return Verdict is
     (if not S.Requested then Run
      elsif GPU_Idle then End_Now
      elsif Now >= S.Since and then Now - S.Since >= Drain_Ms then End_Now
      elsif Now < S.Since then End_Now   --  a clock that went back: do not wait on it
      else Drain)
   with Post => (if not S.Requested then Decide'Result = Run)
                and then (if S.Requested and then (GPU_Idle or else Now - S.Since >= Drain_Ms or else Now < S.Since)
                          then Decide'Result = End_Now)
                and then (if S.Requested then Decide'Result /= Run);

end Intel_GPU_Stop_Policy;
