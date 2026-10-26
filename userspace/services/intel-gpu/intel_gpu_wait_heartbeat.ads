------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The rate bound for intel-gpu's hardware-wait heartbeat lines: when a
--  bring-up wait (GuC load, reset, power wells, devmgr grants) is slow
--  enough to say so, and how often it may say so again.
--
--  @description
--  Every pause of a polling wait is a Pulse. Pulses closer together than
--  Wait_Gap_Us belong to one wait. A wait earns one entry line once it has
--  lasted Slow_Threshold_Us, then one progress line per Period_Us, and at
--  most Lines_Per_Wait lines in all: a hang shows where it is, with the
--  polled register, without flooding the boot-logs view. Pure: the caller
--  reads the clock (microseconds) and writes the line.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Intel_GPU_Wait_Heartbeat with SPARK_Mode, Pure is

   subtype Microseconds is Unsigned_64;

   Slow_Threshold_Us : constant Microseconds := 20_000;
   Period_Us         : constant Microseconds := 500_000;
   Wait_Gap_Us       : constant Microseconds := 100_000;
   Lines_Per_Wait    : constant := 16;

   type Line_Count is range 0 .. Lines_Per_Wait;

   Poll_Limit : constant := 2 ** 62;
   type Poll_Count is range 0 .. Poll_Limit;

   type Line_Kind is (None, Entry_Line, Progress_Line);

   type Monitor is record
      Active     : Boolean := False;
      Started    : Microseconds := 0;
      Last_Pulse : Microseconds := 0;
      Last_Line  : Microseconds := 0;
      Lines      : Line_Count := 0;
      Polls      : Poll_Count := 0;
   end record;

   function Elapsed (M : Monitor; Now : Microseconds) return Microseconds is
     (if Now >= M.Started then Now - M.Started else 0);

   --  One pause of a wait at time Now. Line says whether to write a line
   --  now, and which. Bounds: at most Lines_Per_Wait lines per wait, the
   --  entry line only after Slow_Threshold_Us, and progress lines at least
   --  Period_Us apart.
   procedure Pulse (M : in out Monitor; Now : Microseconds; Line : out Line_Kind)
   with Post => M.Active
     and then M.Lines <= Lines_Per_Wait
     and then (if Line = None then M.Lines = (if M'Old.Active and then
                 Now >= M'Old.Last_Pulse and then Now - M'Old.Last_Pulse < Wait_Gap_Us
                 then M'Old.Lines else 0))
     and then (if Line = Entry_Line then M.Lines = 1
               and then Elapsed (M, Now) >= Slow_Threshold_Us)
     and then (if Line = Progress_Line then
                 M'Old.Active and then M.Lines = M'Old.Lines + 1
                 and then M'Old.Lines >= 1
                 and then Now >= M'Old.Last_Line
                 and then Now - M'Old.Last_Line >= Period_Us)
     and then (if Line /= None then M.Last_Line = Now);

end Intel_GPU_Wait_Heartbeat;
