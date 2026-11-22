------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  intel-gpu's bring-up stages, in the hardware bring-up logging convention
--  (coordination/release-stack.md; docs/device-matching.md, HW-002): one
--  line per stage, "intel-gpu: stage <name> ok|fail [reason] [key=value]",
--  and one summary, "intel-gpu: bring-up reached <stage> (<n>/<total>)" or
--  "intel-gpu: bring-up complete", so a photograph of the boot-logs view
--  shows how far the GPU got and why Desktop does or does not render on it.
------------------------------------------------------------------------------
package Intel_GPU_Bring_Up with SPARK_Mode is
   --  In order. Render is the last: Desktop's GPU sessions are admitted
   --  only once the render backend is ready.
   type Stage is
     (Identify, Registers, Forcewake, GGTT, Firmware, Reset, GuC, CT, Render);

   function Name (S : Stage) return String is
     (case S is
        when Identify  => "identify",
        when Registers => "registers",
        when Forcewake => "forcewake",
        when GGTT      => "ggtt",
        when Firmware  => "firmware",
        when Reset     => "reset",
        when GuC       => "guc",
        when CT        => "ct",
        when Render    => "render");

   Prefix : constant String := "intel-gpu: ";

   function Line (S : Stage; OK : Boolean; Details : String) return String is
     (Prefix & "stage " & Name (S) & (if OK then " ok" else " fail") &
      (if Details'Length > 0 then " " & Details else ""))
   with Pre => Details'Length <= 400;

   --  Record a stage's outcome; Next is the first stage not reported ok.
   procedure Report (S : Stage; OK : Boolean);
   function Next return Stage;
   function Complete return Boolean;

   --  The summary line, from what was reported.
   function Summary return String;
end Intel_GPU_Bring_Up;
