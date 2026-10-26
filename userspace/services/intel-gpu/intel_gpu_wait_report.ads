------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  intel-gpu's bring-up wait heartbeat (bring-up logging convention,
--  coordination/release-stack.md): a slow hardware or devmgr wait logs
--
--    intel-gpu: stage <site> waiting elapsed-ms=<n> polls=<n> reg=0x.. val=0x..
--    intel-gpu: stage <site> still waiting elapsed-ms=<n> polls=<n> reg=0x.. val=0x..
--
--  rate-bounded by Intel_GPU_Wait_Heartbeat, so a hang on a machine without
--  a serial port shows on screen where it is and what the register says.
--  Every pause of a polling wait calls Pulse; register reads of hardware
--  waits call Observe first. Pulse also lets the diagnostics publisher ask
--  for and use its logstore grant while bring-up is in a long wait.
------------------------------------------------------------------------------
with Interfaces;

package Intel_GPU_Wait_Report is

   type Site is
     (GuC_Load, Reset, Display_Power, Power_Well, Pipe_Power,
      Devmgr_Grant, Firmware_File);

   function Name (S : Site) return String is
     (case S is
         when GuC_Load      => "guc-load",
         when Reset         => "reset",
         when Display_Power => "display-power",
         when Power_Well    => "power-well",
         when Pipe_Power    => "pipe-power",
         when Devmgr_Grant  => "devmgr-grant",
         when Firmware_File => "firmware-file");

   --  Polls registers (the line shows the last one read) or waits on IPC.
   function Polls_Registers (S : Site) return Boolean is
     (S not in Devmgr_Grant | Firmware_File);

   --  The register a hardware wait just read (MMIO offset) and its value.
   procedure Observe (Offset, Value : Interfaces.Unsigned_32);

   --  One pause of a wait at S.
   procedure Pulse (S : Site);

end Intel_GPU_Wait_Report;
