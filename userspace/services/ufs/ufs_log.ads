------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Bring-up log lines in the hardware convention (coordination
--  release-stack.md): one compact line per stage,
--    ufs: stage <name> ok|fail <reason> [key=value ...]
--  register and status values in hex on failure, and one summary,
--    ufs: bring-up reached <stage> (n/total)  or  ufs: bring-up complete
--  The user reads them from a photo of the laptop's screen: each goes to
--  logstore (Information, Warning on failure) and to serial.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package UFS_Log is
   type Stage is
     (Identify, Enable, Link_Up, Lists, NOP, Device_Init, Device_Descriptor,
      Geometry, LUN, Unit_Ready, Inquiry, Capacity, Partitions);

   procedure Passed (Item : Stage; Details : String := "");
   procedure Failed (Item : Stage; Reason : String; Details : String := "");

   --  The summary: how far bring-up got. Serving: what the driver offers.
   procedure Summary (Serving : String);

   --  " key=0x..." with Width hex digits, and " key=n".
   function Hex_Field (Key : String; Value : Unsigned_64; Width : Positive)
     return String;
   function Field (Key : String; Value : Unsigned_64) return String;

   --  One line outside the stage list.
   procedure Note (Text : String);
end UFS_Log;
