------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Display's log: each line to the debug console and, when the profile
--  grants logstore, as a typed record in Display's log ring (CuBit.Logging),
--  so Boot Logs and Logs show it on hardware with no serial console.
------------------------------------------------------------------------------
package Display_Logs is
   --  Echo Text; publish each complete line it ends. Logging never blocks:
   --  a full ring sheds the record and counts it.
   procedure Write (Text : String);
end Display_Logs;
