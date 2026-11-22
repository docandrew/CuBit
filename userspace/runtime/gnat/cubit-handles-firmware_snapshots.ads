------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A Call on a firmware snapshot handle, word for word
--  (Firmware_Snapshot_ABI). Programs use CuBit.Firmware_Snapshot, which
--  encodes and checks the words.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with Firmware_Snapshot_ABI;

package CuBit.Handles.Firmware_Snapshots is

   --  One call: Authority, the request words (Destination is the caller's
   --  own buffer for Copy_Table_Part), and where to put the reply (written
   --  only for Ok). The result word is an Outcome code.
   function Call
     (Authority : Firmware_Snapshot;
      Words     : Firmware_Snapshot_ABI.Request_Words;
      Reply     : System.Address) return Unsigned_64;

end CuBit.Handles.Firmware_Snapshots;
