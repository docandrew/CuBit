------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A damage record read from memory the compositor can still write.
--
--  @description
--  The record lives in a pool buffer the client keeps mapped writable:
--  any word may change while display.svc reads it. Decode is proved over
--  a value (Word_Array) and would be wrong over changing memory: a count
--  checked in one read and used in another could exceed Maximum_Boxes, and
--  in a -gnatp build index Boxes past its end. So nothing decodes the
--  shared words themselves: Snapshot_And_Decode reads each word exactly
--  once, in order, into a local copy, and decodes the copy. A word that
--  changes meanwhile changes only what the (validated) copy says.
--
--  Not SPARK: the reads are of memory outside the program's model. The
--  generic takes the read as a function so a test can change a word
--  between reads, deterministically (tests/display-frame-damage).
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
package CuBit.Display_Frame_Damage.Shared with SPARK_Mode => Off is
   generic
      with function Read (Index : Word_Index) return Unsigned_64;
   function Snapshot_And_Decode (Frame : Unsigned_64; Area : DP.Rectangle) return Decoding;

   --  The record at Location (one page another process may write).
   function Decode_At (Location : System.Address; Frame : Unsigned_64; Area : DP.Rectangle)
     return Decoding;
end CuBit.Display_Frame_Damage.Shared;
