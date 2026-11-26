------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  An inlet's ring: a Stream_Rings region a launcher lends for a program's
--  inlet (unix.stdin, typed inlets), which the launcher produces into and
--  the program reads. Unlike an outlet's broadcast ring, an inlet has one
--  reader and loses nothing: the producer writes only into room its reader
--  has freed (CONSUMER, the reader's cursor in the control page) and waits
--  otherwise, and ENDED tells the reader that no record follows the last.
--
--  @description
--  The launcher lends the region (CuBit.Launching.Lend_Ring, the pages the
--  program's manifest declares for the inlet); procmgr derives the
--  program's grant (writable, for CONSUMER); the libc serves the
--  descriptor the manifest maps onto the inlet from it (descriptor 0 for
--  unix.stdin). Records are byte runs of at most Maximum_Record bytes.
--
--  Needs no Ada run-time library (the libc uses it).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;

package CuBit.Inlet_Rings is

   Maximum_Record : constant := 4_096;

   type Put_Result is (Put, Full, Too_Large);

   --  The producer (the launcher): Length bytes at Data as one record of
   --  the region at Base (Pages declared), only into room the reader has
   --  freed. Full: try again once it has read more. Too_Large: longer than
   --  Maximum_Record (or the ring), never written.
   function Write (Base : Unsigned_64; Pages : Natural; Data : System.Address;
                   Length : Natural) return Put_Result;

   --  The producer is done: once the reader has taken every record, it
   --  reads the end of the stream.
   procedure Finish (Base : Unsigned_64);

   --  The reader (the program): the next record into Buffer (Maximum at
   --  least Maximum_Record): its length, 0 when none is ready.
   function Take (Base : Unsigned_64; Pages : Natural; Buffer : System.Address;
                  Maximum : Natural) return Natural;

   --  A record is ready for the reader.
   function Ready (Base : Unsigned_64) return Boolean;

   --  The producer finished and the reader has taken everything.
   function Finished (Base : Unsigned_64) return Boolean;

end CuBit.Inlet_Rings;
