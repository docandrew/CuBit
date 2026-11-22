------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The libc's log channel (ABI-002, the kernel debug console retired): a C,
--  C++ or Rust program's diagnostics become typed records in its logstore
--  ring (com.cubit.stdlog, CuBit.Log_Publish_Rings), as an Ada program's do
--  (CuBit.Console_Route), with the same pure cores: the line framer
--  (CuBit.Console_Lines), the record encoder (CuBit.Log_Records) and the
--  ring (CuBit.Channel_Rings, CuBit.Datagram_Rings).
--
--  @description
--  The channel is opened on first use through the program's "logstore"
--  startup entry (its manifest's (service "logstore" "logstore") request),
--  by a libc-local opener of the Log_Publish_Rings contract: the region is
--  this process's pages, granted read-only with events to logstore, which
--  grants back its index page (consumed index, minimum kept). No Ada
--  run-time library is needed (libc-ada.adc): no allocator, no handler, no
--  tasking, no finalization.
--
--  * cubit_debug_write (<cubit/debug.h>): each line one Debug record marked
--    as console text, as Console_Route makes them.
--  * Descriptor 2's text when the program declares a unix.stderr outlet
--    that no launcher lent a ring for: each line an Information record
--    marked as console text, with the field "outlet.stderr".
--  * The libc's own diagnostics: one record each, with its typed field
--    (Report: an unimplemented system call carries its number).
--
--  A program without the entry, or whose channel logstore refused, loses
--  the text (counted); there is no fallback to the kernel. Lines under the
--  minimum logstore keeps are not made. Calls are serialized by one lock;
--  text written while it is held (a failure inside) is counted, not kept.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Interfaces.C;
with System;
with CuBit.Log_Records;

package CuBit.Libc_Log is
   package Logs renames CuBit.Log_Records;

   --  <cubit/debug.h>'s text: framed into Debug console records.
   procedure Write_Console (Text : System.Address; Length : Interfaces.C.size_t);

   --  Descriptor 2's text (a unix.stderr outlet without a launcher's ring):
   --  framed into Information console records.
   procedure Write_Stderr (Text : System.Address; Length : Interfaces.C.size_t);

   --  Whether descriptor 2's text goes here: the program has a logstore
   --  entry (its channel may still be refused).
   function Takes_Stderr return Boolean;

   --  One record: Text at Level, with the unsigned field Field = Value
   --  (no field when Field is empty).
   procedure Report
     (Text : String; Level : Logs.Severity; Field : String; Value : Unsigned_64;
      Kind : Logs.Field_Kind := Logs.Unsigned_Integer)
   with Pre => Text'Length <= Logs.Maximum_Text_Bytes;

   --  The libc's record for an unimplemented system call ("cubit-libc:
   --  unimplemented system call N"): its number also in the field
   --  "syscall.number".
   procedure Unimplemented_System_Call (Number : Unsigned_64);

   --  Before the process exits: wait briefly (Flush_Wait_Ms at most) until
   --  logstore has taken what was published.
   Flush_Wait_Ms : constant := 200;
   procedure Flush;

   --  Records not delivered (no channel, shed by the ring, not made).
   function Lost return Unsigned_64;
end CuBit.Libc_Log;
