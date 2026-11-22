------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The runtime switch that retires the kernel debug console for a program
--  (ABI-002, SYSCALL_WRITE): from Route_To (or Route_Awaiting) on, the
--  program's debugPrint text is published as Debug records in its logstore
--  ring, never written to the kernel.
--
--  @description
--  Each line of debugPrint text (CuBit.Console_Lines) becomes one Debug
--  record marked as console text, copied into the program's publisher ring
--  (CuBit.Logging, CuBit.Log_Publish_Rings: Shed_Newest, sheds counted and
--  reported by logstore as a gap). Lines under the minimum logstore keeps
--  are skipped before they are made; logstore keeps Information and above
--  unless lowered, so a shipped system keeps no debug text by default.
--  Test builds' logstore mirrors console text onto the serial console
--  (Test_Console_ABI), which the headless tests read.
--
--  Before the ring exists (Route_Awaiting: a program handed logstore at
--  run time), records wait in a process-local queue (CuAlloc), at most
--  Maximum_Queued_Bytes encoded; what does not fit is counted, and the
--  count is published as a Warning gap record when Attach drains the queue.
--  There is no fallback to the kernel.
--
--  One ring per program: CuBit.Log's records go through Emit here once
--  routed (so a program's lines stay in the order it wrote them), and its
--  echo becomes the console mark instead of a second, debugPrint copy.
--  Calls are serialized by a futex mutex (any thread may debugPrint); text
--  a nested debugPrint produces inside the route (a failure it reports
--  while publishing) is counted, not published.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Handles.Services;
with CuBit.Log_Records;

package CuBit.Console_Route is
   package Logstore_Handles renames CuBit.Handles.Services.Logstore;
   package Logs renames CuBit.Log_Records;

   --  The queue's ceiling, in encoded record bytes (about 400 short lines).
   Maximum_Queued_Bytes : constant := 64 * 1024;

   --  Route now, bound to Target (the program's Startup logstore
   --  function). Once only, and not after Route_Awaiting.
   procedure Route_To (Target : Logstore_Handles.Endpoint);

   --  Route now, bind later (Attach): records wait in the queue.
   procedure Route_Awaiting;
   --  Still Awaiting afterwards when logstore's channel could not be
   --  opened (memory exhausted): the caller may Attach again.
   procedure Attach (Target : Logstore_Handles.Endpoint);
   --  An introduced endpoint (Route_Named) is attached on its own thread,
   --  up to Attach_Attempts times, Attach_Retry_Ms apart.
   Attach_Attempts : constant := 100;
   Attach_Retry_Ms : constant := 100;

   function Routed return Boolean;
   --  Routed, and still waiting for Attach (records queue).
   function Awaiting return Boolean;

   --  Route by the program's startup-table entry Label (its declared
   --  logstore request): bound at once when the entry is Granted; when it
   --  is an Absent optional entry procmgr can introduce later
   --  (CuBit.Late_Introduction), routed now and attached on a thread of
   --  its own when the introduction arrives. Otherwise not routed (Routed
   --  stays False): nothing would ever take the text.
   procedure Route_Named (Label : String);

   --  Whether a record at Level would be kept (the minimum logstore last
   --  published; everything before the ring opens).
   function Wanted (Level : Logs.Severity) return Boolean;

   --  Value into the program's ring (or the queue). Console: mark it as
   --  console text, as a record the program would also have echoed.
   --  Accepted False: shed, not made, or the queue full (counted in Lost).
   procedure Emit (Value : Logs.Log_Record; Console : Boolean; Accepted : out Boolean)
   with Pre => Routed;

   --  Wait briefly (at most 200 ms) until logstore has taken what was
   --  published (before exiting).
   procedure Flush;

   --  The start code's hook after main returns (crt0.S, weak
   --  __cubit_program_end): Flush, so a program that ends at once does not
   --  lose its last lines with its ring.
   procedure Program_End
   with Export, Convention => C, External_Name => "__cubit_program_end";

   --  Records not delivered: shed by the ring, over the queue's ceiling,
   --  not made, or produced inside the route.
   function Lost return Unsigned_64;
end CuBit.Console_Route;
