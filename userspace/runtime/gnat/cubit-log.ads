pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Log_Records;

--  Everyday logging for services and apps (docs/typed-logging.md):
--
--     CuBit.Log.Info ("netmgr: lease acquired");
--     CuBit.Log.Warning ("netmgr: no answer from 10.0.2.2");
--
--  Each record goes to logstore through the program's logstore binding
--  (manifest: request-service logstore read-write logstore) and is echoed to
--  the debug console. Records below the minimum logstore keeps are dropped
--  here, before any IPC; the minimum is learned from logstore's replies.
--
--  Queued (the default): records wait in a bounded in-process FIFO and are
--  submitted asynchronously. The program's event loop calls Pump, and hands
--  completions whose token Owns claims to Collect. Tokens in TOKEN_BASE ..
--  TOKEN_LAST are reserved for this package.
--
--  Immediate: each record is published with a synchronous call as it is
--  written; for programs without an event loop (startup, exit paths, simple
--  services that sleep). It never touches the completion queue.
--
--  Without a logstore binding records are only echoed. One task per program:
--  calls are serialized by the program's own loop.
package CuBit.Log is
   package Logs renames CuBit.Log_Records;

   type Delivery is (Queued, Immediate);
   procedure Set_Delivery (Mode : Delivery);
   --  Echo each record to the debug console as well (default on).
   procedure Set_Echo (Enabled : Boolean);

   procedure Write (Level : Logs.Severity; Text : String);
   procedure Trace (Text : String);
   procedure Debug (Text : String);
   procedure Info (Text : String);
   procedure Warning (Text : String);
   procedure Error (Text : String);
   procedure Critical (Text : String);
   --  Whether a record at Level would be kept, as far as is known: skip
   --  building expensive text when it would not.
   function Wanted (Level : Logs.Severity) return Boolean;

   --  Queued delivery: submit the next record if none is in flight.
   procedure Pump;
   TOKEN_BASE : constant Unsigned_64 := 16#4C47_0000_0000_0000#;
   TOKEN_LAST : constant Unsigned_64 := 16#4C47_FFFF_FFFF_FFFF#;
   function Owns (Token : Unsigned_64) return Boolean is (Token in TOKEN_BASE .. TOKEN_LAST);
   procedure Collect (Completion : CuBit.Messages.CompletionEntry);
   --  Publish everything still queued, synchronously (before exiting).
   procedure Flush;

   --  Records not delivered: queue full, logstore unavailable or refusing.
   --  Records under the minimum are not losses.
   function Lost return Unsigned_64;
   function Queued_Count return Natural;
end CuBit.Log;
