pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Log_Records;

--  Everyday logging for services and apps (docs/typed-logging.md):
--
--     CuBit.Log.Info ("netmgr: lease acquired");
--     CuBit.Log.Warning ("netmgr: no answer from 10.0.2.2");
--
--  Each record goes to logstore through the program's logstore binding
--  (manifest: request-service logstore read-write logstore) and is echoed to
--  the debug console. Records below the minimum logstore keeps are dropped
--  here; the minimum is read from the program's log ring, not asked for.
--
--  Writing a record copies it into the program's log ring
--  (CuBit.Logging.Publisher, CuBit.Log_Publish_Rings), which logstore
--  drains: no IPC, no waiting, no event loop needed. A full ring sheds and
--  counts (Lost). Flush waits briefly for logstore to take what was written
--  (before exiting).
--
--  Without a logstore binding records are only echoed. One task per program:
--  calls are serialized by the program's own loop.
package CuBit.Log is
   package Logs renames CuBit.Log_Records;

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

   --  Wait briefly until logstore has taken what was written (before
   --  exiting).
   procedure Flush;

   --  Records not delivered: ring full (shed), no logstore, or not made
   --  (too long). Records under the minimum are not losses.
   function Lost return Unsigned_64;
end CuBit.Log;
