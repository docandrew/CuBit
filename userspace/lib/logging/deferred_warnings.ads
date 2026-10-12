pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Log_Protocol;

--  Warnings from a service that starts before logstore (devmgr, procmgr).
--  Warn echoes at once and keeps the line; once logstore is registered,
--  Pump mints this process a publisher binding (its own capability-space
--  authority, logstore's Publisher_Slot) and publishes what was kept, then
--  each later warning directly. Bounded: lines beyond Capacity are counted
--  and reported as one Warning. Never waits for logstore.
generic
   --  This publisher's issuance in the shared driver budget
   --  (CuBit.Log_Protocol.Publisher_Tag).
   Issuance : CuBit.Log_Protocol.Publication_Issuance;
package Deferred_Warnings is
   procedure Warn (Text : String);
   --  The same at Information (kept and published like a warning).
   procedure Inform (Text : String);
   --  Call from the service's loop; cheap when nothing is pending.
   procedure Pump;
   --  Lines are kept for a logstore that is not running yet: the caller
   --  should wake within Pump_Interval_Ms to Pump.
   function Pending return Boolean;
   Pump_Interval_Ms : constant := 1_000;
   --  "0x..." for addresses and sizes in warnings.
   function Hex (Value : Unsigned_64) return String;
end Deferred_Warnings;
