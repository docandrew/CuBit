with Interfaces; use Interfaces;
with Intel_GPU_Render_Sessions;
package Intel_GPU_Render_Control with SPARK_Mode is
   -- Supervisor-only control protocol. Bind once from trusted bootstrap;
   -- Sender and Stamped_Tag must come from the kernel receive envelope.
   Label : constant Unsigned_32 := 16#0A21#;
   Version : constant Unsigned_64 := 1;
   Reserve : constant Unsigned_64 := 0;
   Activate : constant Unsigned_64 := 1;
   Abort_Session : constant Unsigned_64 := 2;
   OK : constant Unsigned_64 := 0;
   Denied : constant Unsigned_64 := 1;
   Bad_Request : constant Unsigned_64 := 2;
   Unavailable : constant Unsigned_64 := 3;
   Bad_State : constant Unsigned_64 := 4;
   type Words is array (Natural range 0 .. 3) of Unsigned_64;
   type Controller is limited private;
   procedure Bind
     (Object : in out Controller; Broker, Broker_Tag : Unsigned_64);
   -- Request [version, captured generation32/PID32, session-tag, operation].
   -- Reserve uses tag zero. Activate follows successful endpoint delegation;
   -- Abort retires a reservation/active session without freeing GPU backing.
   -- Response [status, version, session-tag-or-zero, zero].
   -- Ready is supplied by the actual allocation/submission backend, not by
   -- the request or by merely observing firmware/engine startup.
   -- Supervisor must keep the captured recipient and selected source endpoint
   -- stable across policy evaluation and delegation. Lost/uncertain admission
   -- replies require Abort, never Activate as a retry. Dispatch is serialized;
   -- this controller neither supplies IPC transport nor revokes kernel caps.
   -- Abort is idempotent for a known identity/tag, including after quarantine.
   procedure Handle
     (Object : in out Controller; Sender, Stamped_Tag : Unsigned_64;
      Ready : Boolean; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words);
   function Resolve
     (Object : Controller; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64;
   -- Captured process incarnation for an authenticated ACTIVE session only.
   -- The dispatcher must compare this with its stable recipient endpoint
   -- before creating a buffer grant. This identity is not grant authority;
   -- never reconstruct a capability from its PID or accept it from app words.
   function Recipient_Identity
     (Object : Controller; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64
     with Post =>
       (if Resolve (Object, Sender, Stamped_Tag) = 0 then
           Recipient_Identity'Result = 0);
   -- Dispatcher-only retirement after an undelivered successful admission or
   -- session mutation reply. Use the original captured identity and session
   -- tag, never a later PID lookup. Idempotent; no mapping rollback, hardware
   -- completion, capability revocation or backing release is implied.
   procedure Reject_Delivery
     (Object : in out Controller; Identity, Tag : Unsigned_64);
   procedure Quarantine (Object : in out Controller);
private
   type Identities is array (1 .. Intel_GPU_Render_Sessions.Capacity) of Unsigned_64;
   type Controller is limited record
      Bound : Boolean := False;
      Broker, Broker_Tag : Unsigned_64 := 0;
      Sessions : Intel_GPU_Render_Sessions.Registry;
      Recipients : Identities := [others => 0];
   end record;
end Intel_GPU_Render_Control;
