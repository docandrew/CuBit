with Interfaces; use Interfaces;
with Intel_GPU_Render_Sessions;
package Intel_GPU_Render_Control with SPARK_Mode is
   -- Supervisor-only control protocol. Bind once from trusted bootstrap;
   -- Sender and Stamped_Tag must come from the kernel receive envelope.
   Label : constant Unsigned_32 := 16#0A21#;
   Close_Own_Label : constant Unsigned_32 := 16#0A2C#;
   Retirement_Label : constant Unsigned_32 := 16#0A2D#;
   Status_Label : constant Unsigned_32 := 16#0A2F#;
   Retirement_Pending : constant Unsigned_64 := 4;
   Version : constant Unsigned_64 := 1;
   Reserve : constant Unsigned_64 := 0;
   Activate : constant Unsigned_64 := 1;
   Abort_Session : constant Unsigned_64 := 2;
   OK : constant Unsigned_64 := 0;
   Denied : constant Unsigned_64 := 1;
   Bad_Request : constant Unsigned_64 := 2;
   Unavailable : constant Unsigned_64 := 3;
   Bad_State : constant Unsigned_64 := 4;
   type Drain_Facts is record
      Uncertain : Boolean := True;
      Work_Pending : Boolean := True;
      GPU_Stopped, Grants_Retired : Boolean := False;
   end record;
   -- Trusted observations, not request fields. OK is session quiescence, not
   -- backing/ID reclamation; pending and uncertainty must never count as OK.
   -- For a registered context, GPU_Stopped requires GuC deregistration done,
   -- not scheduling disable alone. A never-registered session needs separate
   -- proof that no registration was attempted or remains deferred.
   function Drain_Status (Facts : Drain_Facts) return Unsigned_64 is
     (if Facts.Uncertain then Unavailable
      elsif Facts.Work_Pending or not Facts.GPU_Stopped or not Facts.Grants_Retired
      then Retirement_Pending else OK)
     with Post => (Drain_Status'Result = OK) =
       (not Facts.Uncertain and not Facts.Work_Pending and
        Facts.GPU_Stopped and Facts.Grants_Retired);
   type Words is array (Natural range 0 .. 3) of Unsigned_64;
   type Controller is limited private;
   -- Issued-record metadata, including retired/quarantined records. This is
   -- NOT authorization: authenticate the kernel envelope with Resolve before
   -- admitting work. Zero rejects unissued identities, not just bad ranges.
   function Storage_Index (Object : Controller; Tag : Unsigned_64)
                          return Intel_GPU_Render_Sessions.Slot_Index;
   function Issued_Tag
     (Object : Controller; Index : Intel_GPU_Render_Sessions.Slot_Index)
      return Unsigned_64
     with Post => (if Issued_Tag'Result /= 0 then
       Index /= 0 and Storage_Index (Object, Issued_Tag'Result) = Index);
   -- Check the kernel sender/stamp before any backend MMIO observation.
   function Is_Broker
     (Object : Controller; Sender, Stamped_Tag : Unsigned_64) return Boolean;
   procedure Bind
     (Object : in out Controller; Broker, Broker_Tag : Unsigned_64);
   -- Authenticate an activation before inspecting its driver-side recipient
   -- endpoint. Returns the reserved incarnation, never authority by itself.
   function Activation_Identity
     (Object : Controller; Sender, Stamped_Tag : Unsigned_64;
      Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Request : Words) return Unsigned_64
     with Post => (Activation_Identity'Result = 0 or else
       (Activation_Identity'Result = Request (1) and
        Request (2) > Intel_GPU_Render_Sessions.Tag_Base and
        Request (2) <= Intel_GPU_Render_Sessions.Tag_Last));
   -- Request [version, captured generation32/PID32, session-tag, operation].
   -- Reserve uses tag zero. Activate follows successful endpoint delegation;
   -- Abort retires a reservation/active session without freeing GPU backing.
   -- Response [status, version, session-tag-or-zero, recipient-slot-or-zero].
   -- Successful Reserve returns the dedicated driver recipient slot40..55.
   -- All other replies return slot zero. Never derive a slot from the tag.
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
      Request : Words; Response : out Words;
      Recipient_Ready : Boolean := False)
     with Post => (if Request (3) = Activate and not Recipient_Ready
                   then Response (0) /= OK);
   -- Recipient_Ready must come from a kernel endpoint/incarnation check of
   -- the immutable driver-side sharing slot, not an IPC request flag.
   function Resolve
     (Object : Controller; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64;
   -- Active-session observation only: [version,0,0,0] -> [status,version,0,0].
   -- Ready is trusted current driver/session state, never caller input or a
   -- cached identity snapshot. Success is not a lease or permission to submit.
   function Session_Status
     (Object : Controller; Sender, Stamped_Tag : Unsigned_64; Ready : Boolean;
      Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Request : Words) return Words
     with Post =>
       Session_Status'Result (0) <= Unavailable and
       Session_Status'Result (1) = Version and
       Session_Status'Result (2) = 0 and Session_Status'Result (3) = 0 and
       (if Resolve (Object, Sender, Stamped_Tag) = 0 then
          Session_Status'Result (0) = Denied) and
       (if Session_Status'Result (0) = OK then
          Ready and Resolve (Object, Sender, Stamped_Tag) /= 0 and
          Request_Label = Status_Label and Length = 4 and Flags = 0 and
          Reserved = 0 and Request = [Version, 0, 0, 0]);
   -- Cleanup-status authentication only, never new-work authority. Caller
   -- must pass the kernel envelope, not a tag returned in reply data.
   function Resolve_Retired
     (Object : Controller; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64;
   -- Application endpoint: [version,0,0,0], no caller-selected target.
   -- Authenticated ACTIVE sender/stamp only; closes admission before returning
   -- [OK,version,retired-tag,0]. Dispatcher must then retire its resources.
   -- Success is NOT GPU disable completion, grant drain or backing release.
   -- A repeated request is denied (no active session); never replay on an
   -- uncertain reply or infer that the capability slot can be replaced.
   procedure Close_Own
     (Object : in out Controller; Sender, Stamped_Tag : Unsigned_64;
      Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Request : Words; Response : out Words);
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
