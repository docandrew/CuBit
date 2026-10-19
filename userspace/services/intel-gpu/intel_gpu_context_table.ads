with Interfaces; use Interfaces;
with System;
with Intel_GPU_G2H_Credits;
with Intel_GPU_GuC_Context_Event;
with Intel_GPU_GuC_Context_Lifecycle;
with Intel_GPU_GuC_Context_Session;
generic
   -- The most contexts: GuC IDs First_ID .. First_ID + Capacity - 1. Their
   -- records live in caller storage (Attach) and only Grown are backed.
   Capacity : Positive;
   with package Driver is new Intel_GPU_GuC_Context_Session (<>);
   with function Owner_Ready return Boolean;
   with procedure Retain
     (Payload : Intel_GPU_GuC_Context_Event.Words; Fence : Unsigned_16;
      Success : out Boolean);
   First_ID : Unsigned_32 := 1;
   -- G2H words kept for answers to this table's requests (the G2H ring
   -- less the unsolicited reserve): Intel_GPU_G2H_Credits.
   Reply_Words : Intel_GPU_G2H_Credits.Word_Count;
package Intel_GPU_Context_Table is
   pragma Compile_Time_Error (Capacity > 65534, "context ID space exhausted");
   pragma Compile_Time_Error
     (First_ID = 0 or else First_ID > 65535 - Unsigned_32 (Capacity),
      "context ID interval invalid");
   -- Driver-internal, serialized/non-reentrant table for ONE CT lifetime.
   -- Exclusive per-context GPU backing and publication facts come from trusted owners,
   -- never directly from an IPC client. The caller chooses each context's ID
   -- (the session table owns ID allocation) and frees it with Release, only
   -- after the GuC acknowledged its deregistration or never saw it.
   -- Driver's ownership predicate must refer to this same transport lifetime.
   type Table is limited private;
   No_Context : constant Unsigned_32 := 65535;
   subtype Context_Count is Natural range 0 .. Capacity;
   -- Bytes of storage that hold the first Count context records.
   function Storage_Bytes (Count : Context_Count) return Unsigned_64;
   -- Use Storage (Storage_Bytes (Capacity) bytes of address space, of which
   -- the caller backs what Grow needs) for the records. Once, while empty.
   procedure Attach (Object : in out Table; Storage : System.Address);
   function Grown (Object : Table) return Context_Count;
   -- Make records for IDs up to First_ID + To - 1. The caller has backed the
   -- first Storage_Bytes (To) bytes of the attached storage first.
   procedure Grow (Object : in out Table; To : Context_Count; Accepted : out Boolean);
   -- The last ID with a record (First_ID - 1 when none).
   function Last_ID (Object : Table) return Unsigned_32 is
     (First_ID + Unsigned_32 (Grown (Object)) - 1);
   -- ID names an opened context (not Released since).
   function In_Use (Object : Table; ID : Unsigned_32) return Boolean;
   -- The session context ID was opened for (zero: internal or not in use),
   -- whatever its state. Observation only, never authority.
   function Session_Of (Object : Table; ID : Unsigned_32) return Unsigned_64;
   function Can_Run_And_Retire (Object : Table; ID : Unsigned_32) return Boolean;
   -- Submission admission: open, unheld, live context resting either
   -- Enabled (resident between submissions) or Disabled (parked).
   function Can_Submit (Object : Table; ID : Unsigned_32) return Boolean;
   function Failed (Object : Table) return Boolean;
   -- Session comes from authenticated render admission, never IPC words.
   -- ID when context ID is open for Session, live and not retired; else
   -- No_Context. Zero is reserved for internal/bootstrap contexts.
   function Session_Context
     (Object : Table; ID : Unsigned_32; Session : Unsigned_64) return Unsigned_32;
   -- Immediately and irreversibly closes session lookup and new work.
   -- Retained ID for trusted disable/drain even if device ownership is
   -- lost. Does NOT claim hardware stopped, free backing, or remove routes.
   procedure Retire_Session
     (Object : in out Table; ID : Unsigned_32; Session : Unsigned_64);
   -- Trusted dispatcher only: Work_Drained certifies completion of outstanding
   -- work and deferred publication, not merely absence of a VM-update hold.
   -- Also requires retired admission, no hold, and acknowledged Disabled.
   -- Completion retains context ID/backing; this is NOT reclamation.
   procedure Deregister_Retired
     (Object : in out Table; ID : Unsigned_32; Work_Drained : Boolean;
      Status : out Driver.Result);
   function State (Object : Table; ID : Unsigned_32)
     return Intel_GPU_GuC_Context_Lifecycle.Phase;
   -- Open context ID (First_ID .. Last_ID, not in use) for Session (zero
   -- for internal contexts). A failed initialization still takes the ID:
   -- it names the retained failed context though Accepted is False. At
   -- most one context per nonzero session. Caller retains backing on failure.
   procedure Open
     (Object : in out Table; ID : Unsigned_32; GPU_Start, Pin_Bias : Unsigned_64;
      Quantum_Us, Preemption_Us : Unsigned_32;
      Preempt_To_Idle : Boolean; Accepted : out Boolean;
      Session : Unsigned_64 := 0);
   -- Free a retired context's ID and record for reuse: only when the GuC
   -- acknowledged its deregistration (Deregistered) or never saw it (no
   -- request was ever sent). Quarantined contexts are never released.
   procedure Release (Object : in out Table; ID : Unsigned_32; Accepted : out Boolean);
   -- Release would accept now.
   function Releasable (Object : Table; ID : Unsigned_32) return Boolean;
   procedure Submit
     (Object : in out Table; ID : Unsigned_32;
      Action : Intel_GPU_GuC_Context_Lifecycle.Operation; Status : out Driver.Result);
   procedure Notify_Work
     (Object : in out Table; ID : Unsigned_32; Tail_Published : Boolean;
      Status : out Driver.Result);
   -- Trusted VM-update exclusion. Acquire before publishing ANY new ring tail,
   -- not merely before Notify_Work: enabled GuC may observe a tail directly.
   -- Serialized with ring publication and dispatch; no IPC caller supplies ID.
   -- Holds do not drain existing work or acknowledge scheduling disable.
   function Work_Allowed (Object : Table; ID : Unsigned_32) return Boolean;
   -- Ring-tail publication admission: as Work_Allowed, but a parked
   -- (acknowledged Disabled) context may also take a new tail, because the
   -- MODE_SET enable that follows submits it (H6).
   function Publish_Allowed (Object : Table; ID : Unsigned_32) return Boolean;
   procedure Hold_Work
     (Object : in out Table; ID : Unsigned_32; Accepted : out Boolean);
   -- Release only after successful publication/invalidation and acknowledged
   -- re-enable, or Keep_Disabled=True to retain acknowledged disabled state
   -- for a submit-on-demand context. Neither mode changes GPU scheduling.
   -- Failures keep the hold until permanent session retirement.
   -- The caller establishes those hardware conditions; this checks scheduling
   -- state/ownership/retirement, not GPU visibility. Nested holds rejected.
   procedure Release_Work
     (Object : in out Table; ID : Unsigned_32; Accepted : out Boolean;
      Keep_Disabled : Boolean := False);
   type Dispatch_Result is (Delivered, Retained, Context_Fault, Transport_Fault);
   procedure Dispatch
     (Object : in out Table; Payload : Intel_GPU_GuC_Context_Event.Words;
      Fence : Unsigned_16; ID : out Unsigned_32; Status : out Dispatch_Result);
   procedure Fail (Object : in out Table);
   -- G2H answer words reserved now (diagnostics).
   function Reply_Words_In_Use (Object : Table) return Natural;
private
   subtype Ordinal is Positive range 1 .. Capacity;
   -- Draining progress (Intel_GPU_Context_Table.Draining).
   type Progress is record
      Started, Finished : Boolean := False;
      First, Previous : Unsigned_64 := 0;
      Polls : Natural range 0 .. 100_000 := 0;
   end record;
   type Context_Record is limited record
      Item : Driver.Session;
      Opened, Retired, Work_Held : Boolean := False;
      Owner : Unsigned_64 := 0;
      -- G2H answer words this context holds in Credits (its last-known
      -- Driver.Credits_Held). A quarantined context keeps them: its answer
      -- may still arrive.
      Charged : Natural := 0;
      Drain : Progress;
   end record;
   type Record_Array is array (Ordinal) of Context_Record;
   type Record_Access is access all Record_Array;
   pragma No_Strict_Aliasing (Record_Access);
   type Table is limited record
      Items : Record_Access := null;
      Count : Context_Count := 0;
      Broken : Boolean := False;
      Credits : Intel_GPU_G2H_Credits.Pool;
      Configured : Boolean := False;
   end record;
   function Known (Object : Table; ID : Unsigned_32) return Boolean is
     (Object.Items /= null and then ID >= First_ID and then
      ID - First_ID < Unsigned_32 (Object.Count) and then
      Object.Items (Natural (ID - First_ID) + 1).Opened);
   function Index (ID : Unsigned_32) return Ordinal is (Natural (ID - First_ID) + 1)
     with Pre => ID >= First_ID and then ID - First_ID < Unsigned_32 (Capacity);
end Intel_GPU_Context_Table;
