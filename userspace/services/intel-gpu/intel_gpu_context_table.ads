with Interfaces; use Interfaces;
with Intel_GPU_Context_Routes;
with Intel_GPU_Fence_Ranges;
with Intel_GPU_GuC_Context_Event;
with Intel_GPU_GuC_Context_Lifecycle;
with Intel_GPU_GuC_Context_Session;
generic
   Capacity : Positive;
   First_Fence, Last_Fence : Unsigned_16;
   with package Driver is new Intel_GPU_GuC_Context_Session (<>);
   with function Owner_Ready return Boolean;
   with procedure Retain
     (Payload : Intel_GPU_GuC_Context_Event.Words; Fence : Unsigned_16;
      Success : out Boolean);
   First_ID : Unsigned_32 := 1;
package Intel_GPU_Context_Table is
   pragma Compile_Time_Error (Capacity > 65534, "context ID space exhausted");
   pragma Compile_Time_Error
     (First_ID = 0 or else First_ID > 65535 - Unsigned_32 (Capacity),
      "context ID interval invalid");
   -- Driver-internal, serialized/non-reentrant table for ONE CT lifetime.
   -- Exclusive per-context GPU backing and publication facts come from trusted owners,
   -- never directly from an IPC client. No deletion, ID reuse or backing free.
   -- Driver's ownership predicate must refer to this same transport lifetime.
   type Table is limited private;
   No_Context : constant Unsigned_32 := 65535;
   function Count (Object : Table) return Natural;
   function Failed (Object : Table) return Boolean;
   function Owns_Fence (Object : Table; Fence : Unsigned_16) return Boolean;
   -- Session comes from authenticated render admission, never IPC words.
   -- Zero is reserved for internal/bootstrap contexts and never resolves.
   function Session_Context (Object : Table; Session : Unsigned_64) return Unsigned_32;
   -- Immediately and irreversibly closes session lookup and new work.
   -- Returns retained ID for trusted disable/drain even if device ownership
   -- is lost. Does NOT claim hardware stopped, free backing, or remove routes.
   procedure Retire_Session
     (Object : in out Table; Session : Unsigned_64; ID : out Unsigned_32);
   -- Trusted dispatcher only: Work_Drained certifies completion of outstanding
   -- work and deferred publication, not merely absence of a VM-update hold.
   -- Also requires retired admission, no hold, and acknowledged Disabled.
   -- Completion retains ID/fence routes/backing; this is NOT reclamation.
   procedure Deregister_Retired
     (Object : in out Table; ID : Unsigned_32; Work_Drained : Boolean;
      Status : out Driver.Result);
   function State (Object : Table; ID : Unsigned_32)
     return Intel_GPU_GuC_Context_Lifecycle.Phase;
   procedure Open
     (Object : in out Table; GPU_Start, Pin_Bias : Unsigned_64;
      Fence_Count : Natural; Quantum_Us, Preemption_Us : Unsigned_32;
      Preempt_To_Idle : Boolean; ID : out Unsigned_32; Accepted : out Boolean;
      Session : Unsigned_64 := 0);
   -- A failed initialization can still reserve ID/range permanently. In that
   -- case ID names the retained failed session even though Accepted is False.
   -- No_Context means no slot was reserved. Caller retains backing on failure.
   -- At most one context per nonzero session for this transport lifetime.
   -- Failed records retain the association, preventing accidental replacement.
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
private
   package Routes is new Intel_GPU_Context_Routes (Capacity);
   package Fences is new Intel_GPU_Fence_Ranges (First_Fence, Last_Fence);
   type Sessions is array (Positive range 1 .. Capacity) of Driver.Session;
   type Owners is array (Positive range 1 .. Capacity) of Unsigned_64;
   type Retirement_Flags is array (Positive range 1 .. Capacity) of Boolean;
   type Table is limited record
      Used : Natural range 0 .. Capacity := 0;
      Broken : Boolean := False;
      Routing : Routes.Registry;
      Ledger : Fences.Ledger;
      Items : Sessions;
      Session_Owners : Owners := [others => 0];
      Retired : Retirement_Flags := [others => False];
      Work_Held : Retirement_Flags := [others => False];
   end record;
end Intel_GPU_Context_Table;
