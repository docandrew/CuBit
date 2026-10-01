with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Handles;
generic
   -- Resolve from the kernel's sender/tag envelope, never request words.
   with function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64;
   with function Owner_Ready return Boolean;
   First_Slot : Intel_GPU_Buffer_Backing.Slot := 1;
package Intel_GPU_Buffer_Requests is
   Label : constant Unsigned_32 := 16#0A22#;
   Version : constant Unsigned_64 := 1;
   Create : constant Unsigned_64 := 0;
   Close : constant Unsigned_64 := 1;
   OK : constant Unsigned_64 := 0;
   Denied : constant Unsigned_64 := 1;
   Bad_Request : constant Unsigned_64 := 2;
   Unavailable : constant Unsigned_64 := 3;
   type Words is array (Natural range 0 .. 3) of Unsigned_64;
   type Service is limited private;
   subtype Ticket is Natural range 0 .. Intel_GPU_Buffer_Backing.Slot'Last;
   -- Trusted dispatcher only; shares the allocation-ticket namespace with
   -- application Create, but never registers an application-visible handle.
   -- Reserve before starting private context/VM allocation. Finish only after
   -- the allocator is terminal, including failed starts. Tickets are never
   -- reused. Caller owns session validation and retention of private backing.
   procedure Reserve_Private (Object : in out Service; ID : out Ticket);
   procedure Finish_Private
     (Object : in out Service; ID : Ticket; Consumed : out Boolean);
   -- Serialized, nonblocking dispatch; at most one allocation is pending.
   -- Request [version, operation, bytes-or-handle, zero]. Create accepts only
   -- whole 4KiB pages. Response [status, version, handle-or-zero, bytes-or-zero].
   -- Close retires the name only; backing stays retained. No map, GPU binding,
   -- submission, cache-coherence promise, or reclamation is implied.
   procedure Handle
     (Object : in out Service; Sender, Stamp : Unsigned_64;
      Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Request : Words; Response : out Words;
      Deferred : out Ticket);
   -- Deferred=0: reply immediately. Otherwise retain the original kernel reply
   -- authority indexed by this ticket, ignore Response, and start allocation
   -- in backing slot Deferred. Never reconstruct reply authority from a PID.
   -- Complete exactly once, including failed submission/timeout (Ready=False).
   -- Backing must be owned, zeroed, retained application pages, not VM/context
   -- memory. Do not pass application-provided backing or physical addresses.
   procedure Complete
     (Object : in out Service; ID : Ticket;
      Backing : Intel_GPU_Buffer_Reply.Backing;
      Response : out Words; Consumed : out Boolean);
   -- Consumed=False means stale/unknown completion: do not send Response.
   -- Retire/Quarantine do not discard a pending reply: completion drains it
   -- with a denial. Every ticket/backing slot remains spent, even on failure.
   procedure Retire_Session (Object : in out Service; Session : Unsigned_64);
   -- Trusted dispatcher only: retire a newly created handle if its saved
   -- reply cannot be delivered. Ticket is the original internal ticket, not
   -- an application handle. Idempotent; backing remains retained.
   procedure Reject_Delivery (Object : in out Service; ID : Ticket);
   procedure Quarantine (Object : in out Service);
private
   type Issued_Result is record
      Session : Unsigned_64 := 0;
      Handle : Intel_GPU_Buffer_Handles.Handle := Intel_GPU_Buffer_Handles.No_Handle;
   end record;
   type Issued_Results is array (Intel_GPU_Buffer_Backing.Slot) of Issued_Result;
   type Service is limited record
      Attempted : Natural range 0 .. Intel_GPU_Buffer_Backing.Slot'Last := First_Slot - 1;
      Failed : Boolean := False;
      Handles : Intel_GPU_Buffer_Handles.Registry;
      Issued : Issued_Results;
      Pending : Ticket := 0;
      Private_Pending : Ticket := 0;
      Cancelled : Boolean := False;
      Pending_Session, Pending_Sender, Pending_Stamp, Pending_Bytes : Unsigned_64 := 0;
   end record;
end Intel_GPU_Buffer_Requests;
