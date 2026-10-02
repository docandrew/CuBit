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
   -- Retained diagnostic only; never used to authorize allocation or reuse.
   type Allocation_Outcome is
     (Not_Create, Quarantined, Application_Pending, Private_Pending,
      Owner_Unavailable, Slots_Exhausted, Awaiting_Backing, Backing_Unavailable,
      Backing_Size_Mismatch, Handle_Unavailable, Allocation_Ready);
   function Last_Allocation (Object : Service) return Allocation_Outcome;
   -- Immutable identity layout, independent of allocated registry capacity.
   -- Low32 is the one-based slot; high32 is generation minus one. Zero is
   -- never issued. Reserve the final high-word value to prevent wraparound.
   Ticket_Stride : constant Unsigned_64 := Intel_GPU_Buffer_Backing.Ticket_Stride;
   subtype Ticket is Unsigned_64 range 0 .. Intel_GPU_Buffer_Backing.Ticket_Limit;
   -- Extracting a slot is not validation. Completion/delivery paths compare
   -- the full issued ticket before touching the current slot occupant.
   function Ticket_Slot (ID : Ticket) return Intel_GPU_Buffer_Backing.Slot is
     (if ID mod Ticket_Stride in
          Unsigned_64 (Intel_GPU_Buffer_Backing.Slot'First) ..
          Unsigned_64 (Intel_GPU_Buffer_Backing.Slot'Last)
      then Intel_GPU_Buffer_Backing.Slot (ID mod Ticket_Stride)
      else Intel_GPU_Buffer_Backing.Slot'First);
   -- Like slot extraction, generation decoding does not authenticate a ticket.
   function Ticket_Generation (ID : Ticket) return Unsigned_32 is
     (if ID = 0 then 0 else Unsigned_32 (ID / Ticket_Stride + 1));
   -- Trusted dispatcher only; shares the allocation-ticket namespace with
   -- application Create, but never registers an application-visible handle.
   -- Reserve before starting private context/VM allocation. Finish only after
   -- the allocator is terminal, including failed starts. Default tickets stay
   -- pinned. Reclaimable=True is for replacement tables only, never original
   -- context/root/scratch storage. Caller owns lifetime/backing validation.
   -- An acknowledged reusable replacement-table slot may cross sessions;
   -- its fresh ticket is rebound to Session before allocation starts. No old
   -- ticket, owner or acknowledgement can affect the replacement generation.
   -- Session is supplied by the trusted dispatcher. Zero explicitly denotes
   -- device-lifetime bootstrap storage, never reclaimable by closing a session.
   procedure Reserve_Private
     (Object : in out Service; Session : Unsigned_64; ID : out Ticket;
      Reclaimable : Boolean := False);
   -- Trusted coordinator only after exact supervisor retirement ack AND
   -- hardware/TLB retirement. Also retire/reset the offline VM image before
   -- reserving again. Never accepts bootstrap, pinned or application tickets.
   procedure Acknowledge_Private_Retirement
     (Object : in out Service; Session : Unsigned_64; ID : Ticket;
      References_Retired : Boolean; Accepted : out Boolean);
   -- Retained allocation ownership, including failed/deferred allocations.
   -- Observation only, not proof that GPU mappings or CPU grants are retired.
   function Ticket_Session (Object : Service; ID : Ticket) return Unsigned_64;
   function Pending_For (Object : Service; Session : Unsigned_64) return Boolean;
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
   -- in Ticket_Slot(Deferred). Never reconstruct reply authority from a PID.
   -- Complete exactly once, including failed submission/timeout (Ready=False).
   -- Backing must be owned, zeroed, retained application pages, not VM/context
   -- memory. Do not pass application-provided backing or physical addresses.
   procedure Complete
     (Object : in out Service; ID : Ticket;
      Backing : Intel_GPU_Buffer_Reply.Backing;
      Response : out Words; Consumed : out Boolean);
   -- Consumed=False means stale/unknown completion: do not send Response.
   -- Retire/Quarantine do not discard a pending reply: completion drains it
   -- with a denial. Failed/pinned tickets remain spent. Closed application
   -- tickets require Acknowledge_Retirement before replacement, including by
   -- another authenticated session. Closing a session does not revoke an
   -- already confirmed backing retirement or reopen an old handle.
   procedure Retire_Session (Object : in out Service; Session : Unsigned_64);
   -- Trusted dispatcher only: retire a newly created handle if its saved
   -- reply cannot be delivered. Ticket is the original internal ticket, not
   -- an application handle. Idempotent; backing remains retained.
   procedure Reject_Delivery (Object : in out Service; ID : Ticket);
   -- Trusted coordinator only, AFTER exact supervisor retirement ack and
   -- GPU/TLB/CPU-grant retirement. Not a public request or a Close shortcut.
   -- Permits application slot reuse under fresh ticket/handle identities;
   -- the next session is independently authenticated. Private allocations and
   -- incomplete/failed deliveries do not enter this path.
   procedure Acknowledge_Retirement
     (Object : in out Service; Session : Unsigned_64; ID : Ticket;
      References_Retired : Boolean; Accepted : out Boolean);
   type Closed_Allocation (Ready : Boolean := False) is record
      case Ready is
         when False => null;
         when True =>
            ID : Ticket;
            Session : Unsigned_64;
            Handle : Intel_GPU_Buffer_Handles.Handle;
            Generation : Unsigned_32;
      end case;
   end record;
   -- Trusted enumeration of retained closed application allocations. No
   -- caller identity/address is accepted; this is NOT retirement evidence.
   -- The coordinator must still validate session lifetime, GPU/CPU references,
   -- and serialize observation through the supervisor acknowledgement.
   function Closed_At (Object : Service; Index : Intel_GPU_Buffer_Backing.Slot)
     return Closed_Allocation;
   procedure Quarantine (Object : in out Service);
private
   type Issued_Result is record
      Session : Unsigned_64 := 0;
      Handle : Intel_GPU_Buffer_Handles.Handle := Intel_GPU_Buffer_Handles.No_Handle;
   end record;
   type Issued_Results is array (Intel_GPU_Buffer_Backing.Slot) of Issued_Result;
   type Ticket_Owners is array (Intel_GPU_Buffer_Backing.Slot) of Unsigned_64;
   type Ticket_Identities is array (Intel_GPU_Buffer_Backing.Slot) of Ticket;
   type Reusable_Slots is array (Intel_GPU_Buffer_Backing.Slot) of Boolean;
   type Service is limited record
      Attempted : Natural range 0 .. Intel_GPU_Buffer_Backing.Slot'Last := First_Slot - 1;
      Failed : Boolean := False;
      Outcome : Allocation_Outcome := Not_Create;
      Handles : Intel_GPU_Buffer_Handles.Registry;
      Issued : Issued_Results;
      Owners : Ticket_Owners := [others => 0];
      Identities : Ticket_Identities := [others => 0];
      Reusable : Reusable_Slots := [others => False];
      Private_Reclaimable, Private_Reusable : Reusable_Slots := [others => False];
      Pending_Previous : Intel_GPU_Buffer_Handles.Handle := 0;
      Pending_Previous_Session : Unsigned_64 := 0;
      Pending : Ticket := 0;
      Private_Pending : Ticket := 0;
      Cancelled : Boolean := False;
      Pending_Session, Pending_Sender, Pending_Stamp, Pending_Bytes : Unsigned_64 := 0;
   end record;
end Intel_GPU_Buffer_Requests;
