with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Reply;
with System;
package Intel_GPU_Buffer_Handles with SPARK_Mode is
   -- Serialized, driver-internal registry for one device/endpoint lifetime.
   -- Session is a trusted, non-reused session identity, NOT a PID or a value
   -- read from request payloads. The IPC owner must authenticate it first.
   -- These 32-bit handles fit ANV's BO identifiers. They are not capabilities
   -- on their own: every lookup/close also checks the authenticated session.
   -- Names increase independently of record indices and never wrap/repeat.
   -- Namespace exhaustion rejects issuance even if storage remains available.
   subtype Session_ID is Unsigned_64;
   subtype Handle is Unsigned_32;
   No_Handle : constant Handle := 0;
   Initial_Capacity : constant := 16;
   type Registry is limited private;
   type Retained_Reference is limited private;
   -- Driver-internal lifetime pin, NOT an import capability or a writable view.
   -- Limited tokens cannot be copied by normal Ada assignment. Keep the registry
   -- root at a stable address and alive until every token has been returned.
   -- A name may close while its reference continues retaining the allocation.
   procedure Retain_Backing
     (Object : in out Registry; Session : Session_ID; ID : Handle;
      Reference : in out Retained_Reference; Accepted : out Boolean);
   -- A trusted coordinator may split an already retained lifetime into two
   -- independently retired users, including after the original name closes.
   -- This does NOT admit a new client or delegate rights: authenticate any
   -- importer and its allowed operations separately before calling this.
   -- No new BO name, address-space binding, CPU mapping or wire token results.
   -- Rejects an active destination (including source/destination aliasing),
   -- stale/foreign source and a quarantined registry without changing pins.
   procedure Retain_Referenced_Backing
     (Object : in out Registry; Source : Retained_Reference;
      Destination : in out Retained_Reference; Accepted : out Boolean;
      Exclude_Writes : Boolean := False);
   -- Exclusion is inherited on splits and cannot be downgraded. The trusted
   -- coordinator must first drain existing writers; this only gates NEW work.
   function Writes_Excluded (Object : Registry; Session : Session_ID; ID : Handle)
      return Boolean;
   function Session_Writes_Excluded (Object : Registry; Session : Session_ID)
      return Boolean;
   function Referenced_Backing
     (Object : Registry; Reference : Retained_Reference)
      return Intel_GPU_Buffer_Reply.Backing;
   -- Exact original name/session retained by this token, including after
   -- name closure. Not new client admission or permission to reopen a name.
   function Reference_Matches
     (Object : Registry; Reference : Retained_Reference;
      Session : Session_ID; ID : Handle) return Boolean;
   procedure Return_Reference
     (Object : in out Registry; Reference : in out Retained_Reference;
      References_Retired : Boolean; Accepted : out Boolean);
   -- Only trusted retirement evidence permits returning a pin. A timeout,
   -- closed owner name, or application assertion is not such evidence.
   -- Wrong-registry, stale and duplicate returns leave the token unchanged.
   function Count (Object : Registry) return Natural;
   function Can_Issue (Object : Registry) return Boolean;
   function Record_Capacity (Object : Registry) return Natural;
   -- Trusted CPU metadata backing, never app/GPU-provided addresses. The
   -- caller reserves stable storage and commits its prefix before extending.
   -- It must retain that writable, nonaliasing CPU mapping for the registry's
   -- entire lifetime, separate from BO backing and the Registry object itself.
   -- Base cannot change after first extension. At most64KiB added per call;
   -- new full records are initialized before capacity becomes observable.
   procedure Extend_Storage
     (Object : in out Registry; Base, Bytes : Unsigned_64; Accepted : out Boolean);
   function Is_Open (Object : Registry; Session : Session_ID; ID : Handle) return Boolean;
   type Close_Check is
     (Close_Ready, Registry_Quarantined, Session_Unavailable, Invalid_Handle,
      Unknown_Handle, Foreign_Session, Already_Closed, Backing_Unavailable);
   -- Trusted diagnostics only; does not expose identity details to the client
   -- or relax admission. Evaluate on the serialized service thread.
   function Check_Close (Object : Registry; Session : Session_ID; ID : Handle)
     return Close_Check;
   -- Checks stored entries, including all generations, not just initial IDs.
   function Session_Closed (Object : Registry; Session : Session_ID) return Boolean;
   function Resolve (Object : Registry; Session : Session_ID; ID : Handle)
     return Intel_GPU_Buffer_Reply.Backing
     with Post => Resolve'Result.Ready = Is_Open (Object, Session, ID);
   -- Trusted retirement inspection only: never expose a closed name through
   -- normal Resolve. Storage remains retained, and this is not a reuse token.
   function Closed_Backing (Object : Registry; Session : Session_ID; ID : Handle)
     return Intel_GPU_Buffer_Reply.Backing;
   procedure Register
     (Object : in out Registry; Session : Session_ID;
      Backing : Intel_GPU_Buffer_Reply.Backing; ID : out Handle)
     with Post => Count (Object) = Count (Object)'Old + (if ID /= No_Handle then 1 else 0)
       and (if ID /= No_Handle then Is_Open (Object, Session, ID));
   -- Backing must be owned, zeroed and retained by the trusted buffer pool.
   -- Rejects malformed, overlapping and different-arena records. Never
   -- register page-table/context backing as an application-visible buffer.
   procedure Close
     (Object : in out Registry; Session : Session_ID; ID : Handle;
      Accepted : out Boolean)
     with Post => Count (Object) = Count (Object)'Old and
       (if Accepted then not Is_Open (Object, Session, ID));
   procedure Close_Session (Object : in out Registry; Session : Session_ID)
     with Post => Count (Object) = Count (Object)'Old and
       Session_Closed (Object, Session);
   procedure Quarantine (Object : in out Registry);
   -- Serialized preflight BEFORE asking the supervisor to recycle a slice.
   -- Only observes internal pins; caller still establishes GPU/TLB/grant
   -- retirement. Does not reserve a transition or grant reuse authority.
   function Can_Release_Backing
     (Object : Registry; Session : Session_ID; ID : Handle) return Boolean;
   -- Exact closed identity, trusted acknowledgement only. Stops reserving
   -- the old range but keeps its identity tombstone for future replacement.
   procedure Release_Retired_Backing
     (Object : in out Registry; Session : Session_ID; ID : Handle;
      References_Retired : Boolean; Accepted : out Boolean)
     with Post => Count (Object) = Count (Object)'Old and then
       (if Accepted then not Is_Open (Object, Session, ID));
   -- Trusted, serialized replacement. References_Retired
   -- is supplied by the retirement coordinator, NEVER an application bit.
   -- It must cover GPU reachability/TLBs, CPU grants, pending publications and
   -- supervisor slice retirement. Backing is freshly owned/zeroed storage.
   -- Metadata transition only: does not free, zero or allocate anything.
   -- A different new session additionally requires an acknowledged released
   -- reservation. Previous_Session authenticates the exact old tombstone;
   -- Session is the independently authenticated new allocation owner.
   procedure Replace_Retired
     (Object : in out Registry; Previous_Session, Session : Session_ID; Previous : Handle;
      Backing : Intel_GPU_Buffer_Reply.Backing; References_Retired : Boolean;
      ID : out Handle)
     with Post => Count (Object) = Count (Object)'Old and then
       (if ID /= No_Handle then ID > Previous and then
          Is_Open (Object, Session, ID) and then not Is_Open (Object, Previous_Session, Previous)
          and then (if Session /= Previous_Session then not Is_Open (Object, Previous_Session, ID)));
   -- Close only retires the name; it does NOT free/unmap/reuse storage or
   -- cancel GPU work. IDs never repeat; retained ranges are reusable only via
   -- the trusted Replace_Retired boundary. Growable metadata alone is not
   -- production BO reclamation or session admission. Lookup/range validation
   -- still scans live records and needs indexed/budgeted work before large use.
private
   subtype Slot is Positive;
   type Item is record
      ID : Handle := No_Handle;
      Session : Session_ID := 0;
      Open : Boolean := False;
      Released : Boolean := False;
      Backing : Intel_GPU_Buffer_Reply.Backing;
      Retained : Natural := 0;
      Write_Holds : Natural := 0;
   end record;
   type Retained_Reference is limited record
      Active : Boolean := False;
      Excludes_Writes : Boolean := False;
      Origin : System.Address := System.Null_Address;
      -- Internal stable slot, never a wire handle. Root and exact identity
      -- are still checked; growth never moves or renumbers existing records.
      Index : Natural := 0;
      Session : Session_ID := 0;
      ID : Handle := No_Handle;
   end record;
   type Items is array (Positive range 1 .. Initial_Capacity) of Item;
   type Registry is limited record
      Used : Natural := 0;
      Available : Natural := Initial_Capacity;
      Storage_Base, Storage_Bytes : Unsigned_64 := 0;
      Last_Issued : Handle := No_Handle;
      -- Exact sum of per-record write holds. Zero avoids a whole-registry
      -- scan on ordinary submissions; nonzero still requires session lookup.
      Total_Write_Holds : Unsigned_64 := 0;
      Failed : Boolean := False;
      Entries : Items;
   end record;
end Intel_GPU_Buffer_Handles;
