with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Reply;
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
   Capacity : constant := 16;
   type Registry is limited private;
   function Count (Object : Registry) return Natural;
   function Can_Issue (Object : Registry) return Boolean;
   function Is_Open (Object : Registry; Session : Session_ID; ID : Handle) return Boolean;
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
   -- the trusted Replace_Retired boundary. This bounded
   -- bring-up registry is not production BO reclamation or session admission.
private
   subtype Slot is Positive range 1 .. Capacity;
   type Item is record
      ID : Handle := No_Handle;
      Session : Session_ID := 0;
      Open : Boolean := False;
      Released : Boolean := False;
      Backing : Intel_GPU_Buffer_Reply.Backing;
   end record;
   type Items is array (Slot) of Item;
   type Registry is limited record
      Used : Natural range 0 .. Capacity := 0;
      Last_Issued : Handle := No_Handle;
      Failed : Boolean := False;
      Entries : Items;
   end record;
end Intel_GPU_Buffer_Handles;
