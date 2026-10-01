with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Reply;
package Intel_GPU_Buffer_Handles with SPARK_Mode is
   -- Serialized, driver-internal registry for one device/endpoint lifetime.
   -- Session is a trusted, non-reused session identity, NOT a PID or a value
   -- read from request payloads. The IPC owner must authenticate it first.
   -- These 32-bit handles fit ANV's BO identifiers. They are not capabilities
   -- on their own: every lookup/close also checks the authenticated session.
   subtype Session_ID is Unsigned_64;
   subtype Handle is Unsigned_32;
   No_Handle : constant Handle := 0;
   Capacity : constant := 16;
   type Registry is limited private;
   function Count (Object : Registry) return Natural;
   function Is_Open (Object : Registry; Session : Session_ID; ID : Handle) return Boolean;
   function Resolve (Object : Registry; Session : Session_ID; ID : Handle)
     return Intel_GPU_Buffer_Reply.Backing
     with Post => Resolve'Result.Ready = Is_Open (Object, Session, ID);
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
       (for all ID in Handle range 1 .. Handle (Count (Object)) =>
          not Is_Open (Object, Session, ID));
   procedure Quarantine (Object : in out Registry);
   -- Close only retires the name; it does NOT free/unmap/reuse storage or
   -- cancel GPU work. IDs and retained ranges are never reused. This bounded
   -- bring-up registry is not production BO reclamation or session admission.
private
   subtype Slot is Positive range 1 .. Capacity;
   type Item is record
      Session : Session_ID := 0;
      Open : Boolean := False;
      Backing : Intel_GPU_Buffer_Reply.Backing;
   end record;
   type Items is array (Slot) of Item;
   type Registry is limited record
      Used : Natural range 0 .. Capacity := 0;
      Failed : Boolean := False;
      Entries : Items;
   end record;
end Intel_GPU_Buffer_Handles;
