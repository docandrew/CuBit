with Interfaces; use Interfaces;
with System;
with Intel_GPU_Record_Store;
with Intel_GPU_Physical_Extents;
package Intel_GPU_Extent_Directory is
   -- Trusted, serialized arena owner. Never reconstruct one from wire data.
   -- Owner and committed metadata stay at stable addresses for every View's
   -- lifetime. No reset/free: quarantine invalidates views but retains storage.
   -- This core describes DMA geometry, not allocation or DMA authority.
   type Directory is limited private;
   type View is private;
   type Borrowed_View is private;
   -- Internal stable-owner boundary: never serialize this value. The caller
   -- MUST retain this aliased owner and its metadata until every borrowed
   -- descriptor is gone. This is not an owning reference or a client grant.
   -- Intended for service-incarnation pools; no stack owner may escape scope.
   function Borrow (Object : aliased Directory) return Borrowed_View;
   function Valid (Object : Borrowed_View) return Boolean;
   function Byte_Count (Object : Borrowed_View) return Unsigned_64;
   function Same_Owner (Left, Right : Borrowed_View) return Boolean;
   function Resolve
     (Object : Borrowed_View; Offset, Bytes : Unsigned_64)
      return Intel_GPU_Physical_Extents.Span;
   procedure Initialize
     (Object : in out Directory; Byte_Quota, DMA_Limit : Unsigned_64;
      Accepted : out Boolean);
   -- DMA_Limit is exclusive and comes from trusted device policy. Quota is
   -- independent from committed bytes; neither call allocates pixel memory.
   function Ready (Object : Directory) return Boolean;
   function Committed_Bytes (Object : Directory) return Unsigned_64;
   function Metadata_Capacity (Object : Directory) return Positive;
   Max_Admission_Probes : constant Positive :=
     64 - (12 + Intel_GPU_Physical_Extents.Allocation_Order) + 1;
   function Last_Admission_Probes (Object : Directory) return Natural;
   -- Number of key nodes read by the last Append, not an allocation latency.
   type Lookup_State is (Unavailable, Absent, Present);
   type DMA_Location is record
      State : Lookup_State := Unavailable;
      Offset : Unsigned_64 := 0;
      Probes : Natural range 0 .. Max_Admission_Probes := 0;
   end record;
   -- Reverse lookup of one DMA byte in the captured prefix, at most44 key
   -- reads. Offset is arena-relative, not an authority or CPU pointer. An
   -- invalid owner/index fails closed as Unavailable, never Absent.
   function Locate_DMA (Object : Borrowed_View; Address : Unsigned_64)
     return DMA_Location;
   procedure Extend_Metadata
     (Object : in out Directory; Base, Bytes : Unsigned_64;
      Accepted : out Boolean);
   -- Owned RW metadata only, retained for this directory's lifetime; existing
   -- entries never move. At most64KiB additional metadata per call. Caller
   -- separately budgets metadata and prevents aliasing with device buffers.
   procedure Append
     (Object : in out Directory; DMA : Unsigned_64; Accepted : out Boolean);
   -- One fixed2MiB extent per append. A digital-search index checks duplicates
   -- in at most Max_Admission_Probes key reads, independent of entry count.
   -- No balancing, rehash or relocation of published insertion-order entries.
   -- Rejection leaves all earlier entries and views unchanged.
   function Snapshot (Object : Directory) return View;
   function Valid (Object : Directory; Snapshot : View) return Boolean;
   function Byte_Count (Object : Directory; Snapshot : View) return Unsigned_64;
   function Resolve
     (Object : Directory; Snapshot : View; Offset, Bytes : Unsigned_64)
      return Intel_GPU_Physical_Extents.Span;
   -- Object is explicitly supplied, never dereferenced from a caller pointer.
   -- A view covers only its published prefix, even when Object grows later.
   procedure Quarantine (Object : in out Directory);
private
   type Extent_Entry is record
      DMA : Unsigned_64;
      Left, Right : Unsigned_32;
   end record;
   for Extent_Entry use record
      DMA at 0 range 0 .. 63;
      Left at 8 range 0 .. 31;
      Right at 12 range 0 .. 31;
   end record;
   for Extent_Entry'Size use 128;
   package Entries is new Intel_GPU_Record_Store
     (Extent_Entry, (DMA => 0, Left => 0, Right => 0));
   type Directory is limited record
      Started, Failed : Boolean := False;
      Quota, Limit : Unsigned_64 := 0;
      Count : Natural := 0;
      Root : Unsigned_32 := 0;
      Probes : Natural range 0 .. Max_Admission_Probes := 0;
      Items : Entries.Store;
   end record;
   type View is record
      Root : System.Address := System.Null_Address;
      Count : Natural := 0;
   end record;
   type Owner_Access is access constant Directory;
   type Borrowed_View is record
      Owner : Owner_Access := null;
      Count : Natural := 0;
   end record;
end Intel_GPU_Extent_Directory;
