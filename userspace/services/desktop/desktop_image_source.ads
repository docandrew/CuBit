with Compositor_Formats;
with Compositor_Source_Content;
with Desktop_Vulkan_Startup;
with Vulkan_Owned_Targets;
with Vulkan_Submission;
with Interfaces;
with System;
-- One persistent GPU image for one client surface, in one reserved client
-- slot. New content versions are copied into the SAME allocation: only the
-- changed row band when the size is unchanged, every row after a resize.
-- The allocation is replaced only when the extent changes. The CPU mapping
-- is read only while a pass copies rows into staging; the GPU never reads
-- it. The descriptor is retired before a pass writes the image and is
-- republished only after the whole pass completes, so no scene can sample
-- a half-updated image and no write targets an image a pending frame reads.
-- No mapping authority is created.
package Desktop_Image_Source with SPARK_Mode is
   package D renames Desktop_Vulkan_Startup;
   package V renames Vulkan_Submission;
   package A renames Vulkan_Owned_Targets.A;
   package C renames Compositor_Source_Content;
   use type C.Content_Version;
   type Phase is (Fresh, Ready_To_Upload, Uploading, Resident, Quarantined, Closed);
   -- Unaffordable: valid request, but the device refused the allocation
   -- (budget or renderer state). The caller may evict and retry.
   type Outcome is (Available, Pending, Deferred, Rejected, Unaffordable, Unsafe);
   type State is limited private with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean;
   function Current (S : State) return Phase;
   -- Version held (Resident) or being copied (Ready_To_Upload/Uploading).
   function Version (S : State) return C.Content_Version;
   -- A pass still copies rows from this CPU mapping.
   function Reads (S : State; Pixels : System.Address) return Boolean;
   -- Allocations retained only for this slot's key; no foreign authority.
   function Holds_Backing (S : State) return Boolean;
   -- No pass in flight and no scene reader: Close may free it now.
   function Evictable (S : State) return Boolean with Global => (Input => D.Engine);
   -- Only after successful Close; no live identity or allocation survives.
   procedure Rearm (S : in out State; Accepted : out Boolean)
     with Global => null, Pre => Valid (S), Post => Valid (S);
   -- Image is the CPU mapping holding Version. Stale is every row changed
   -- since the held version; Started reports that this call began a new
   -- pass covering it (the caller then forgets Stale). An empty Stale with a
   -- new version, or a new extent, copies every row.
   procedure Acquire (S : in out State; Index : V.Client_Slot;
      Version : C.Content_Version; Image : Compositor_Formats.Image;
      Bytes : Natural; Stale : C.Row_Band; Started : out Boolean;
      Source : out V.Source_Ticket; Result : out Outcome)
     with Global => (In_Out => D.Engine),
       Pre => Valid (S) and D.Valid, Post => Valid (S) and D.Valid and
         (if Result = Available then
            Current (S) = Resident and Desktop_Image_Source.Version (S) = Version);
   -- At most one fence observation and one bounded staging chunk per call.
   procedure Poll (S : in out State; Result : out Outcome)
     with Global => (In_Out => D.Engine),
       Pre => Valid (S) and D.Valid, Post => Valid (S) and D.Valid;
   -- Forget a CPU mapping. Safe unless a pass still copies from it, or the
   -- slot is quarantined while it was the mapping in use. No GPU work.
   procedure Detach (S : in out State; Pixels : System.Address; Safe : out Boolean)
     with Global => null, Pre => Valid (S), Post => Valid (S);
   -- Capture_Retired attests all caller scene references are gone. Unknown
   -- GPU state retains the backing.
   procedure Close (S : in out State; Capture_Retired : Boolean; Safe : out Boolean)
     with Global => (In_Out => D.Engine),
       Pre => Valid (S) and D.Valid, Post => Valid (S) and D.Valid;
private
   use type Compositor_Formats.Word, A.Ticket, V.Source_Ticket;
   type State is limited record
      Status : Phase := Fresh;
      Index : V.Client_Slot := V.Client_Slot'First;
      Held : C.Content_Version := C.No_Version;
      Width, Height : Compositor_Formats.Word := 0;
      Image : Compositor_Formats.Image;
      Bytes : Natural := 0;
      Lease : A.Ticket := A.No_Ticket;
      Source : V.Source_Ticket := V.No_Source;
   end record;
   function Valid (S : State) return Boolean is
     ((if S.Status in Ready_To_Upload | Uploading then
         Compositor_Formats.Valid (S.Image, Interfaces.Unsigned_64 (S.Bytes)) and
         S.Image.Width = S.Width and S.Image.Height = S.Height) and
      (if S.Status in Ready_To_Upload | Uploading | Resident then S.Lease /= A.No_Ticket) and
      (if S.Status in Fresh | Closed then S.Lease = A.No_Ticket and S.Source = V.No_Source));
   function Current (S : State) return Phase is (S.Status);
   function Version (S : State) return C.Content_Version is (S.Held);
   function Holds_Backing (S : State) return Boolean is (S.Lease /= A.No_Ticket);
   use type System.Address;
   function Reads (S : State; Pixels : System.Address) return Boolean is
     (S.Status in Ready_To_Upload | Uploading and then S.Image.Pixels = Pixels);
end Desktop_Image_Source;
