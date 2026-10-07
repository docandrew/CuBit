with Compositor_Formats;
with Desktop_Vulkan_Startup;
with Vulkan_Owned_Targets;
with Vulkan_Submission;
with Interfaces;
-- One immutable CPU acquisition, not a pointer-keyed cache. Caller supplies
-- a fresh nonzero generation and holds the mapping unchanged until Close.
-- Caller reserves a client slot exclusively. No mapping authority is created.
package Desktop_Image_Source is
   package D renames Desktop_Vulkan_Startup;
   package V renames Vulkan_Submission;
   package A renames Vulkan_Owned_Targets.A;
   type Phase is (Fresh, Ready_To_Upload, Uploading, Resident, Quarantined, Closed);
   type Outcome is (Available, Pending, Deferred, Rejected, Unsafe);
   type State is limited private;
   function Current (S : State) return Phase;
   -- Only after successful Close; no live identity or allocation survives.
   procedure Rearm (S : in out State; Accepted : out Boolean);
   procedure Acquire (S : in out State; Index : V.Client_Slot;
      Generation : Interfaces.Unsigned_64; Image : Compositor_Formats.Image;
      Bytes : Natural; Source : out V.Source_Ticket; Result : out Outcome);
   -- At most one fence observation and one bounded staging chunk per call.
   procedure Poll (S : in out State; Result : out Outcome);
   -- Capture_Retired attests all caller scene references are gone. Unknown
   -- GPU state retains backing AND the caller's CPU acquisition.
   procedure Close (S : in out State; Capture_Retired : Boolean; Safe : out Boolean);
private
   type State is limited record
      Status : Phase := Fresh;
      Index : V.Client_Slot := V.Client_Slot'First;
      Generation : Interfaces.Unsigned_64 := 0;
      Image : Compositor_Formats.Image;
      Bytes : Natural := 0;
      Lease : A.Ticket := A.No_Ticket;
      Source : V.Source_Ticket := V.No_Source;
   end record;
   function Current (S : State) return Phase is (S.Status);
end Desktop_Image_Source;
