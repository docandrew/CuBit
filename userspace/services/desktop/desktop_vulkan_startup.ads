with Vulkan_Glyph_Sources;
with Compositor_Upload;
with Compositor_Upload_Progress;
with Vulkan_Upload_Owner;
with Vulkan_Submission;
with Vulkan_Scene;
with Compositor_Damage;
with System;
with Vulkan_Owned_Targets;
with Vulkan_Frame;
with Interfaces;
with Vulkan_Device_Owner;
-- Opt-in Desktop GPU owner. Default Desktop does not depend on this package.
-- Owns exactly one device/context/submission lifetime; never exports copies.
package Desktop_Vulkan_Startup with SPARK_Mode,
  Abstract_State => Engine, Initializes => Engine,
  Initial_Condition => Valid is
   use type Vulkan_Scene.Phase, Vulkan_Device_Owner.Phase, Vulkan_Owned_Targets.Phase, Vulkan_Submission.Source_Ticket, System.Address;
   pragma Unevaluated_Use_Of_Old (Allow);
   function Valid return Boolean with Ghost, Global => (Input => Engine);
   function Current return Vulkan_Device_Owner.Phase with Global => (Input => Engine);
   -- Nonzero must be the manifest-bound endpoint already admitted by procmgr.
   -- Zero selects software without Mesa IPC. Repeated calls do not retry.
   procedure Initialize (Admitted_Slot : Interfaces.Unsigned_64)
     with Global => (In_Out => Engine),
       Pre => Valid, Post => Valid and Current /= Vulkan_Device_Owner.Fresh;
   procedure Check_Health (Usable : out Boolean)
     with Global => (In_Out => Engine),
       Pre => Valid, Post => Valid and (Usable = (Current = Vulkan_Device_Owner.Ready));
   -- First output target set; private metadata must match this admitted
   -- device/context and outlive Engine. No export/import or display authority.
   -- One attempt, three images, actual allocation bytes charged before binding.
   -- Multi-output admission and resize replacement remain separate integration.
   function Can_Prepare_Targets return Boolean with Global => (Input => Engine);
   procedure Prepare_Targets
     (Requests : Vulkan_Frame.Targets; Description : System.Address;
      Epoch : Vulkan_Owned_Targets.P.Live_ID; Byte_Limit : Natural;
      Allowed_Types : Interfaces.Unsigned_32; Ready : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid,
       Post => Valid and
         (if Can_Prepare_Targets'Old then
            Configured_Limit = Byte_Limit and Charged_Bytes <= Byte_Limit
          else Configured_Limit = Configured_Limit'Old);
   -- Construct private requests from the admitted device, then use the same
   -- bounded target owner. Rejected metadata consumes the one target attempt.
   procedure Configure_Targets
     (Width, Height : Interfaces.Unsigned_32;
      Epoch : Vulkan_Owned_Targets.P.Live_ID; Byte_Limit : Natural;
      Ready : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid,
       Post => Valid and
         (if Can_Prepare_Targets'Old then
            Configured_Limit = Byte_Limit and Charged_Bytes <= Byte_Limit
          else Configured_Limit = Configured_Limit'Old);
   function Target_Phase return Vulkan_Owned_Targets.Phase
     with Global => (Input => Engine);
   function Charged_Bytes return Natural with Global => (Input => Engine);
   function Configured_Limit return Natural with Global => (Input => Engine);
   -- Own the existing textured drawing pipeline and fixed source provider as
   -- a context child. No pixel upload/import is performed by this operation.
   procedure Prepare_Pipeline (Ready : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid;
   function Pipeline_Ready return Boolean with Global => (Input => Engine);
   -- Reserve the upload allocation from the same aggregate budget as targets
   -- and sources. Call before allocating sources to preserve staging headroom.
   -- This does not expose a CPU mapping or grant permission to write pixels.
   procedure Configure_Upload (Size : Vulkan_Upload_Owner.Capacity_Range;
      Ready : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid,
       Post => Valid and Configured_Limit = Configured_Limit'Old and
         (if Ready then Upload_Capacity = Size);
   function Upload_Capacity return Natural with Global => (Input => Engine);
   function Upload_Phase return Vulkan_Upload_Owner.Phase with Global => (Input => Engine);
   procedure Release_Upload (Released : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid,
       Post => Valid and Configured_Limit = Configured_Limit'Old and
         (if Released then Upload_Capacity = 0);
   type Source_Result is (Source_Accepted, Source_Busy, Source_Rejected, Source_Unsafe);
   subtype Backing_Slot is Vulkan_Submission.Source_Slot;
   use type Vulkan_Owned_Targets.A.Ticket, Vulkan_Owned_Targets.I.Phase;
   function Backing_Phase (Index : Backing_Slot) return Vulkan_Owned_Targets.I.Phase
     with Global => (Input => Engine);
   function Backing_Lease (Index : Backing_Slot) return Vulkan_Owned_Targets.A.Ticket
     with Global => (Input => Engine);
   -- Allocate private BGRA color or R8 mask backing on the admitted device.
   -- Targets must already have established the aggregate budget. No upload,
   -- shader-readable layout or descriptor import is implied by acceptance.
   procedure Allocate_Backing (Index : Backing_Slot;
      Width, Height : Interfaces.Unsigned_32; Mask : Boolean;
      Lease : out Vulkan_Owned_Targets.A.Ticket; Result : out Source_Result)
     with Global => (In_Out => Engine), Pre => Valid,
       Post => Valid and Configured_Limit = Configured_Limit'Old and
         (if Lease /= Vulkan_Owned_Targets.A.No_Ticket then
            Lease = Backing_Lease (Index) and Backing_Phase (Index) = Vulkan_Owned_Targets.I.Live);
   -- Matching generation, idle submission, healthy device and no descriptor in
   -- this slot are required, with no active producer or upload. This also works
   -- during shutdown after confirmed completion or producer cancellation.
   procedure Release_Backing (Index : Backing_Slot;
      Lease : Vulkan_Owned_Targets.A.Ticket; Released : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid,
       Post => Valid and Configured_Limit = Configured_Limit'Old and
         (if Released then Backing_Phase (Index) = Vulkan_Owned_Targets.I.Closed);

   package Upload_Progress is new Compositor_Upload_Progress;
   type Write_Ticket is private;
   No_Write : constant Write_Ticket;
   function Write_Active (Ticket : Write_Ticket) return Boolean with Global => (Input => Engine);
   -- Private same-process producer only. The returned mapping covers Plan's
   -- row span (including requested padding) and is writable only until
   -- Submit_Write or Cancel_Write. Row_Pixels=0 selects tightly packed rows;
   -- nonzero stride must cover the image width and fit staging.
   -- Never retain/use it after those calls. Producer_Retired is an audited
   -- assertion that every producer access has ended, not a request to wait.
   procedure Begin_Write (Index : Backing_Slot; Lease : Vulkan_Owned_Targets.A.Ticket;
      Ticket : out Write_Ticket; Plan : out Compositor_Upload.Plan;
      Mapping : out System.Address; Result : out Source_Result;
      Row_Pixels : Compositor_Upload.Edge := 0)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid and
       (if Mapping /= System.Null_Address then Write_Active (Ticket) and Compositor_Upload.Valid (Plan));
   procedure Submit_Write (Ticket : Write_Ticket; Producer_Retired : Boolean;
      Result : out Source_Result)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid and not Write_Active (Ticket);
   procedure Cancel_Write (Ticket : Write_Ticket; Producer_Retired : Boolean;
      Cancelled : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid and not Write_Active (Ticket);
   -- New content for the same live allocation: every row (Restart_Content)
   -- or only rows First .. Last - 1, keeping the rest (Update_Content). The
   -- descriptor must already be released; Import_Backing republishes it only
   -- after the whole pass completes. Neither allocates nor frees memory.
   procedure Restart_Content (Index : Backing_Slot; Lease : Vulkan_Owned_Targets.A.Ticket;
      Accepted : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid;
   procedure Update_Content (Index : Backing_Slot; Lease : Vulkan_Owned_Targets.A.Ticket;
      First, Last : Compositor_Upload.Edge; Accepted : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid;
   -- Monotonic count of accepted transfer submissions (progress evidence for
   -- callers that must distinguish cold uploads from a stalled scene).
   function Transfers_Submitted return Interfaces.Unsigned_64 with Global => (Input => Engine);
   -- Monotonic counts of backing allocations accepted and backings freed on
   -- this device, by slot class: the steady-state churn evidence (each one
   -- is a GPU VM update in the driver).
   function Backings_Allocated return Interfaces.Unsigned_64 with Global => (Input => Engine);
   function Allocated_In (Class : Vulkan_Submission.Source_Class) return Interfaces.Unsigned_64
     with Global => (Input => Engine);
   function Released_In (Class : Vulkan_Submission.Source_Class) return Interfaces.Unsigned_64
     with Global => (Input => Engine);
   procedure Import_Backing (Index : Backing_Slot; Lease : Vulkan_Owned_Targets.A.Ticket;
      Ticket : out Vulkan_Submission.Source_Ticket; Result : out Source_Result)
     with Global => (In_Out => Engine), Pre => Valid,
       Post => Valid and (if Ticket /= Vulkan_Submission.No_Source then Source_Held (Ticket));
   -- Associate an uploaded R8 backing with producer-supplied face/code/density.
   -- The producer attests the raster contents; policy checks extent and lifetime.
   -- The first 128 backing slots can hold glyphs; remaining slots stay available
   -- for other images. Binding requires an idle writer/submission and a matching
   -- live descriptor. Retired descriptors make old associations unresolvable.
   function Glyph_Source (Key : Vulkan_Glyph_Sources.Key) return Vulkan_Submission.Source_Ticket
     with Global => (Input => Engine);
   procedure Bind_Glyph (Index : Backing_Slot; Lease : Vulkan_Owned_Targets.A.Ticket;
      Key : Vulkan_Glyph_Sources.Key; Source : Vulkan_Submission.Source_Ticket;
      Accepted : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid,
       Post => Valid and (if Accepted then Glyph_Source (Key) = Source);
   -- Capture typed glyph geometry through the private submission's association
   -- table. Missing/stale/wrong-density keys reject the whole scene snapshot.
   procedure Capture_Glyph (Scene : in out Vulkan_Scene.State;
      Key : Vulkan_Glyph_Sources.Key; Cell : Vulkan_Scene.A.G.Logical_Rectangle;
      Tint : Vulkan_Scene.A.Word; Accepted : out Boolean)
     with Global => (Input => Engine), Pre => Valid,
       Post => (if Accepted then Vulkan_Scene.Current (Scene) = Vulkan_Scene.Collecting
         else Vulkan_Scene.Current (Scene) = Vulkan_Scene.Rejected);
   -- Positive confirmation of a healthy, idle renderer. Not-pending alone is
   -- insufficient: quarantined work may still reference every retained source.
   function Can_Retire_Readers return Boolean with Global => (Input => Engine);
   function Source_Held (Ticket : Vulkan_Submission.Source_Ticket) return Boolean
     with Global => (Input => Engine);
   -- Bounded CPU scene readers protect source registrations before submission.
   -- A reader is retired only after its CPU references are dropped and the
   -- renderer positively confirms quiescence. Tokens never wrap or alias.
   type Source_Reader is private;
   No_Source_Reader : constant Source_Reader;
   function Reader_Held (Reader : Source_Reader) return Boolean
     with Global => (Input => Engine);
   function Source_Pinned (Ticket : Vulkan_Submission.Source_Ticket) return Boolean
     with Global => (Input => Engine);
   procedure Pin_Source (Ticket : Vulkan_Submission.Source_Ticket; Reader : out Source_Reader)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid and
       (if Reader /= No_Source_Reader then Reader_Held (Reader) and Source_Pinned (Ticket));
   procedure Unpin_Source (Reader : Source_Reader; CPU_Retired : Boolean; Accepted : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid and
       (if Accepted then not Reader_Held (Reader));
   -- A slot may hold either a Desktop backing or a trusted external source.
   -- External import requires a Fresh/confirmed-Closed backing owner; a live
   -- Desktop backing must use completion-gated Import_Backing.
   -- Description is private metadata for this pipeline's provider and an
   -- already-authorized, nonaliasing sampled image. This is NOT a CPU pointer
   -- or cross-process capability importer. Caller retains backing until a
   -- successful Release_Source; unsafe import retains the attempted lease.
   procedure Import_Source (Index : Vulkan_Submission.Source_Slot;
      Description : System.Address; Ticket : out Vulkan_Submission.Source_Ticket;
      Result : out Source_Result)
     with Global => (In_Out => Engine), Pre => Valid,
       Post => Valid and (if Ticket /= Vulkan_Submission.No_Source then Source_Held (Ticket));
   -- Construct metadata for a live, private owned-image record on this device.
   -- Caller retains backing and established shader-readable layout. Foreign
   -- authority/import and CPU uploads are not provided by this constructor.
   procedure Import_Owned_Source (Index : Vulkan_Submission.Source_Slot;
      Image : System.Address; Ticket : out Vulkan_Submission.Source_Ticket;
      Result : out Source_Result)
     with Global => (In_Out => Engine), Pre => Valid,
       Post => Valid and (if Ticket /= Vulkan_Submission.No_Source then Source_Held (Ticket));
   -- Allowed during shutdown after GPU completion, never after health loss.
   -- A null release key gives no permission to retire backing storage.
   procedure Release_Source (Ticket : Vulkan_Submission.Source_Ticket;
      Released : out System.Address)
     with Global => (In_Out => Engine), Pre => Valid,
       Post => Valid and (if Released /= System.Null_Address then not Source_Held (Ticket));
   type Capture_Admission is
     (Capture_Allowed, Capture_Busy, Capture_Unavailable, Capture_Uncertain);
   -- Read-only preflight before any scene readers or cold uploads. This is
   -- not a target reservation, device-health IPC or image sharing authority;
   -- Render must revalidate ownership when it actually acquires a writer.
   function Admit_Capture (Screen : Vulkan_Scene.A.G.Output) return Capture_Admission
     with Global => (Input => Engine), Pre => Valid,
       Post => (if Admit_Capture'Result = Capture_Allowed then Can_Retire_Readers);
   -- Reserve target history before capturing commands; uploads may still use
   -- the idle submission while this CPU-only reservation is held.
   procedure Reserve_Capture (Screen : Vulkan_Scene.A.G.Output;
      Ticket : out Vulkan_Owned_Targets.P.Ticket)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid;
   procedure Capture_Repaint (Ticket : Vulkan_Owned_Targets.P.Ticket;
      Plan : out Compositor_Damage.State; Accepted : out Boolean)
     with Global => (Input => Engine), Pre => Valid,
       Post => Compositor_Damage.Valid (Plan) and
         (if not Accepted then Compositor_Damage.Count (Plan) = 0);
   procedure Cancel_Capture (Ticket : Vulkan_Owned_Targets.P.Ticket;
      Accepted : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid;
   type Frame_Result is (Submitted, Deferred, Rejected, Failed);
   type Poll_Result is (Idle, Pending, Completed, GPU_Failed);
   -- Bounded calls: one scene replay/submission or one fence observation.
   -- A completed frame is an unpublished candidate, not a display latch.
   procedure Render (Scene : Vulkan_Scene.State; Result : out Frame_Result;
      Reservation : Vulkan_Owned_Targets.P.Ticket := Vulkan_Owned_Targets.P.None)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid;
   procedure Poll_Frame (Result : out Poll_Result)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid;
   procedure Poll_Upload (Result : out Poll_Result)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid;
   function Upload_Pending return Boolean with Global => (Input => Engine);
   procedure Damage_Output (Region : Compositor_Damage.Box; Accepted : out Boolean)
     with Global => (In_Out => Engine),
       Pre => Valid and Compositor_Damage.Valid (Region), Post => Valid;
   function Frame_Pending return Boolean with Global => (Input => Engine);
   -- One retirement attempt; pending/uncertain ownership remains in Engine.
   subtype Presentation_Ticket is Vulkan_Owned_Targets.P.Ticket;
   No_Presentation : constant Presentation_Ticket := Vulkan_Owned_Targets.P.None;
   use type Presentation_Ticket;
   function Presentation_Pending return Presentation_Ticket with Global => (Input => Engine);
   function Presentation_Front return Presentation_Ticket with Global => (Input => Engine);
   function Presentation_Faulted return Boolean with Global => (Input => Engine);
   -- A copied presentation has a completed-target reader, not a scanout latch.
   -- Identity only: this does not export an image pointer or admit a recipient.
   function Readback_Pending return Presentation_Ticket with Global => (Input => Engine);
   -- Private coherent staging, charged by actual Vulkan allocation size.
   -- No pixels exposed here; transfer completion and CPU access are separate.
   function Readback_Capacity return Natural with Global => (Input => Engine);
   -- Staging is tightly packed BGRA for the configured target, not an arbitrary
   -- byte array. Equal byte capacity alone cannot establish row geometry.
   function Readback_Layout_Matches (Width, Height : Natural) return Boolean
     with Global => (Input => Engine);
   procedure Submit_Readback (Ticket : Presentation_Ticket; Accepted : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid;
   -- Only the selected output writer's repair pixels are transferred. The
   -- mapping remains full-image layout; other bytes must not be consumed.
   procedure Submit_Region_Readback (Ticket : Presentation_Ticket;
      Repair : Compositor_Damage.State; Accepted : out Boolean)
     with Global => (In_Out => Engine),
       Pre => Valid and Compositor_Damage.Valid (Repair), Post => Valid;
   procedure Poll_Readback (Result : out Poll_Result)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid;
   -- Trusted in-process READ-ONLY borrow; never publish to external clients.
   -- Null before exact completion. Finish CPU reads before Retire_Readback.
   function Readback_Mapping (Ticket : Presentation_Ticket) return System.Address
     with Global => (Input => Engine);
   procedure Configure_Readback (Size : Vulkan_Upload_Owner.Capacity_Range; Ready : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid and
       (if Ready then Readback_Capacity = Size);
   procedure Release_Readback_Storage (Released : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid and
       Configured_Limit = Configured_Limit'Old and
       (if Released then Readback_Capacity = 0);
   procedure Take_Readback (Ticket : out Presentation_Ticket)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid and
       (if Ticket /= No_Presentation then Readback_Pending = Ticket) and
       Presentation_Pending = Presentation_Pending'Old and
       Presentation_Front = Presentation_Front'Old;
   -- Trusted adapter evidence for this exact transfer and all CPU readers.
   -- Cleanup remains available after Stop while targets are retained.
   procedure Retire_Readback
     (Ticket : Presentation_Ticket; Transfer_Complete, CPU_Drained : Boolean;
      Accepted : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid and
       (if Accepted then Readback_Pending = No_Presentation) and
       Presentation_Pending = Presentation_Pending'Old and
       Presentation_Front = Presentation_Front'Old;
   -- Move the newest completed frame into the single display-pending slot.
   -- This exports identity only, not an image pointer or scanout authority.
   procedure Take_Presentation (Ticket : out Presentation_Ticket)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid and
       (if Ticket /= No_Presentation then Presentation_Pending = Ticket) and
       Presentation_Front = Presentation_Front'Old;
   -- Confirmed attests both the new latch AND retirement of the exact old
   -- front, after authenticating output/epoch/frame evidence in the adapter.
   -- Command acceptance or timeout is not confirmation. Invalid evidence
   -- quarantines the pool and retains every possible display reader.
   procedure Confirm_Presentation
     (Ticket, Previous : Presentation_Ticket; Confirmed : Boolean; Accepted : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid and
       (if Accepted then Presentation_Front = Ticket and Presentation_Pending = No_Presentation);
   -- A known rejected/cancelled presentation never became visible and no
   -- display reader remains. Preserve the front and the newest ready frame.
   -- As with latch evidence, the adapter authenticates the matching ticket;
   -- uncertainty or a stale ticket quarantines the pool instead of recycling.
   procedure Cancel_Presentation
     (Ticket : Presentation_Ticket; Quiescent : Boolean; Accepted : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid and
       Presentation_Front = Presentation_Front'Old and
       (if Accepted then Presentation_Pending = No_Presentation);
   -- Output-disable retirement remains necessary for the final visible frame.
   procedure Retire_Presentation
     (Ticket : Presentation_Ticket; Confirmed : Boolean; Accepted : out Boolean)
     with Global => (In_Out => Engine), Pre => Valid, Post => Valid and
       (if Accepted then Presentation_Front = No_Presentation);
   procedure Stop with Global => (In_Out => Engine), Pre => Valid,
     Post => Valid and Configured_Limit = Configured_Limit'Old;
private
   subtype Reader_Index is Positive range 1 .. Natural (Vulkan_Submission.Source_Slot'Last) + 1;
   type Source_Reader is record
      Index : Reader_Index := Reader_Index'First;
      Serial : Interfaces.Unsigned_64 := 0;
   end record;
   No_Source_Reader : constant Source_Reader := (others => <>);
   type Write_Ticket is record
      Index : Backing_Slot := Backing_Slot'First;
      Chunk : Upload_Progress.Ticket := Upload_Progress.No_Ticket;
   end record;
   No_Write : constant Write_Ticket := (others => <>);
end Desktop_Vulkan_Startup;
