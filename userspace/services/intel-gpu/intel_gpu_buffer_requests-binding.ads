with Intel_GPU_VM_Image;
with Intel_GPU_VM_Update;
generic
   with package VM is new Intel_GPU_VM_Image (<>);
package Intel_GPU_Buffer_Requests.Binding is
   Bind_Label : constant Unsigned_32 := 16#0A24#;
   Update_Label : constant Unsigned_32 := 16#0A28#;
   type Preparation_Result is
     (Request_Denied, Malformed, Stale_Generation, Not_Ready, Eligible, Prepared);
   procedure Check_Update_Request
     (Object : Service; Source : VM.Image;
      VM_Session, Current_Generation, Sender, Stamp : Unsigned_64;
      Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Request : Words; Status : out Preparation_Result);
   -- Allocation-free preflight for asynchronous native dispatch. Eligible
   -- validates the envelope, epoch and owned BO extent, not a future mapping
   -- or table capacity. No candidate is consumed. Prepare_Request repeats
   -- these checks after allocation; eligibility is NOT retained authority.
   generic
      with package Coordinator is new Intel_GPU_VM_Update (<>);
   procedure Handle_Update
     (Object : Service; Source : VM.Image; Candidate : in out VM.Image;
      Tables : VM.Backing_Pages; State : in out Coordinator.State;
      VM_Session, Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words);
   -- Serialized transaction handler, not native dispatch enablement. Coordinator
   -- callbacks must use THIS candidate, retain both generations, and complete
   -- drain/publication/invalidation/resume. Success contains the committed epoch.
   -- Caller installs Candidate as its current image before processing more work;
   -- failed reply delivery must retire the captured session, never roll back.
   -- Live-update protocol (native dispatch exists; public admission gated):
   -- word0: version1 in bits0..15, operation0(bind)/1(unbind) in16..31,
   --        expected VM generation in32..63;
   -- word1: BO handle in0..31, BO offset in4KiB pages in32..63;
   -- word2: raw48 GPU virtual address; word3: byte length.
   -- Trusted Current_Generation comes from the serialized VM coordinator.
   -- Authentication and stale/malformed rejection precede candidate mutation.
   -- Prepared is NOT a success reply: caller must still exclude/drain work,
   -- publish, invalidate, resume and commit the generation before replying.
   -- Uncertain publication/reply must retire the session, never replay.
   procedure Prepare_Request
     (Object : Service; Source : VM.Image; Candidate : in out VM.Image;
      Tables : VM.Backing_Pages; VM_Session, Current_Generation : Unsigned_64;
      Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Status : out Preparation_Result);
   -- Submission preflight only: resolve the live handle in the authenticated
   -- session, then check the complete DWORD-sized batch slice against its
   -- sealed published VM generation. QWORD-aligned raw48 start. No command
   -- validation, immutability, scheduling or completion is implied. Caller
   -- serializes with close/retirement and retains VM/backing through execution.
   function Batch_Mapped
     (Object : Service; Image : VM.Image; VM_Session : Unsigned_64;
      Sender, Stamp, ID, GPU, Offset, Bytes : Unsigned_64) return Boolean;
   -- Prepare a fresh sealed generation for the existing stable-root updater.
   -- Source remains untouched. Tables are trusted fresh retained DMA backing,
   -- never client addresses. Candidate is single-attempt; retain it on failure.
   -- Success is NOT publication, invalidation, completion or address reuse.
   -- Caller serializes session/BO/VM lifetime and later establishes exclusion.
   procedure Prepare_Change
     (Object : Service; Source : VM.Image; Candidate : in out VM.Image;
      Tables : VM.Backing_Pages; VM_Session : Unsigned_64;
      Sender, Stamp, ID, GPU, Offset, Bytes : Unsigned_64;
      Remove : Boolean; Accepted : out Boolean);
   -- Offline binding: [version | (BO offset in pages << 32), handle,
   -- GPU address, bytes]. Low32 is the version; high32 is an unsigned 4KiB
   -- page offset. The registry validates the complete slice against backing.
   -- Reply [status, version, GPU address, bytes] on success, trailing zeros
   -- otherwise. No unbind/live update; sealed VMs reject further bindings.
   procedure Handle
     (Object : Service; Image : in out VM.Image; VM_Session : Unsigned_64;
      Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words);
   -- Driver-internal range binding used by the initial offline wire operation.
   -- VM_Session comes from the dispatcher's retained VM owner, never request
   -- words. Serialize with admission, buffer close and VM mutation. The caller
   -- retains backing and revalidates ownership before publishing the image.
   procedure Bind
     (Object : Service; Image : in out VM.Image; VM_Session : Unsigned_64;
      Sender, Stamp, ID, GPU, Offset, Bytes : Unsigned_64;
      Accepted : out Boolean);
   -- Only registry-owned buffer ranges are reachable. No physical/CPU address
   -- leaves the registry; WB policy and RW-only Gen12 policy are driver-owned.
   -- Closing a handle later does not unbind this image or cancel GPU work.
   procedure Unbind
     (Object : Service; Image : in out VM.Image; VM_Session : Unsigned_64;
      Sender, Stamp, ID, GPU, Offset, Bytes : Unsigned_64;
      Accepted : out Boolean);
   -- Driver-internal only, same authenticated owner and retained-handle checks.
   -- No wire endpoint: this modifies only an unpublished image, not a live VM.
end Intel_GPU_Buffer_Requests.Binding;
