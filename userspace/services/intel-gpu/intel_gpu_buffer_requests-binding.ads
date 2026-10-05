with Intel_GPU_VM_Image;
with Intel_GPU_VM_Update;
generic
   with package VM is new Intel_GPU_VM_Image (<>);
package Intel_GPU_Buffer_Requests.Binding is
   Bind_Label : constant Unsigned_32 := 16#0A24#;
   Update_Label : constant Unsigned_32 := 16#0A28#;
   -- Trusted retirement observation, not IPC authority or a release token.
   -- Requires a closed retained name and a sealed VM image, then excludes
   -- every physical alias, including table and scratch backing. Caller must
   -- establish this is the correct session's committed hardware generation,
   -- plus GPU quiescence/TLB completion and independent CPU-loan retirement.
   function Closed_Buffer_Disjoint
     (Object : Service; Image : VM.Image; Session, ID : Unsigned_64)
      return Boolean;
   type Preparation_Result is
     (Request_Denied, Malformed, Stale_Generation, Not_Ready, Eligible, Prepared);
   procedure Check_Offline_Bind_Request
     (Object : Service; Source : VM.Image;
      VM_Session, Expected_Revision, Sender, Stamp : Unsigned_64;
      Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Request : Words; Status : out Preparation_Result);
   -- Allocation-free initial bind preflight. Unlike Update_Label, the BO
   -- offset is word0 high32 (4KiB pages), and word1 is the complete handle.
   -- Only bind operation0 is eligible for backing growth. Captured source
   -- revision must still match and image must be initialized/unsealed. Resolve
   -- ownership/extent again after asynchronous allocation; Eligible is neither
   -- retained authority nor a guarantee of mapping geometry/alias acceptance.
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
   generic
      with function Read_Page (Page : VM.Page_Number) return Unsigned_64;
      with package Coordinator is new Intel_GPU_VM_Update (<>);
   procedure Handle_Update_From_Pages
     (Object : Service; Source : VM.Image; Candidate : in out VM.Image;
      Table_Count : Natural; State : in out Coordinator.State;
      VM_Session, Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words);
   generic
      with package Coordinator is new Intel_GPU_VM_Update (<>);
      Remove : Boolean;
      with procedure Capture
        (Backing : Intel_GPU_Buffer_Reply.Backing;
         GPU, Offset, Bytes, Revision : Unsigned_64; Accepted : out Boolean);
   procedure Handle_In_Place
     (Object : Service; Source : in out VM.Image;
      State : in out Coordinator.State;
      VM_Session, Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words);
   -- Allocation-free authenticated bind/unbind. Remove selects the operation;
   -- mismatched requests reject before Capture. Capture stores only validated
   -- owned backing/range for this serialized transaction; it must not allocate
   -- or publish. For bind it also checks existing-directory/empty-leaf
   -- eligibility before execution. Coordinator callbacks edit Source's retained
   -- tables, invalidate and commit metadata before successful completion.
   -- No private replacement-table ticket or candidate image is required.
   -- Capture and callbacks must preserve session/BO lifetime and exclusion.
   -- Failure after effects or reply loss requires quarantine, never replay.
   generic
      with package Coordinator is new Intel_GPU_VM_Update (<>);
      Remove : Boolean;
      with procedure Capture
        (Backing : Intel_GPU_Buffer_Reply.Backing;
         GPU, Offset, Bytes, Revision : Unsigned_64; Accepted : out Boolean);
   procedure Begin_In_Place
     (Object : Service; Source : in out VM.Image;
      State : in out Coordinator.State;
      VM_Session, Sender, Stamp : Unsigned_64; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words; Started : out Boolean);
   -- Same authenticated capture, but closes coordinator admission and returns
   -- before any hardware stage. Started=True is NOT a successful wire reply;
   -- caller retains reply authority, session/BO lifetime and context hold while
   -- advancing the coordinator. Response is usable only for rejected starts.
   generic
      with package Coordinator is new Intel_GPU_VM_Update (<>);
   procedure Finish_In_Place
     (Object : Service; State : in out Coordinator.State;
      VM_Session, Sender, Stamp, Previous_Generation : Unsigned_64;
      Response : out Words);
   -- Invoke only after terminal coordinator completion. Revalidates identity
   -- and the exact next committed epoch. Failed/incomplete/stale completion
   -- quarantines; no successful reply or implicit hardware resume is inferred.
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
   generic
      with function Read_Page (Page : VM.Page_Number) return Unsigned_64;
   procedure Prepare_Request_From_Pages
     (Object : Service; Source : VM.Image; Candidate : in out VM.Image;
      Table_Count : Natural; VM_Session, Current_Generation : Unsigned_64;
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
   -- A zero suffix denotes unallocated capacity. The populated prefix must
   -- cover the source and requested mapping; holes/nonzero suffixes reject.
   -- Success is NOT publication, invalidation, completion or address reuse.
   -- Caller serializes session/BO/VM lifetime and later establishes exclusion.
   procedure Prepare_Change
     (Object : Service; Source : VM.Image; Candidate : in out VM.Image;
      Tables : VM.Backing_Pages; VM_Session : Unsigned_64;
      Sender, Stamp, ID, GPU, Offset, Bytes : Unsigned_64;
      Remove : Boolean; Accepted : out Boolean);
   generic
      with function Read_Page (Page : VM.Page_Number) return Unsigned_64;
   procedure Prepare_Change_From_Pages
     (Object : Service; Source : VM.Image; Candidate : in out VM.Image;
      Table_Count : Natural; VM_Session : Unsigned_64;
      Sender, Stamp, ID, GPU, Offset, Bytes : Unsigned_64;
      Remove : Boolean; Accepted : out Boolean);
   -- Callback-fed variants read only Table_Count authenticated, retained
   -- table pages. Caller serializes both images; zero rejects. Reader must
   -- not mutate either image. No client address or backing allocation authority
   -- is granted by this interface. Array callers use these same cores.
   -- Offline binding: [version | (operation << 16) | (BO offset in pages << 32),
   -- handle, GPU address, bytes]. Low16 is the version; operation is0(bind)
   -- or1(unbind) in bits16..31; high32 is an unsigned 4KiB
   -- page offset. The registry validates the complete slice against backing.
   -- Reply [status, version, GPU address, bytes] on success, trailing zeros
   -- otherwise. No live update; sealed VMs reject both operations.
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
   -- Same authenticated owner and retained-handle checks. Used by the offline
   -- wire handler; modifies only an unpublished image, not a live VM.
end Intel_GPU_Buffer_Requests.Binding;
