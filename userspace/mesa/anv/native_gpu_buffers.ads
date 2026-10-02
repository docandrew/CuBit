with Interfaces; use Interfaces;
package Native_GPU_Buffers is
   -- Owned WB RAM contract:0 unavailable/invalid,1 explicit maintenance,
   -- 2 coherent. Discovery only; same stable endpoint as allocation.
   function Memory_Contract (Slot : Unsigned_64) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_memory_contract";
   -- Active-session health observation:0 ready,1 denied,2 malformed,
   -- 3 unavailable,4 invalid transport. Not a submission authorization lease.
   function Session_Status (Slot : Unsigned_64) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_session_status";
   -- Read-only query after admission close:0 quiescent,1 denied,2 malformed,
   -- 3 unavailable/uncertain,4 still retiring,5 invalid transport/reply.
   -- May poll with the SAME retained capability; never reissue Close as a
   -- poll. Quiescence does not reclaim backing, context IDs or capability slots.
   function Poll_Session_Retirement (Slot : Unsigned_64) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_poll_session_retirement";
   -- Close only the session selected by this stable capability. Returned tag
   -- is diagnostic identity, NOT authority or completed GPU/grant retirement.
   -- Output clears on failure. One attempt; uncertainty must not be replayed.
   function Close_Session (Slot : Unsigned_64; Retired_Tag : access Unsigned_64)
      return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_close_session";
   -- Pending live VM bind/unbind transport (driver dispatch not enabled yet).
   -- Remove=0 binds,1 unbinds; generation0 is the initial prepared VM.
   -- Success requires an exact successor after completed publication,
   -- invalidation and resume, NOT merely candidate preparation. Output clears
   -- on failure. Same status codes as Create; never replay uncertain updates.
   function Update_Binding
     (Slot : Unsigned_64; Handle : Unsigned_32; GPU, Offset, Bytes : Unsigned_64;
      Remove, Previous : Unsigned_32; Generation : access Unsigned_32)
      return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_update_binding";
   -- Synchronous application submission after trusted setup. Previous starts
   -- at 1; success must return its exact successor. Caller serializes the
   -- session and pins all reachable BO/VM backing. Output clears on failure.
   -- Same codes as Create; uncertainty requires retirement, NEVER replay.
   function Submit
     (Slot : Unsigned_64; Handle : Unsigned_32; GPU, Offset, Bytes : Unsigned_64;
      Previous : Unsigned_32; Completion : access Unsigned_32) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_submit_batch";
   -- Register and initialize the prepared context, once per session.
   -- Success means driver setup completed and scheduling disable acknowledged,
   -- NOT permission to submit application batches. Same uncertainty rules as Create.
   function Register_Context (Slot : Unsigned_64) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_register_context";
   -- One-shot seal/materialize/publish of the session's bound context VM.
   -- NOT GuC registration, scheduling, submission or a GPU completion fence.
   -- Same status codes as Create. Any uncertain result requires session
   -- retirement, never replay; no further offline binds after success.
   function Prepare_Context (Slot : Unsigned_64) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_prepare_context";
   -- Offline GPU binding of a whole-page BO slice. Same codes as Create. No live
   -- remapping, publication or execution; uncertain replies require retirement
   -- of the session rather than replay. Caller pins endpoint/BO lifetimes.
   function Bind_GPU
     (Slot : Unsigned_64; Handle : Unsigned_32; GPU, Offset, Bytes : Unsigned_64)
      return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_bind_buffer";
   -- Unbind only an unpublished VM slice. Does not free backing or alter a
   -- sealed/live image; same status and no-replay rules as Bind_GPU.
   function Unbind_GPU
     (Slot : Unsigned_64; Handle : Unsigned_32; GPU, Offset, Bytes : Unsigned_64)
      return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_unbind_buffer";
   -- Caller pins the render endpoint slot and serializes BO lifetime changes.
   -- 0=success, 1..3=service denial/bad request/unavailable, 4=invalid transport
   -- or reply. Failed/uncertain create can leave a retained server allocation;
   -- it must not be blindly retried. Session retirement invalidates names,
   -- but the initial driver does not yet reclaim that backing.
   function Create
     (Slot, Bytes : Unsigned_64; Handle : access Unsigned_32) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_create_buffer";
   function Close (Slot : Unsigned_64; Handle : Unsigned_32) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_close_buffer";
   -- Close retires a name, not CPU mappings, GPU work or physical backing.
   -- Mapping protocol: 0 success, 1..3 service errors, 4 retirement pending,
   -- 5 local/transport/protocol error (unlike create/close's local error4).
   -- Outputs clear on failure. This requests a grant, not a CPU address;
   -- acquire it separately with the memory FFI. Never blindly retry Map.
   function Map
     (Slot : Unsigned_64; Handle : Unsigned_32; Offset, Bytes : Unsigned_64;
      Writable : Unsigned_32; Mapping : access Unsigned_32;
      Reference : access Unsigned_64) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_map_buffer";
   -- Presentation requests a read-only, terminal-forwardable grant of an app
   -- BO. Acquire then derive to Desktop; no GPU completion is implied. Same
   -- status codes and uncertainty rules as Map; never blindly retry.
   function Map_Presentation
     (Slot : Unsigned_64; Handle : Unsigned_32; Offset, Bytes : Unsigned_64;
      Mapping : access Unsigned_32; Reference : access Unsigned_64)
      return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_map_presentation";
   -- Return any CPU acquisition before retiring its grant. Pending is polled
   -- by repeating Retire_Map, never by repeating Map. No GPU fence implied.
   function Retire_Map
     (Slot : Unsigned_64; Mapping : Unsigned_32) return Unsigned_32
     with Export, Convention => C, External_Name => "cubit_intel_retire_mapping";
end Native_GPU_Buffers;
