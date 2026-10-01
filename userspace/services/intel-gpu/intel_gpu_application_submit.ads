with Interfaces; use Interfaces;
generic
   -- All callbacks select the SAME authenticated session/context incarnation.
   -- Serialize against retirement, BO close, VM updates and other submissions.
   -- Ready includes command-privilege configuration, isolated retained VM,
   -- inaccessible completion storage and bounded recovery, not just firmware.
   with function Owner_Ready return Boolean;
   -- Resolve an application BO and its entire batch slice in the published VM.
   -- No client CPU/DMA address. This is not a software command-parser proof.
   with function Batch_Ready (Handle, GPU, Offset, Bytes : Unsigned_64) return Boolean;
   type Completion_Attempt is limited private;
   -- Arm verifies Previous in protected completion storage before publication.
   with procedure Arm (Attempt : in out Completion_Attempt;
                        Previous, Expected : Unsigned_32; OK : out Boolean);
   with procedure Enable (OK : out Boolean);
   -- Driver constructs the NONPRIVILEGED PPGTT branch and completion barrier
   -- in its inaccessible ring; the app never supplies privileged ring words.
   with procedure Publish (GPU : Unsigned_64; Sequence : Unsigned_32; OK : out Boolean);
   with procedure Notify (OK : out Boolean);
   with procedure Wait_Completion (Attempt : in out Completion_Attempt; OK : out Boolean);
   with procedure Disable (OK : out Boolean);
   -- Irreversibly close admission, initiate bounded recovery and retain ALL
   -- backing. Called on any uncertain execution/ownership result, even if the
   -- publication callback returned False after partially writing a ring tail.
   with procedure Quarantine;
package Intel_GPU_Application_Submit is
   type Phase is (Uninitialized, Idle, Checking, Executing, Failed);
   type Result is (Rejected, Batch_Denied, Complete, Faulted, Exhausted);
   type State is limited private;
   function Current (Object : State) return Phase;
   function Last_Completed (Object : State) return Unsigned_32;
   -- Trusted dispatcher only, after setup marker1 AND disable acknowledgement.
   -- One-shot; caller must not manufacture this fact from a client request.
   procedure Initialize (Object : in out State; Setup_Complete : Boolean);
   -- No new IPC endpoint here. A completed result includes scheduling disable;
   -- it does not free resources, invalidate VM mappings or release CPU readers.
   -- All callbacks must be bounded and nonraising. This bring-up coordinator
   -- is synchronous, not the eventual asynchronous queue implementation.
   procedure Execute
     (Object : in out State; Handle, GPU, Offset, Bytes : Unsigned_64;
      Status : out Result; Completion : out Unsigned_32);
private
   type State is limited record
      Value : Phase := Uninitialized;
      Completed : Unsigned_32 := 0;
   end record;
end Intel_GPU_Application_Submit;
