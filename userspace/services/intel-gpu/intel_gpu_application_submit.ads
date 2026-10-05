with Interfaces; use Interfaces;
with System;
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
   type Completion_Receipt is limited private;
   -- Keep the exact State root alive and unmoved through receipt consumption.
   -- A receipt is internal evidence, never a client-supplied wire structure.
   function Receipt_Confirmed (Object : State; Receipt : Completion_Receipt)
      return Boolean;
   function Receipt_Sequence (Object : State; Receipt : Completion_Receipt)
      return Unsigned_32;
   function Current (Object : State) return Phase;
   function Last_Completed (Object : State) return Unsigned_32;
   -- Observation for the EXACT state/context incarnation selected by the
   -- trusted coordinator. Sequence must come from its reserved submission,
   -- never a client completion claim. Marker1 is setup, not application work.
   -- Only Idle after marker observation AND disable confirms application work.
   -- Does not establish current ownership or CPU/display consumer retirement.
   function Completion_Confirmed (Object : State; Sequence : Unsigned_32)
      return Boolean;
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
   -- Caller must reserve all consumer obligations BEFORE dispatch. Only a
   -- completed execution seals this single-use receipt; uncertainty retains
   -- the caller's obligations without manufacturing completion evidence.
   procedure Execute_With_Receipt
     (Object : in out State; Handle, GPU, Offset, Bytes : Unsigned_64;
      Receipt : in out Completion_Receipt; Status : out Result);
private
   type Completion_Receipt is limited record
      Attempted : Boolean := False;
      Origin : System.Address := System.Null_Address;
      Sequence : Unsigned_32 := 0;
   end record;
   type State is limited record
      Value : Phase := Uninitialized;
      Completed : Unsigned_32 := 0;
   end record;
end Intel_GPU_Application_Submit;
