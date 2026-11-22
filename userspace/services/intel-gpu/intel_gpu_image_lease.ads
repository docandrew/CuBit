with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Handles;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Image_Layout;
package Intel_GPU_Image_Lease is
   -- Internal serialized coordinator API, NOT a client IPC/capability API.
   -- All identities and completion facts come from trusted service state.
   -- A lifetime pin alone never authorizes CPU/GPU access or excludes writers.
   type Identity is record
      Adapter, Session, Allocation, Output_Epoch, Serial : Unsigned_64 := 0;
      Display_Instance, Output_Number, Consumer_Instance : Unsigned_64 := 0;
   end record;
   -- Output number zero is valid. Display/consumer incarnations are nonzero,
   -- authenticated lifetime identities, not PIDs or local registry constants.
   -- This record is correlation metadata, NOT a capability or wire ABI.
   type State is (Empty, Held, Retired);
   type Lease is limited private;
   function Current (Object : Lease) return State;
   procedure Prepare
     (Object : in out Lease; Buffers : in out Intel_GPU_Buffer_Handles.Registry;
      Source : Intel_GPU_Buffer_Handles.Retained_Reference;
      Key : Identity; Image : Intel_GPU_Image_Layout.Descriptor;
      Authorized, Producer_Quiescent : Boolean; Accepted : out Boolean);
   -- Session/allocation must match Source's retained registry identity.
   -- Allocation is its original non-reused BO name, not a CPU/GPU address.
   -- Caller authenticates the complete output/consumer binding and serial,
   -- draining prior writers BEFORE Prepare. Prepare adds the independent hold;
   -- every producer admission path must honor it through consumer retirement.
   -- Producer_Quiescent is not a client claim or a substitute for that policy.
   -- Object is single-use, including after retirement, preventing local ABA.
   function Backing
     (Object : Lease; Buffers : Intel_GPU_Buffer_Handles.Registry; Key : Identity)
      return Intel_GPU_Buffer_Reply.Backing;
   function Layout (Object : Lease; Key : Identity)
      return Intel_GPU_Image_Layout.Descriptor;
   procedure Retire
     (Object : in out Lease; Buffers : in out Intel_GPU_Buffer_Handles.Registry;
      Key : Identity; GPU_Drained, CPU_Drained, Display_Drained : Boolean;
      Accepted : out Boolean);
   -- Queue acceptance or new-front latch is NOT old-consumer retirement.
   -- Each fact covers ALL consumers of this lease, including uncertain replies.
   -- False/unknown evidence retains the pin. This operation only returns the
   -- lease's pin, never releases backing or other independent pins.
private
   type Lease is limited record
      Phase : State := Empty;
      Key : Identity;
      Image : Intel_GPU_Image_Layout.Descriptor;
      Pin : Intel_GPU_Buffer_Handles.Retained_Reference;
   end record;
end Intel_GPU_Image_Lease;
