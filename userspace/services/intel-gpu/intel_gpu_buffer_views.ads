with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Grant_References;
with Intel_GPU_Buffer_Handles;
with Intel_GPU_Buffer_Reply;

package Intel_GPU_Buffer_Views is
   -- Serialized by the service owner. One view object per grant lifetime;
   -- no retry after creation failure, copying, address reuse or backing free.
   type View is limited private;
   type View_State is (Empty, Shared, Retiring, Retired, Failed);
   function State (Object : View) return View_State;
   function Wire_Reference (Object : View) return Unsigned_64;
   generic
      with function Completed_Backing return Intel_GPU_Buffer_Reply.Backing;
   procedure Share_Completed
     (Object : in out View; Recipient : CuBit.Messages.CapabilitySlot;
      Identity : Unsigned_64);
   -- Trusted driver-only source: returns ONLY completed, coherent pixel pages,
   -- never context/table storage. It must reject after ownership loss and keep
   -- the source immutable/retained until all readers retire. Export is always
   -- read-only and terminal-forwardable. Rechecked after grant creation; loss
   -- or changed backing retires the grant without publishing its reference.
   -- Session is already authenticated; Recipient is a stable endpoint cap
   -- for that SAME session, not a PID or a caller-selected capability slot.
   -- Identity is Recipient_Identity from trusted render admission, not request
   -- data. Keep the slot unchanged until Share returns (including supervisor
   -- edits); the kernel then checks the endpoint's generation during creation.
   -- Registry must contain application BOs only (never context/page tables).
   procedure Share
     (Object : in out View; Buffers : Intel_GPU_Buffer_Handles.Registry;
      Session : Intel_GPU_Buffer_Handles.Session_ID;
      ID : Intel_GPU_Buffer_Handles.Handle;
      Recipient : CuBit.Messages.CapabilitySlot;
      Identity : Unsigned_64;
      Offset, Bytes : Unsigned_64; Writable : Boolean;
      Presentation : Boolean := False);
   -- Presentation explicitly permits terminal read-only forwarding (e.g. to
   -- Desktop). It is rejected with Writable; ordinary mappings never forward.
   -- This is CPU sharing authority, not a GPU-completion or scanout fence.
   procedure Retire (Object : in out View);
   procedure Poll_Retirement (Object : in out View);
   -- Reuse bookkeeping only after the kernel confirms grant retirement.
   -- Caller must replace its public mapping identity before sharing again.
   procedure Recycle (Object : in out View; Accepted : out Boolean);
   -- Retired confirms this grant alone is gone. Other views and GPU work
   -- may still reference backing; none of these operations releases it.
private
   type View is limited record
      Current : View_State := Empty;
      Reference : CuBit.Grant_References.Reference;
   end record;
end Intel_GPU_Buffer_Views;
