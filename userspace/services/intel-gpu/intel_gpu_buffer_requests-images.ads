with Intel_GPU_Image_Lease;
with Intel_GPU_Image_Layout;
generic
   -- Serialized trusted coordinator callbacks, never application assertions.
   -- Authorize checks adapter/output generation and the intended consumer.
   with function Authorize
     (Key : Intel_GPU_Image_Lease.Identity;
      Image : Intel_GPU_Image_Layout.Descriptor) return Boolean;
   -- Covers GPU work AND all existing writable CPU aliases of this BO.
   with function Producer_Drained (Session, Allocation : Unsigned_64) return Boolean;
   -- Covers all GPU/CPU/display consumers, including uncertain handoffs.
   with function Consumers_Drained (Key : Intel_GPU_Image_Lease.Identity) return Boolean;
package Intel_GPU_Buffer_Requests.Images is
   -- Driver-internal provider only. No new wire protocol or import capability.
   procedure Prepare
     (Object : in out Service; Sender, Stamp : Unsigned_64;
      Key : Intel_GPU_Image_Lease.Identity;
      Image : Intel_GPU_Image_Layout.Descriptor;
      Lease : in out Intel_GPU_Image_Lease.Lease; Accepted : out Boolean);
   function Backing
     (Object : Service; Lease : Intel_GPU_Image_Lease.Lease;
      Key : Intel_GPU_Image_Lease.Identity) return Intel_GPU_Buffer_Reply.Backing;
   -- Cleanup may follow session closure, but requires exact lease identity
   -- and trusted consumer retirement. It does not authorize new access.
   procedure Retire
     (Object : in out Service; Lease : in out Intel_GPU_Image_Lease.Lease;
      Key : Intel_GPU_Image_Lease.Identity; Accepted : out Boolean);
end Intel_GPU_Buffer_Requests.Images;
