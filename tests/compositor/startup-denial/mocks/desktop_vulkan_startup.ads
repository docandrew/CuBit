with Interfaces; with Compositor_Pool; with Vulkan_Device_Owner;
package Desktop_Vulkan_Startup is
 procedure Initialize(Slot : Interfaces.Unsigned_64);
 function Current return Vulkan_Device_Owner.Phase;
 procedure Check_Health(Usable : out Boolean);
 procedure Configure_Targets(Width, Height : Interfaces.Unsigned_32; Epoch : Compositor_Pool.ID; Byte_Limit : Natural; Ready : out Boolean);
 procedure Prepare_Pipeline(Ready : out Boolean);
 procedure Configure_Upload(Size : Natural; Ready : out Boolean);
 procedure Configure_Readback(Size : Natural; Ready : out Boolean);
 procedure Stop;
end Desktop_Vulkan_Startup;
