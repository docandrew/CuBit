with Test_Control;
package body Desktop_Vulkan_Startup is
 package T renames Test_Control;
 Status : Vulkan_Device_Owner.Phase := Vulkan_Device_Owner.Fresh;
 procedure Initialize(Slot : Interfaces.Unsigned_64) is
  use type Interfaces.Unsigned_64; OK : Boolean;
 begin T.Attempt(T.Device,OK); Status := (if OK and Slot/=0 then Vulkan_Device_Owner.Ready else Vulkan_Device_Owner.Retired); end;
 function Current return Vulkan_Device_Owner.Phase is (Status);
 procedure Check_Health(Usable : out Boolean) is begin T.Attempt(T.Health,Usable); end;
 procedure Configure_Targets(Width, Height : Interfaces.Unsigned_32; Epoch : Compositor_Pool.ID; Byte_Limit : Natural; Ready : out Boolean) is
 begin T.Attempt(T.Targets,Ready); end;
 procedure Prepare_Pipeline(Ready : out Boolean) is begin T.Attempt(T.Pipeline,Ready); end;
 procedure Configure_Upload(Size : Natural; Ready : out Boolean) is begin T.Attempt(T.Upload,Ready); end;
 procedure Configure_Readback(Size : Natural; Ready : out Boolean) is begin T.Attempt(T.Readback,Ready); end;
 procedure Stop is begin T.Stops := T.Stops+1; Status := Vulkan_Device_Owner.Retired; end;
end Desktop_Vulkan_Startup;
