package body Vulkan_Device_Source_FFI with SPARK_Mode => Off is
   type Native_Description is record
      Image : System.Address := System.Null_Address;
      Allowed : Interfaces.Unsigned_32 := 0;
   end record with Convention => C;
   function Native_Prepare (Index, Width, Height, Mask : Interfaces.Unsigned_32;
      Description : access Native_Description) return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_vulkan_device_source_prepare";
   procedure Prepare (Index : Slot; Width, Height : Interfaces.Unsigned_32;
      Mask : Boolean; Request : out System.Address;
      Allowed_Types : out Interfaces.Unsigned_32) is
      Native : aliased Native_Description;
      use type Interfaces.Unsigned_32;
   begin
      Request := System.Null_Address; Allowed_Types := 0;
      if Native_Prepare (Index, Width, Height, (if Mask then 1 else 0), Native'Access) = 0 then
         Request := Native.Image; Allowed_Types := Native.Allowed;
      end if;
   end Prepare;
end Vulkan_Device_Source_FFI;
