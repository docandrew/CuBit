package body Vulkan_Device_Targets_FFI with SPARK_Mode => Off is
   -- Mirrors cubit_vulkan_device_targets: four native pointers, then uint32.
   -- No ownership or handles escape this process-private metadata boundary.
   type Native_Description is record
      Description, A, B, C : System.Address := System.Null_Address;
      Allowed : Interfaces.Unsigned_32 := 0;
   end record with Convention => C;
   function Native_Prepare
     (Width, Height : Interfaces.Unsigned_32; Result : access Native_Description)
      return Interfaces.Unsigned_32
     with Import, Convention => C,
       External_Name => "cubit_vulkan_device_targets_prepare";
   procedure Prepare
     (Width, Height : Interfaces.Unsigned_32;
      Description : out System.Address; Requests : out Vulkan_Frame.Targets;
      Allowed_Types : out Interfaces.Unsigned_32) is
      Native : aliased Native_Description;
      use type Interfaces.Unsigned_32;
   begin
      Description := System.Null_Address;
      Requests := (others => System.Null_Address);
      Allowed_Types := 0;
      if Native_Prepare (Width, Height, Native'Access) = 0 then
         Description := Native.Description;
         Requests := (Native.A, Native.B, Native.C);
         Allowed_Types := Native.Allowed;
      end if;
   end Prepare;
end Vulkan_Device_Targets_FFI;
