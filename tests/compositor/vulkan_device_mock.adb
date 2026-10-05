package body Vulkan_Device_Mock with SPARK_Mode => Off is
   procedure Native_Set (Owned, Description, Retirement : Interfaces.Unsigned_32)
     with Import, Convention => C, External_Name => "device_mock_set";
   procedure Set (Owned : Boolean; Description : Boolean; Retirement : Interfaces.Unsigned_32) is
   begin Native_Set (Boolean'Pos (Owned), Boolean'Pos (Description), Retirement); end Set;
   function Native_Starts return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "device_mock_starts";
   function Native_Closes return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "device_mock_closes";
   function Starts return Interfaces.Unsigned_32 is (Native_Starts);
   function Closes return Interfaces.Unsigned_32 is (Native_Closes);
end Vulkan_Device_Mock;
