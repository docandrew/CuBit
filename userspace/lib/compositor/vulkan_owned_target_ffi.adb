package body Vulkan_Owned_Target_FFI with SPARK_Mode => Off is
   function Native_Bind (Description, A, B, C, Submission : System.Address) return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_vulkan_owned_targets_bind";
   procedure Bind (Description, A, B, C, Submission : System.Address; Result : out Interfaces.Unsigned_32) is
   begin Result := Native_Bind (Description, A, B, C, Submission); end Bind;
   function Native_Prepare (Description, Submission : System.Address;
      Slot, Width, Height, Discard : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_vulkan_owned_targets_prepare_frame";
   procedure Prepare_Frame (Description, Submission : System.Address;
      Slot, Width, Height : Interfaces.Unsigned_32; Discard : Boolean;
      Result : out Interfaces.Unsigned_32) is
   begin
      Result := Native_Prepare (Description, Submission, Slot, Width, Height,
                                (if Discard then 1 else 0));
   end Prepare_Frame;
end Vulkan_Owned_Target_FFI;
