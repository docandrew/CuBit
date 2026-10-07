with System; with Interfaces;
package Vulkan_Owned_Target_FFI with SPARK_Mode is
   procedure Bind (Description, A, B, C, Submission : System.Address; Result : out Interfaces.Unsigned_32)
     with Global => null;
   procedure Prepare_Frame (Description, Submission : System.Address;
      Slot, Width, Height : Interfaces.Unsigned_32; Discard : Boolean;
      Result : out Interfaces.Unsigned_32) with Global => null;
   procedure Record_Readback (Description, Submission, Staging : System.Address;
      Slot : Interfaces.Unsigned_32; Result : out Interfaces.Unsigned_32)
     with Global => null;
end Vulkan_Owned_Target_FFI;
