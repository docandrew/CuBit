package body Vulkan_Checker_FFI with SPARK_Mode => Off is
   function Native_Record (Borrowed : System.Address; Value : access constant Request) return Word
     with Import, Convention => C, External_Name => "cubit_vulkan_device_checker_record";
   procedure Record_Draw (Borrowed : System.Address; Value : Request; Result : out Word) is
      Copy : aliased constant Request := Value;
   begin
      Result := Native_Record (Borrowed, Copy'Access);
   end Record_Draw;
end Vulkan_Checker_FFI;
