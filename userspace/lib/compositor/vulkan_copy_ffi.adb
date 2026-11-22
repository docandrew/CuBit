with Interfaces;
package body Vulkan_Copy_FFI with SPARK_Mode => Off is
   use type Interfaces.Unsigned_32;
   type Native_Plan is record
      Target_X, Target_Y, Source_X, Source_Y, Width, Height : Interfaces.Unsigned_32;
   end record with Convention => C;
   function Record_Copy (Borrowed : System.Address; P : access constant Native_Plan)
     return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_vulkan_record_copy";
   procedure Record_Plan
     (Borrowed : System.Address; P : Desktop_Composition.Blit_Plan;
      Accepted : out Boolean) is
      Value : aliased constant Native_Plan :=
        (Interfaces.Unsigned_32 (P.Target_X), Interfaces.Unsigned_32 (P.Target_Y),
         Interfaces.Unsigned_32 (P.Source_X), Interfaces.Unsigned_32 (P.Source_Y),
         Interfaces.Unsigned_32 (P.Width), Interfaces.Unsigned_32 (P.Height));
   begin
      pragma Assert (Native_Plan'Size = 24 * 8);
      Accepted := Record_Copy (Borrowed, Value'Access) = 0;
   end Record_Plan;
end Vulkan_Copy_FFI;
