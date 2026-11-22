with Compositor_Damage;
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
   procedure Record_Readback_Regions (Description, Submission, Staging : System.Address;
      Slot : Interfaces.Unsigned_32; Repair : Compositor_Damage.State;
      Result : out Interfaces.Unsigned_32)
     with Global => null, Pre => Compositor_Damage.Valid (Repair) and
       Compositor_Damage.Count (Repair) > 0 and
       Compositor_Damage.Bounds (Repair).Right <= 65535 and
       Compositor_Damage.Bounds (Repair).Bottom <= 65535;
end Vulkan_Owned_Target_FFI;
