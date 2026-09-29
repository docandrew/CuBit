with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
with Intel_GPU_ADS_Layout;
with Intel_GPU_ADS_Header;
with Intel_GPU_ADS_Policies;
with Intel_GPU_ADS_System_Info;
with Intel_GPU_ADS_Register_Image;
with Intel_GPU_ADS_Capture_Image;
package Intel_GPU_ADS_Initialization with SPARK_Mode is
   type Prepared_Image is record
      Valid : Boolean := False;
      Layout : Intel_GPU_ADS_Layout.Layout;
      Header : Intel_GPU_ADS_Header.Header_Bytes := [others => 0];
      Policies : Intel_GPU_ADS_Policies.Policy_Bytes := [others => 0];
      System_Info : Intel_GPU_ADS_System_Info.System_Info;
      Registers : Intel_GPU_ADS_Register_Image.Register_Image;
      Capture : Intel_GPU_ADS_Capture_Image.Capture_Image;
   end record;
   function Prepare
     (Inventory : Intel_GPU_ADLN_Inventory.Inventory;
      Topology : Intel_GPU_ADLN_Steering.Topology;
      Doorbell_First, Doorbell_Second, MMIO_Bytes : Unsigned_32;
      GPU_Base, Backing_Bytes, Firmware_Private_Bytes : Unsigned_64)
      return Prepared_Image
     with Post => (if Prepare'Result.Valid then
       Prepare'Result.Layout.Valid and then
       Intel_GPU_ADS_Layout.Sound (Prepare'Result.Layout) and then
       Prepare'Result.Layout.Total <= Backing_Bytes and then
       GPU_Base > 0 and then GPU_Base mod 4096 = 0 and then
       GPU_Base < Intel_GPU_ADS_Layout.Limit and then
       Prepare'Result.Layout.Total <= Intel_GPU_ADS_Layout.Limit - GPU_Base and then
       Prepare'Result.System_Info.Valid and then
       Prepare'Result.Registers.Valid and then Prepare'Result.Capture.Valid and then
       Prepare'Result.Policies (76) = 1);
   -- ADL-N / admitted GuC70.49.4 only. Zero the entire allocation before
   -- copying these sections, including usage/private/golden storage. Early
   -- golden data is intentionally empty and engine recovery stays disabled.
   -- Valid is numeric/ABI preparation, not GGTT ownership, coherency, native
   -- MMIO setup, executable contexts or permission to publish to firmware.
end Intel_GPU_ADS_Initialization;
