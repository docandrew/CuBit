with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
package Intel_GPU_ADS_Capture_Image with SPARK_Mode is
   Capacity : constant := 8 * 4096;
   type Capture_Bytes is array (Natural range 0 .. Capacity - 1) of Unsigned_8;
   -- Packed guc_ads: capture_instance[2][16], capture_class[2][16], global[2].
   type Pointer_Bytes is array (Natural range 0 .. 263) of Unsigned_8;
   type Capture_Image is record
      Valid : Boolean := False;
      Used : Natural range 0 .. Capacity := 0;
      Data : Capture_Bytes := [others => 0];
      Pointers : Pointer_Bytes := [others => 0];
   end record;
   function Build
     (Description : Intel_GPU_ADLN_Inventory.Inventory;
      Steering : Intel_GPU_ADLN_Steering.Topology;
      GPU_Base, Backing_Bytes : Unsigned_64) return Capture_Image;
   -- GPU_Base names the capture section, not the ADS base. Numeric admission
   -- only: mapping, ownership, coherency and firmware publication are external.
end Intel_GPU_ADS_Capture_Image;
