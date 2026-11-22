with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
package Intel_GPU_ADS_Register_Image with SPARK_Mode is
   Capacity : constant := (63 + 4 * 55) * 16;
   type Register_Bytes is array (Natural range 0 .. Capacity - 1) of Unsigned_8;
   type Descriptor_Bytes is array (Natural range 0 .. 4095) of Unsigned_8;
   type Register_Image is record
      Valid : Boolean := False;
      Used : Natural range 0 .. Capacity := 0;
      Registers : Register_Bytes := [others => 0];
      Descriptors : Descriptor_Bytes := [others => 0];
   end record;
   function Build (Description : Intel_GPU_ADLN_Inventory.Inventory;
                   Steering : Intel_GPU_ADLN_Steering.Topology;
                   MMIO_Bytes : Unsigned_32;
                   Register_GPU_Base : Unsigned_64) return Register_Image;
   -- Base names the register section, NOT the ADS allocation base. It may be
   -- DWORD-aligned rather than page-aligned (packed layout starts at21692).
   -- Numeric range validation only: requires owned, coherent GGTT backing
   -- before these bytes can be copied/published for firmware consumption.
end Intel_GPU_ADS_Register_Image;
