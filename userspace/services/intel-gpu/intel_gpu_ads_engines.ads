with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory;
package Intel_GPU_ADS_Engines with SPARK_Mode is
   -- First 576 bytes of packed guc_gt_system_info, Linux v6.16 ABI.
   -- Remaining 64 bytes of generic system info require hardware observations.
   -- ADL-N only: video physical instances 0,2 compact in that order.
   type Engine_Bytes is array (Natural range 0 .. 575) of Unsigned_8;
   type Encoding is record
      Valid : Boolean := False;
      Bytes : Engine_Bytes := [0 .. 511 => 32, others => 0];
   end record;
   function Encode (Description : Intel_GPU_ADLN_Inventory.Inventory)
     return Encoding
     with Post => Encode'Result.Valid =
       (Description.Valid and then
        Description.Engines (Intel_GPU_ADLN_Inventory.Render) and then
        Description.Engines (Intel_GPU_ADLN_Inventory.Copy));
   -- Valid means representable engine inventory only; neither complete system
   -- information nor executable contexts/ADS publication are established.
end Intel_GPU_ADS_Engines;
