with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
package Intel_GPU_ADS_System_Info with SPARK_Mode is
   Doorbell_Register : constant Unsigned_32 := 16#D08#;
   type Info_Bytes is array (Natural range 0 .. 639) of Unsigned_8;
   type System_Info is record
      Valid : Boolean := False;
      Bytes : Info_Bytes := [others => 0];
   end record;
   function Build (Description : Intel_GPU_ADLN_Inventory.Inventory;
                   Topology : Intel_GPU_ADLN_Steering.Topology;
                   Doorbell_First, Doorbell_Second : Unsigned_32) return System_Info;
   -- ADL-N only: one admitted slice; enabled even physical VDBOXes have SFC.
   -- Doorbell bits23:16 encode count minus one. Caller must hold appropriate
   -- forcewake across both samples and reject an unsuccessful lease release.
   -- This constructs bytes; it does not initialize or publish live ADS.
end Intel_GPU_ADS_System_Info;
