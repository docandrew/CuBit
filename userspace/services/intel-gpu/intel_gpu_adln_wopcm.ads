with Interfaces; use Interfaces;
package Intel_GPU_ADLN_WOPCM with SPARK_Mode is
   type Layout is record
      Valid, Locked : Boolean := False;
      Capacity, Base, Bytes, Pin_Bias : Unsigned_64 := 0;
   end record;
   -- ADL-N GuC-only policy: Gen11+ default 2MiB capacity, no HuC upload.
   -- Stable register evidence, not programming or hardware ownership. Do not
   -- infer a larger capacity from firmware-programmed register values.
   function Select_Layout
     (Size_First, Base_First, Size_Second, Base_Second : Unsigned_32;
      Upload_Bytes : Unsigned_64) return Layout
   with Global => null,
     Post => (if Select_Layout'Result.Valid then
       Select_Layout'Result.Capacity = 2_097_152 and then
       Select_Layout'Result.Pin_Bias = Select_Layout'Result.Bytes and then
       Select_Layout'Result.Bytes > 0 and then
       Select_Layout'Result.Base + Select_Layout'Result.Bytes + 36_864 <=
         Select_Layout'Result.Capacity
       else Select_Layout'Result.Capacity = 0 and then
         Select_Layout'Result.Pin_Bias = 0);
end Intel_GPU_ADLN_WOPCM;
