-- Adapted table values: Linux v6.16 intel_mocs.c, Intel Corporation (2015).
-- MIT license; full notice is retained in intel_gpu_adln_lrc_template.adb.
with Intel_GPU_L3_MOCS_Registers;
with Intel_GPU_MOCS_Control_Registers;
package body Intel_GPU_ADLN_MOCS with SPARK_Mode is
   package Fields renames Intel_GPU_L3_MOCS_Registers;
   function Control (Index : Entry_Index) return Unsigned_32 is
      use Intel_GPU_MOCS_Control_Registers;
      R : Control_Register :=
        (Cacheability => 3, Target_Cache => 1, LRU_Management => 3, others => <>);
   begin
      case Index is
         when 3 | 4 | 49 | 51 | 61 =>
            R.Cacheability := 1; R.LRU_Management := 0;
         when 6 | 7 => R.LRU_Management := 1;
         when 8 | 9 => R.LRU_Management := 2;
         when 10 | 11 => R.Do_Not_Allocate_On_Miss := 1;
         when 12 | 13 =>
            R.LRU_Management := 1; R.Do_Not_Allocate_On_Miss := 1;
         when 14 | 15 =>
            R.LRU_Management := 2; R.Do_Not_Allocate_On_Miss := 1;
         when 16 | 17 =>
            R.Cacheability := 1; R.LRU_Management := 0; R.Snoop_Control := 1;
         when 18 => R.Self_Snoop := 3;
         when 19 => R.Skip_Caching_Control := 7;
         when 20 => R.Skip_Caching_Control := 3;
         when 21 => R.Skip_Caching_Control := 1;
         when 22 => R.Reverse_Skip_Caching := 1; R.Skip_Caching_Control := 3;
         when 23 => R.Reverse_Skip_Caching := 1; R.Skip_Caching_Control := 7;
         when others => null;
      end case;
      return Encode (R);
   end Control;
   function Cache (Index : Entry_Index) return Fields.Bits_2 is
     (if Index in 3 | 5 | 6 | 8 | 10 | 12 | 14 | 16 | 50 | 51 | 60 | 62 | 63
      then Fields.Uncached else Fields.Write_Back);
   function L3 (Index : Entry_Index) return Unsigned_32 is
     (Fields.Pack (Cache (Index), 0));
   function Offset (Index : Register_Index) return Unsigned_32 is
     (if Index < 64 then 16#4000# + Unsigned_32 (Index) * 4
      else 16#B020# + Unsigned_32 (Index - 64) * 4);
   function Value (Index : Register_Index) return Unsigned_32 is
     (if Index < 64 then Control (Index)
      else Fields.Pack (Cache ((Index - 64) * 2), Cache ((Index - 64) * 2 + 1)));
end Intel_GPU_ADLN_MOCS;
