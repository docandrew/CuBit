-- Adapted table values: Linux v6.16 intel_mocs.c, Intel Corporation (2015).
-- MIT license; full notice is retained in intel_gpu_adln_lrc_template.adb.
package body Intel_GPU_ADLN_MOCS with SPARK_Mode is
   function Control (Index : Entry_Index) return Unsigned_32 is
     (case Index is
        when 3 | 4 | 49 | 51 | 61 => 16#5#,
        when 6 | 7 => 16#17#,
        when 8 | 9 => 16#27#,
        when 10 | 11 => 16#77#,
        when 12 | 13 => 16#57#,
        when 14 | 15 => 16#67#,
        when 16 | 17 => 16#4005#,
        when 18 => 16#60037#,
        when 19 => 16#737#,
        when 20 => 16#337#,
        when 21 => 16#137#,
        when 22 => 16#3B7#,
        when 23 => 16#7B7#,
        when others => 16#37#);
   function L3 (Index : Entry_Index) return Unsigned_32 is
     (if Index in 3 | 5 | 6 | 8 | 10 | 12 | 14 | 16 | 50 | 51 | 60 | 62 | 63
      then 16#10# else 16#30#);
   function Offset (Index : Register_Index) return Unsigned_32 is
     (if Index < 64 then 16#4000# + Unsigned_32 (Index) * 4
      else 16#B020# + Unsigned_32 (Index - 64) * 4);
   function Value (Index : Register_Index) return Unsigned_32 is
     (if Index < 64 then Control (Index)
      else L3 ((Index - 64) * 2) or Shift_Left (L3 ((Index - 64) * 2 + 1), 16));
end Intel_GPU_ADLN_MOCS;
