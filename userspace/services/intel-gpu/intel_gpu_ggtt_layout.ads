with Interfaces;
package Intel_GPU_GGTT_Layout with SPARK_Mode is
   use Interfaces;
   Upload_Reservation_Bytes : constant Unsigned_64 := 16#0120_0000#;
   type Layout is record
      Valid : Boolean := False;
      Total, Runtime_First, Runtime_Limit, Upload_First, Upload_Limit,
        Guard_First : Unsigned_64 := 0;
   end record;
   -- ADL-N initial GuC placement policy. Limits are exclusive. Reserve the
   -- same top18MiB even for a table smaller than4GiB; preserve the last page.
   -- Pin_Bias comes from validated WOPCM layout, never an app/IPC argument.
   -- These are numeric regions ONLY: scanout/platform exclusions, retained
   -- ownership, PTE checks and serialized publication remain mandatory.
   function Plan (Table_Bytes, Pin_Bias : Unsigned_64) return Layout
   with Post => (if Plan'Result.Valid then
     Plan'Result.Total = Table_Bytes / 8 * 4096 and then
     Plan'Result.Runtime_First >= Pin_Bias and then
     Plan'Result.Runtime_First >= 4096 and then
     Plan'Result.Runtime_First mod 4096 = 0 and then
     Plan'Result.Runtime_First < Plan'Result.Runtime_Limit and then
     Plan'Result.Runtime_Limit = Plan'Result.Upload_First and then
     Plan'Result.Upload_First = Plan'Result.Total - Upload_Reservation_Bytes and then
     Plan'Result.Upload_First < Plan'Result.Upload_Limit and then
     Plan'Result.Upload_Limit = Plan'Result.Guard_First and then
     Plan'Result.Guard_First = Plan'Result.Total - 4096 and then
     Plan'Result.Runtime_Limit <= 16#FEE0_0000#);
end Intel_GPU_GGTT_Layout;
