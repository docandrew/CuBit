with Interfaces; use Interfaces;
package Intel_GPU_ADS_Backing with SPARK_Mode is
   Request_Label : constant := 16#0230#;
   CPU_Address : constant Unsigned_64 := 16#6200_0000#;
   Allocation_Order : constant Unsigned_64 := 12;
   Capacity : constant Unsigned_64 := 16 * 1024 * 1024;
   function Valid_Physical (Physical : Unsigned_64) return Boolean is
     (Physical /= 0 and then Physical mod 4096 = 0 and then
      Physical <= 2 ** 32 - Capacity);
   pragma Compile_Time_Error (Capacity /= 4096 * 2 ** Natural (Allocation_Order),
                              "ADS allocation order/size disagree");
end Intel_GPU_ADS_Backing;
