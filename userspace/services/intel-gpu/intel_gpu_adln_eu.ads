with Interfaces; use Interfaces;
package Intel_GPU_ADLN_EU with SPARK_Mode is
   EU_Disable_Register : constant Unsigned_32 := 16#9134#;
   type Topology is record
      Valid : Boolean := False;
      DSS_Mask : Unsigned_8 := 0;
      EU_Mask : Unsigned_16 := 0;
      Total_EUs : Natural range 0 .. 96 := 0;
   end record;
   -- ADL-N identity and owned, forcewake-held MMIO are caller obligations.
   -- One slice, at most six DSS, sixteen EUs per DSS; fuse bits disable pairs.
   -- This is not Mesa's offline PCI-table default topology.
   function Decode (Slice_Enable, DSS_Enable, EU_Disable : Unsigned_32)
     return Topology
     with Post => (if Decode'Result.Valid then
       Decode'Result.DSS_Mask /= 0 and Decode'Result.EU_Mask /= 0 and
       Decode'Result.Total_EUs > 0);
end Intel_GPU_ADLN_EU;
