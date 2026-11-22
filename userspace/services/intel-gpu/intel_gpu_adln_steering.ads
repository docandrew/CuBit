with Interfaces; use Interfaces;
package Intel_GPU_ADLN_Steering with SPARK_Mode is
   Slice_Register : constant Unsigned_32 := 16#9138#;
   DSS_Register : constant Unsigned_32 := 16#913C#;
   L3_Register : constant Unsigned_32 := 16#9118#;
   type Topology is record
      Valid : Boolean := False;
      DSS_Mask, L3_Mask : Unsigned_8 := 0;
      Default_Instance : Natural range 0 .. 5 := 0;
      L3_Instance : Natural range 0 .. 3 := 0;
      Separate_L3 : Boolean := False;
   end record;
   -- Caller must establish ADL-N identity, stable reads, forcewake and ownership.
   -- Reserved bits are ignored, but all-ones reads fail closed.
   function Decode (Slice_Enable, DSS_Enable, L3_Disable : Unsigned_32)
     return Topology;
   type Fuse_Snapshot is record
      Slice_Enable, DSS_Enable, L3_Disable : Unsigned_32 := Unsigned_32'Last;
   end record;
   function Decode_Stable (First, Second : Fuse_Snapshot) return Topology
     with Post => (if First /= Second then not Decode_Stable'Result.Valid);
   -- Equality of two samples detects observed changes, not atomicity or
   -- ownership. Caller must retain forcewake across both complete samples.
   function Instance (Value : Topology; Offset : Unsigned_32) return Natural
     with Pre => Value.Valid,
          Post => Instance'Result <= 5;
   -- Group is always zero on this platform. Only the L3BANK range needs an
   -- override; this is selection data, not a write to MCR or a read grant.
end Intel_GPU_ADLN_Steering;
