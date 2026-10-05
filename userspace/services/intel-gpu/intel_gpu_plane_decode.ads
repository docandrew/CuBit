with Interfaces;
with Intel_GPU_Scanout_Range;
package Intel_GPU_Plane_Decode with SPARK_Mode is
   use Interfaces;
   -- ADL-N universal plane register values, not MMIO offsets. The collector
   -- must establish platform identity, display power and exclusive ownership.
   -- Matching samples detect some changes; they are NOT an atomic snapshot.
   type Sample is record
      Control, Stride, Size, Offset, Surface, Live_Surface : Unsigned_32 := 0;
   end record;
   type Status is (Invalid_Read, Changing, Disabled, Unsupported,
                   Invalid_Geometry, Linear_Ready);
   type Decoded is record
      State : Status := Invalid_Read;
      Memory : Intel_GPU_Scanout_Range.Extent;
   end record;
   type Flip_Plan is record
      Valid : Boolean := False;
      Surface_Word : Unsigned_32 := 0;
      Memory : Intel_GPU_Scanout_Range.Extent;
   end record;
   -- Prospective same-geometry synchronous MMIO flip, NOT a modeset or write.
   -- Target is a GGTT byte range, never a CPU/physical/PPGTT address. Require
   -- stable supported current state and a disjoint complete target footprint.
   -- Caller must still establish ADL-N identity, exclusive scanout authority,
   -- power, GGTT mapping/visibility, backing ownership, no other plane/cursor
   -- aliases, unchanged geometry and global update controls, producer readiness
   -- and flip/latch/old-reader retirement. Valid is geometric eligibility only.
   -- PRM Vol2c pp840-841: SURF[31:12] is GraphicsAddress; a SURF write arms
   -- double-buffered updates. It is NOT evidence those updates have latched.
   function Plan_Linear_Flip
     (Before, After : Sample; Table_Bytes, Target_First, Target_Bytes : Unsigned_64)
      return Flip_Plan
   with Global => null,
     Post => Plan_Linear_Flip'Result.Valid = Plan_Linear_Flip'Result.Memory.Valid
       and then (if Plan_Linear_Flip'Result.Valid then
         Plan_Linear_Flip'Result.Memory.First = Target_First and then
         Plan_Linear_Flip'Result.Memory.Bytes <= Target_Bytes and then
         Unsigned_64 (Plan_Linear_Flip'Result.Surface_Word) = Target_First);
   -- Initially supports unrotated, uncompressed, linear 32-bit RGB only.
   -- Reject unrecognized control bits and a pending/live address mismatch:
   -- current dimensions cannot safely describe an older live surface.
   -- Neither Disabled nor any rejection permits reclamation/admission.
   function Decode
     (Before, After : Sample; Table_Bytes : Unsigned_64) return Decoded
   with Global => null,
     Post => (Decode'Result.Memory.Valid =
                (Decode'Result.State = Linear_Ready)) and then
       (if Decode'Result.State = Linear_Ready then
          Before = After and then
          (Before.Surface and 16#FFFF_F000#) = Before.Live_Surface and then
          Before.Control in 16#8400_0000# | 16#8410_0000# |
            16#8400_0008# | 16#8410_0008# and then
          Decode'Result.Memory.First = Unsigned_64 (Before.Live_Surface) and then
          Decode'Result.Memory.Bytes > 0 and then
          Decode'Result.Memory.Bytes mod 4096 = 0 and then
          Decode'Result.Memory.First < Table_Bytes / 8 * 4096 and then
          Decode'Result.Memory.Bytes <=
            Table_Bytes / 8 * 4096 - Decode'Result.Memory.First);
end Intel_GPU_Plane_Decode;
