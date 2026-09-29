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
