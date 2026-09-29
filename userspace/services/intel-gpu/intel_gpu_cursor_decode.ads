with Interfaces;
with Intel_GPU_Scanout_Range;
package Intel_GPU_Cursor_Decode with SPARK_Mode is
   use Interfaces;
   type Sample is record
      Control, Base, Live_Base, FBC_Control : Unsigned_32 := 0;
   end record;
   type Status is (Invalid_Read, Changing, Disabled, Unsupported,
                   Invalid_Geometry, Ready);
   type Decoded is record
      State : Status := Invalid_Read;
      Memory : Intel_GPU_Scanout_Range.Extent;
   end record;
   -- ADL-N new-style cursor control, not legacy bit31 enable semantics.
   -- Initially only square, unrotated ARGB modes with no extra control bits.
   -- Matching reads are not an atomic snapshot or evidence of ownership.
   -- Disabled/rejected samples never authorize reclamation of old backing.
   function Decode (Before, After : Sample; Table_Bytes : Unsigned_64)
     return Decoded
   with Global => null,
     Post => (Decode'Result.Memory.Valid = (Decode'Result.State = Ready))
       and then (if Decode'Result.State = Ready then
         Before = After and then Before.Base = Before.Live_Base and then
         Before.Control in 16#22# | 16#23# | 16#27# and then
         Before.FBC_Control = 0 and then
         Decode'Result.Memory.First = Unsigned_64 (Before.Base) and then
         Decode'Result.Memory.Bytes in 16_384 | 65_536 | 262_144);
end Intel_GPU_Cursor_Decode;
