with Interfaces;
with Intel_GPU_Plane_Decode;
generic
   -- These bounded callbacks must not raise. Begin holds a display-power
   -- reference and serializes against display reconfiguration until End.
   -- Begin failure must leave no reference for this helper to release.
   with procedure Begin_Access (Success : out Boolean);
   with procedure End_Access (Success : out Boolean);
   -- Index 0..5: CTL, STRIDE, SIZE, OFFSET, SURF, SURFLIVE for one plane.
   -- The native adapter must select a platform-admitted plane and enforce
   -- exactly those read-only register offsets. No arbitrary MMIO is supplied.
   with procedure Read_Field
     (Index : Natural; Value : out Interfaces.Unsigned_32;
      Success : out Boolean);
package Intel_GPU_Plane_Collect is
   type Outcome is (Access_Unavailable, Read_Failed, Access_End_Failed, Collected);
   type Observation is record
      State : Outcome := Access_Unavailable;
      Before, After : Intel_GPU_Plane_Decode.Sample;
      Decoded : Intel_GPU_Plane_Decode.Decoded;
      -- Number of successful nonsentinel reads, at most two six-field sets.
      Reads : Natural range 0 .. 12 := 0;
   end record;
   -- Always calls End exactly once after successful Begin, including read
   -- failure. Decode only follows twelve successful reads and successful End.
   -- Collected is NOT Linear_Ready: inspect Decoded.State independently.
   -- Neither outcome conveys GPU address ownership or safe reclamation.
   procedure Inspect
     (Table_Bytes : Interfaces.Unsigned_64; Result : out Observation);
end Intel_GPU_Plane_Collect;
