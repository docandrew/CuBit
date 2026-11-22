with Interfaces;
generic
   -- Ordered, bounded, nonraising callbacks. Caller owns reset/forcewake and
   -- excludes other writers. A failed write may still have reached hardware.
   with function Read32 (Offset : Interfaces.Unsigned_32)
     return Interfaces.Unsigned_32;
   with procedure Write32 (Offset, Value : Interfaces.Unsigned_32;
                           Success : out Boolean);
package Intel_GPU_WOPCM is
   type Phase is (Fresh, Consumed, Quarantined, Configured);
   type Attempt is limited private;
   function Current (Object : Attempt) return Phase;
   type Result is (Rejected, Invalid_MMIO, Locked_Conflict,
                   Write_Failed, Readback_Failed, Complete);
   -- GuC-only bring-up; HuC agent remains disabled. Capacity is authenticated
   -- platform knowledge, NOT inferred from these registers. Base/size and
   -- CSS+code length must pass the shared ADL-N layout admission.
   -- Reuses only a fully locked matching pair; partial locks fail without writes.
   procedure Configure
     (Object : in out Attempt;
      Capacity, Base, Size, Upload_Bytes : Interfaces.Unsigned_64;
      Status : out Result);
private
   type Attempt is limited record
      Value : Phase := Fresh;
   end record;
end Intel_GPU_WOPCM;
