with Intel_GPU_Firmware;
package body Intel_GPU_WOPCM is
   use Interfaces;
   Size_Reg : constant Unsigned_32 := 16#C050#;
   Base_Reg : constant Unsigned_32 := 16#C340#;
   Size_Mask : constant Unsigned_32 := 16#FFFFF001#;
   Base_Mask : constant Unsigned_32 := 16#FFFFC003#;
   function Current (Object : Attempt) return Phase is (Object.Value);
   procedure Configure
     (Object : in out Attempt;
      Capacity, Base, Size, Upload_Bytes : Unsigned_64;
      Status : out Result)
   is
      Old_Size, Old_Base, Expected_Size, Expected_Base, Value : Unsigned_32;
      OK : Boolean;
   begin
      Status := Rejected;
      if Object.Value /= Fresh then return; end if;
      Object.Value := Consumed;
      if not Intel_GPU_Firmware.Fits_ADLN_WOPCM
        (Capacity, Base, Size, Upload_Bytes, 0) then return; end if;
      Expected_Size := Unsigned_32 (Size) or 1;
      Expected_Base := Unsigned_32 (Base) or 1;
      Old_Size := Read32 (Size_Reg);
      Old_Base := Read32 (Base_Reg);
      Status := Invalid_MMIO;
      if Old_Size = Unsigned_32'Last or Old_Base = Unsigned_32'Last then return; end if;
      if ((Old_Size or Old_Base) and 1) /= 0 then
         Status := Locked_Conflict;
         if (Old_Size and Size_Mask) /= Expected_Size or else
           (Old_Base and Base_Mask) /= Expected_Base then return; end if;
      else
         Object.Value := Quarantined;
         -- Writing size/base causes hardware to assert the corresponding lock
         -- bits. Do not set the lock bits in the programming values ourselves.
         Write32 (Size_Reg, Unsigned_32 (Size), OK);
         Status := Write_Failed;
         if not OK then return; end if;
         Value := Read32 (Size_Reg);
         Status := Readback_Failed;
         if Value = Unsigned_32'Last or else
           (Value and Size_Mask) /= Expected_Size then return; end if;
         Write32 (Base_Reg, Unsigned_32 (Base), OK);
         Status := Write_Failed;
         if not OK then return; end if;
         Value := Read32 (Base_Reg);
         Status := Readback_Failed;
         if Value = Unsigned_32'Last or else
           (Value and Base_Mask) /= Expected_Base then return; end if;
      end if;
      Object.Value := Configured;
      Status := Complete;
   end Configure;
end Intel_GPU_WOPCM;
