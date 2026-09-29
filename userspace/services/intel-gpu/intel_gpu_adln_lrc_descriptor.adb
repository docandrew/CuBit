package body Intel_GPU_ADLN_LRC_Descriptor with SPARK_Mode is
   function Encode (GPU_Base, Image_Bytes : Unsigned_64;
                    Priority : EU_Priority) return Unsigned_32 is
      -- Valid bit0, force restore bit2, four-level mode3 in bits4:3,
      -- privilege bit8. Gen8-only LLC bit5 is deliberately absent on ADL-N.
      Flags : constant Unsigned_32 := 16#11D#;
      EU_Bits : constant array (EU_Priority) of Unsigned_32 :=
        [Low => 0, Normal => 16#200#, High => 16#400#];
   begin
      if not Admissible (GPU_Base, Image_Bytes) then return 0; end if;
      return Unsigned_32 (GPU_Base) or Flags or EU_Bits (Priority);
   end Encode;
end Intel_GPU_ADLN_LRC_Descriptor;
