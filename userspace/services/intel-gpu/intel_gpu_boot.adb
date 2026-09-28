with Intel_GPU_Probe; use Intel_GPU_Probe;
package body Intel_GPU_Boot with SPARK_Mode is
   use Intel_GPU_Resources;
   function Decode (Data : Words) return Mapping_Plan is
      Family : constant Platform := Identify
        (Unsigned_16 (Data (1) and 16#FFFF#),
         Unsigned_16 (Shift_Right (Data (1), 16) and 16#FFFF#),
         Unsigned_8 (Shift_Right (Data (1), 40) and 16#FF#));
   begin
      if (Data (3) and 16#FFFF#) /= Protocol_Version or else
        Shift_Right (Data (3), 32) /= 0 or else
        Data (2) not in 16#1_0000# .. 16#1_FFFF# or else
        Shift_Right (Data (1), 56) /= 0 or else
        (Shift_Right (Data (1), 48) and 16#7F#) /= 0
      then
         return (Invalid_BAR, 0, 0);
      end if;
      return Plan_ADLN_Registers
        (Family, Unsigned_16 (Data (2) and 16#FFFF#),
         Unsigned_32 (Data (0) and 16#FFFF_FFFF#),
         Unsigned_32 (Shift_Right (Data (0), 32)), 0, 16#20_0000#);
   end Decode;
end Intel_GPU_Boot;
