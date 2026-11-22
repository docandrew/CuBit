package body Intel_GPU_Resources with SPARK_Mode is
   use Intel_GPU_Probe;
   function Plan_Registers
     (Hardware : Platform; Command : Unsigned_16; BAR : BAR_Result;
      Resource_Bytes, Offset, Bytes : Unsigned_64) return Mapping_Plan is
   begin
      if Hardware = Unrecognized then
         return (Status => Unknown_Platform, others => 0);
      elsif (Command and 2) = 0 then
         return (Status => Memory_Decode_Disabled, others => 0);
      elsif BAR.Status not in Memory_32 | Memory_64 or else BAR.Base = 0 then
         return (Status => Invalid_BAR, others => 0);
      elsif BAR.Prefetchable then
         return (Status => Wrong_Cache_Class, others => 0);
      elsif Resource_Bytes = 0 then
         return (Status => Unknown_Extent, others => 0);
      elsif BAR.Base mod Page_Bytes /= 0 or else
        Resource_Bytes mod Page_Bytes /= 0 or else
        Offset mod Page_Bytes /= 0 or else Bytes = 0 or else
        Bytes mod Page_Bytes /= 0
      then
         return (Status => Invalid_Alignment, others => 0);
      elsif Resource_Bytes - 1 > Unsigned_64'Last - BAR.Base then
         return (Status => Address_Overflow, others => 0);
      elsif Offset >= Resource_Bytes or else Bytes > Resource_Bytes - Offset then
         return (Status => Outside_Resource, others => 0);
      else
         pragma Assert (Offset <= Unsigned_64'Last - BAR.Base);
         pragma Assert (Bytes - 1 <= Resource_Bytes - 1 - Offset);
         pragma Assert
           (Resource_Bytes - 1 - Offset <= Unsigned_64'Last - (BAR.Base + Offset));
         return (Admitted, BAR.Base + Offset, Bytes);
      end if;
   end Plan_Registers;
   function Plan_ADLN_Registers
     (Hardware : Platform; Command : Unsigned_16;
      Low, High : Unsigned_32; Offset, Bytes : Unsigned_64) return Mapping_Plan
   is
      BAR : constant BAR_Result := Decode_BAR (Low, High, True);
   begin
      if Hardware /= Alder_Lake_N then
         return (Status => Unknown_Platform, others => 0);
      end if;
      -- Intel datasheet 767626 GTTMMADR offsets 10h/14h: address [38:24],
      -- low hardwired bits 23:0 = 4 (64-bit non-prefetchable memory).
      -- Inspect only: no all-ones sizing write or memory-decode toggle.
      if (Low and 16#00FF_FFFF#) /= 4 or else High > 16#7F# then
         return (Status => Invalid_BAR, others => 0);
      end if;
      return Plan_Registers
        (Hardware, Command, BAR, 16#20_0000#, Offset, Bytes);
   end Plan_ADLN_Registers;
end Intel_GPU_Resources;
