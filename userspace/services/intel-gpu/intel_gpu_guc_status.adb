package body Intel_GPU_GuC_Status with SPARK_Mode is
   use Interfaces;
   function Decode (Value : Unsigned_32) return State is
      Bootrom : constant Unsigned_32 := Shift_Right (Value, 1) and 16#7F#;
      Firmware : constant Unsigned_32 := Shift_Right (Value, 8) and 255;
      Authentication : constant Unsigned_32 := Shift_Right (Value, 30);
   begin
      if Value = Unsigned_32'Last then return Invalid_MMIO; end if;
      if Authentication = 1 or Bootrom in 16#13# | 16#2B# | 16#50# then
         return Authentication_Failed;
      end if;
      if Bootrom in 16#73# .. 16#75# | 16#77# | 16#79# | 16#7A# | 16#7E#
      then return Bootrom_Failed; end if;
      if Firmware in 2 .. 4 | 7 | 16#60# | 16#70# | 16#71# | 16#73# .. 16#75#
      then return Firmware_Failed; end if;
      if Firmware = 16#F0# and Authentication = 2 and (Value and 1) = 0
      then return Ready; end if;
      return Pending;
   end Decode;
end Intel_GPU_GuC_Status;
