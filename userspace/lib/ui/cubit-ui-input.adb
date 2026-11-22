package body CuBit.UI.Input is
   function Pointer_Wheel_Delta (event : Input_Event) return Integer is
      raw : constant Unsigned_32 :=
        Unsigned_32 (event.payload1 and 16#FFFF_FFFF#);
      negativeMagnitude : Unsigned_64;
   begin
      if raw <= Unsigned_32 (Integer'Last) then
         return Integer (raw);
      elsif raw = 16#8000_0000# then
         return Integer'First;
      end if;
      negativeMagnitude := 16#1_0000_0000# - Unsigned_64 (raw);
      return -Integer (negativeMagnitude);
   end Pointer_Wheel_Delta;
end CuBit.UI.Input;
