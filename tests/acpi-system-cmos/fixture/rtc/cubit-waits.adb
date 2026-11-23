with RTC_Model; package body CuBit.Waits is
 function Sleep_For (Milliseconds : Interfaces.Unsigned_64) return Interfaces.Unsigned_64 is
 use type Interfaces.Unsigned_64;
 begin if Milliseconds /= 2 then raise Program_Error; end if; RTC_Model.Pauses := RTC_Model.Pauses + 1; return 0; end Sleep_For;
end CuBit.Waits;
