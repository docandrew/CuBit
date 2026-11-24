with RTC_Model; package body CuBit.Monotonic is
 use type RTC_Model.Scenario;
 function Milliseconds return Interfaces.Unsigned_64 is
 begin return (if RTC_Model.Mode = RTC_Model.No_Monotonic then Interfaces.Unsigned_64'Last else 100); end Milliseconds;
end CuBit.Monotonic;
