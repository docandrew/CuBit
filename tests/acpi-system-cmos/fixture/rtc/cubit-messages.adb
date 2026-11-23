with RTC_Model; use RTC_Model;
package body CuBit.Messages is
 function portOutp8 (ports : CuBit.Handles.IO_Ports; port : Unsigned_16; val : Unsigned_8) return Unsigned_64 is
   use type CuBit.Handles.IO_Ports;
 begin
   if ports /= 1 or port /= 16#70# or (val and 16#80#) /= 0 then raise Program_Error with "RTC select boundary"; end if;
   Selects := Selects + 1;
   if Mode = Refuse_First_Index or else (Mode = Refuse_Late_Index and Selects = 3) then return Unsigned_64'Last; end if;
   Selected := val; return 0;
 end portOutp8;
 function portInp8 (ports : CuBit.Handles.IO_Ports; port : Unsigned_16) return Unsigned_64 is
   use type CuBit.Handles.IO_Ports;
 begin
   if ports /= 1 or port /= 16#71# then raise Program_Error with "RTC data boundary"; end if;
   Reads := Reads + 1;
   if Mode = Refuse_First_Data then return Unsigned_64'Last; end if;
   if Mode = Busy and Selected = 16#A# then return 16#80#; end if;
   case Selected is
     when 16#0# => return (if Mode = Unstable then Unsigned_64 (Reads mod 60) else 0);
     when 16#2# => return 12;
     when 16#4# => return 6;
     when 16#7# => return (if Mode = Invalid_Date then 0 else 19);
     when 16#8# => return 10;
     when 16#9# => return 26;
     when 16#A# => return 0;
     when 16#B# => return 6; -- binary, 24 hour
     when 16#D# => return 16#80#;
     when others => raise Program_Error with "unexpected RTC register";
   end case;
 end portInp8;
 procedure debugPrint (str : String) is
 begin if str'Length = 0 then raise Program_Error; end if; Logs := Logs + 1; end debugPrint;
end CuBit.Messages;
