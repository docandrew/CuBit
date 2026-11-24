with Ada.Text_IO; with Interfaces; with Boot_RTC; with RTC_Model; use RTC_Model;
procedure CMOS_Boot_RTC_Tests is
 use type Interfaces.Unsigned_64;
 Checks : Natural := 0;
 UTC, Mono : Interfaces.Unsigned_64; Good : Boolean; History : Boot_RTC.Handover_History;
 procedure Check (OK : Boolean) is begin Checks := Checks + 1; if not OK then raise Program_Error with "RTC history" & Mode'Image & Checks'Image; end if; end Check;
begin
 for M in Scenario loop
   Reset (M); History := (True,True,True);
   Boot_RTC.Read (1,UTC,Mono,Good,History);
   Check (Good = (M = Valid_Date));
   Check (History.Selector_Completed = (M /= Refuse_First_Index));
   Check (not History.NMI_Disabled);
   Check (History.Legacy_Validated = (M in Valid_Date | Invalid_Date | No_Monotonic));
   Check (Selects <= 500 and Pauses <= 20 and Logs in 3 .. 4);
   if M = Valid_Date then Check (UTC /= 0 and Mono = 100); end if;
   if M = Refuse_First_Index then Check (Reads = 0); end if;
 end loop;
 Ada.Text_IO.Put_Line ("CMOS boot RTC history checks" & Checks'Image & " PASS");
end CMOS_Boot_RTC_Tests;
