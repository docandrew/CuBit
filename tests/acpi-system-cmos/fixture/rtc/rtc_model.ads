with Interfaces; use Interfaces;
package RTC_Model is
 type Scenario is (Valid_Date, Invalid_Date, No_Monotonic, Refuse_First_Index, Refuse_First_Data, Refuse_Late_Index, Busy, Unstable);
 Mode : Scenario := Valid_Date;
 Selected : Unsigned_8 := 0;
 Selects, Reads, Pauses, Logs : Natural := 0;
 procedure Reset (Next_Mode : Scenario);
end RTC_Model;
