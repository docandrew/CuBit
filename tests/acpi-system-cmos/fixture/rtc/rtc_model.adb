package body RTC_Model is
 procedure Reset (Next_Mode : Scenario) is
 begin Mode := Next_Mode; Selected := 0; Selects := 0; Reads := 0; Pauses := 0; Logs := 0; end Reset;
end RTC_Model;
