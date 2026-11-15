with Interfaces;package Probe_Model is
 type Test_Mode is (Good,No_Window,Unsafe,No_Clock,No_Deadline,Nonfinite,No_Intel,No_Class,Class_Refused,LGMR_Refused,Wide_Identity,Disabled,Expired,Shed);
 Selected:Test_Mode:=Good;Headers,Reads,Logs,Dropped:Natural:=0;
 Last_Text:String(1..2048):=[others=>' '];Last:Natural:=0;
 Scope:constant Interfaces.Unsigned_64:=16#ABCD_0000_0000_0001#;
 Epoch:constant Interfaces.Unsigned_64:=16#1234_5678_9876_4321#;
 ID:constant Interfaces.Unsigned_64:=16#FEDC_BA98_7654_3210#;
end Probe_Model;