--  Hosted tests for RTC_Decode and RTC_Sampling against a simulated CMOS:
--  BCD/binary and 12/24-hour decoding, every failure path, and the exact
--  log text devmgr prints for each.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Civil_Time;
with RTC_Decode; use RTC_Decode;
with RTC_Sampling;

procedure Main is
   Failures : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Put_Line ("FAIL: " & Name);
         Failures := Failures + 1;
      end if;
   end Check;
   procedure Check_Text (Got, Expected, Name : String) is
   begin
      if Got /= Expected then
         Put_Line ("FAIL: " & Name);
         Put_Line ("  got:      " & Got);
         Put_Line ("  expected: " & Expected);
         Failures := Failures + 1;
      end if;
   end Check_Text;

   BCD_24 : constant Unsigned_8 := Hour_24_Mode;
   Binary_24 : constant Unsigned_8 := Hour_24_Mode or Binary_Mode;
   BCD_12 : constant Unsigned_8 := 0;
   Binary_12 : constant Unsigned_8 := Binary_Mode;
   Valid_D : constant Unsigned_8 := Valid_RAM_And_Time;

   use type Civil_Time.Date_Time;
   function Date (Y, M, D, H, N, S : Natural) return Civil_Time.Date_Time is
     ((Year => Y, Month => M, Day => D, Hour => H, Minute => N, Second => S));

   --  The simulated CMOS: registers, and scripted behaviour.
   type Register_File is array (CMOS_Register) of Unsigned_8;
   CMOS : Register_File := [others => 0];
   Refuse_Index_At, Refuse_Data_At : Integer := -1;
   Busy_Reads : Natural := 0;           --  status A reads that report UIP
   Tick_Every_Read : Boolean := False;  --  seconds change on every read
   Monotonic_Value : Unsigned_64 := 1_234;
   Pauses : Natural := 0;

   procedure Read_Register
     (Register : CMOS_Register; Value : out Unsigned_8; OK : out Boolean; Index_Refused : out Boolean) is
   begin
      Value := 0;
      Index_Refused := Integer (Register) = Refuse_Index_At;
      OK := not Index_Refused and then Integer (Register) /= Refuse_Data_At;
      if not OK then return; end if;
      if Register = Status_A_Register and then Busy_Reads > 0 then
         Busy_Reads := Busy_Reads - 1;
         Value := Update_In_Progress;
         return;
      end if;
      if Register = Seconds_Register and then Tick_Every_Read then
         CMOS (Seconds_Register) := (CMOS (Seconds_Register) + 1) mod 16#50#;
      end if;
      Value := CMOS (Register);
   end Read_Register;
   procedure Pause is begin Pauses := Pauses + 1; end Pause;
   function Monotonic return Unsigned_64 is (Monotonic_Value);

   package Sampling is new RTC_Sampling
     (Index_Port => 16#70#, Data_Port => 16#71#, Read_Register => Read_Register,
      Pause => Pause, Monotonic_Milliseconds => Monotonic);
   use type Sampling.Failure;

   procedure Reset (B : Unsigned_8; Sec, Min, Hour, Day, Month, Year : Unsigned_8) is
   begin
      CMOS := [others => 0];
      CMOS (Seconds_Register) := Sec;
      CMOS (Minutes_Register) := Min;
      CMOS (Hours_Register) := Hour;
      CMOS (Day_Register) := Day;
      CMOS (Month_Register) := Month;
      CMOS (Year_Register) := Year;
      CMOS (Status_B_Register) := B;
      CMOS (Status_D_Register) := Valid_D;
      Refuse_Index_At := -1; Refuse_Data_At := -1;
      Busy_Reads := 0; Tick_Every_Read := False; Monotonic_Value := 1_234; Pauses := 0;
   end Reset;

   UTC, Mono : Unsigned_64;
   R : Sampling.Diagnosis;
   D : RTC_Decode.Result;
begin
   --  Decoding.
   D := Decode ((16#45#, 16#30#, 16#18#, 16#19#, 16#10#, 16#26#, BCD_24), Default_Century);
   Check (D.Status = Decoded and then D.Date = Date (2026, 10, 19, 18, 30, 45), "BCD 24-hour");
   D := Decode ((45, 30, 18, 19, 10, 26, Binary_24), Default_Century);
   Check (D.Status = Decoded and then D.Date = Date (2026, 10, 19, 18, 30, 45), "binary 24-hour");
   D := Decode ((16#00#, 16#00#, 16#12#, 16#01#, 16#01#, 16#26#, BCD_12), Default_Century);
   Check (D.Status = Decoded and then D.Date.Hour = 0, "BCD 12 AM is midnight");
   D := Decode ((16#00#, 16#00#, 16#92#, 16#01#, 16#01#, 16#26#, BCD_12), Default_Century);
   Check (D.Status = Decoded and then D.Date.Hour = 12, "BCD 12 PM is noon");
   D := Decode ((0, 0, PM_Flag or 11, 1, 1, 26, Binary_12), Default_Century);
   Check (D.Status = Decoded and then D.Date.Hour = 23, "binary 11 PM");
   D := Decode ((0, 0, 16#81#, 1, 1, 26, Binary_24), Default_Century);
   Check (D.Status = Out_Of_Range and then D.Failed = Hour_Field, "24-hour mode keeps bit 7 (out of range)");
   D := Decode ((16#5A#, 0, 0, 1, 1, 0, BCD_24), Default_Century);
   Check (D.Status = Bad_BCD and then D.Failed = Second_Field and then D.Raw = 16#5A#, "bad BCD seconds");
   D := Decode ((0, 0, 16#13#, 1, 1, 0, BCD_12), Default_Century);
   Check (D.Status = Bad_12_Hour and then D.Raw = 16#13#, "12-hour 13");
   D := Decode ((0, 0, 16#00#, 1, 1, 0, BCD_12), Default_Century);
   Check (D.Status = Bad_12_Hour, "12-hour 0");
   D := Decode ((16#60#, 0, 0, 1, 1, 0, BCD_24), Default_Century);
   Check (D.Status = Out_Of_Range and then D.Failed = Second_Field, "second 60");
   D := Decode ((0, 0, 0, 1, 16#13#, 0, BCD_24), Default_Century);
   Check (D.Status = Out_Of_Range and then D.Failed = Month_Field, "month 13");
   D := Decode ((0, 0, 0, 0, 1, 0, BCD_24), Default_Century);
   Check (D.Status = Out_Of_Range and then D.Failed = Day_Field, "day 0");
   D := Decode ((0, 0, 0, 1, 1, 100, Binary_24), Default_Century);
   Check (D.Status = Out_Of_Range and then D.Failed = Year_Field, "binary year 100");
   D := Decode ((0, 0, 0, 16#30#, 16#02#, 16#26#, BCD_24), Default_Century);
   Check (D.Status = Invalid_Date, "30 February");
   D := Decode ((0, 0, 0, 16#29#, 16#02#, 16#24#, BCD_24), Default_Century);
   Check (D.Status = Decoded, "29 February 2024");
   D := Decode ((0, 0, 0, 1, 1, 16#99#, BCD_24), 2100);
   Check (D.Status = Decoded and then D.Date.Year = 2199, "century parameter");

   --  Exhaustive: every BCD byte for each field decodes or names that field.
   for Raw in Unsigned_8 loop
      D := Decode ((Raw, 0, 0, 1, 1, 0, BCD_24), Default_Century);
      Check (D.Status = Decoded or else D.Failed = Second_Field, "seconds failure names seconds");
      D := Decode ((0, 0, Raw, 1, 1, 0, BCD_12), Default_Century);
      Check (D.Status = Decoded or else D.Failed = Hour_Field, "12-hour failure names hours");
      Check ((D.Status = Decoded) = ((Raw and Hour_Mask) in 16#01# .. 16#09# | 16#10# .. 16#12#),
             "12-hour decodes exactly BCD 1..12 with or without PM");
   end loop;

   --  Sampling: success, and every failure path with its exact text.
   Reset (BCD_24, 16#45#, 16#30#, 16#18#, 16#19#, 16#10#, 16#26#);
   Sampling.Sample (Default_Century, UTC, Mono, R);
   Check (R.Reason = Sampling.None and then Mono = 1_234, "sample succeeds");
   Check (UTC = Unsigned_64 (Civil_Time.Seconds (Date (2026, 10, 19, 18, 30, 45))), "sample UTC");
   Check_Text (Sampling.Registers_Text (R),
     "sec=16#45# min=16#30# hour=16#18# day=16#19# month=16#10# year=16#26# B=16#02# D=16#80# (BCD, 24-hour)",
     "registers text");
   Check_Text (Sampling.Date_Text (R.Decoding.Date), "2026-10-19 18:30:45", "date text");
   Check (not Sampling.VRT_Clear (R), "VRT set");

   Reset (BCD_24, 16#45#, 16#30#, 16#18#, 16#19#, 16#10#, 16#26#);
   CMOS (Status_D_Register) := 0;
   Sampling.Sample (Default_Century, UTC, Mono, R);
   Check (R.Reason = Sampling.None and then Sampling.VRT_Clear (R), "VRT clear is a warning only");

   Reset (BCD_24, 16#45#, 16#30#, 16#18#, 16#19#, 16#10#, 16#26#);
   Busy_Reads := 3;
   Sampling.Sample (Default_Century, UTC, Mono, R);
   Check (R.Reason = Sampling.None and then R.Busy = 3 and then Pauses = 3, "waits out UIP");

   Reset (BCD_24, 0, 0, 0, 1, 1, 0);
   Busy_Reads := Natural'Last;
   Sampling.Sample (Default_Century, UTC, Mono, R);
   Check (R.Reason = Sampling.Update_Never_Clear and then UTC = 0 and then Mono = 0, "UIP stuck");
   Check_Text (Sampling.Failure_Text (R), "update-in-progress (status A bit 7) set on all 20 attempts",
     "UIP text");
   Check_Text (Sampling.Registers_Text (R), "no register snapshot", "no snapshot text");

   Reset (BCD_24, 0, 0, 0, 1, 1, 0);
   Tick_Every_Read := True;
   Sampling.Sample (Default_Century, UTC, Mono, R);
   Check (R.Reason = Sampling.Snapshots_Unstable and then R.Mismatched = 20, "unstable");
   Check_Text (Sampling.Failure_Text (R),
     "no stable snapshot in 20 attempts (update in progress 0, copies differed 20)", "unstable text");

   Reset (BCD_24, 0, 0, 0, 1, 1, 0);
   Refuse_Index_At := Integer (Status_A_Register);
   Sampling.Sample (Default_Century, UTC, Mono, R);
   Check (R.Reason = Sampling.Port_Refused and then Pauses = 0, "index port refused");
   Check_Text (Sampling.Failure_Text (R),
     "CMOS port access refused (index port 16#0070#, register 16#0A#)", "index refused text");

   Reset (BCD_24, 0, 0, 0, 1, 1, 0);
   Refuse_Data_At := Integer (Year_Register);
   Sampling.Sample (Default_Century, UTC, Mono, R);
   Check (R.Reason = Sampling.Port_Refused and then not R.Index_Port, "data port refused");
   Check_Text (Sampling.Failure_Text (R),
     "CMOS port access refused (data port 16#0071#, register 16#09#)", "data refused text");

   Reset (BCD_24, 0, 0, 0, 1, 1, 0);
   Refuse_Data_At := Integer (Status_D_Register);
   Sampling.Sample (Default_Century, UTC, Mono, R);
   Check (R.Reason = Sampling.Port_Refused and then R.Register = Status_D_Register, "status D refused");

   Reset (BCD_24, 16#5A#, 0, 0, 1, 1, 0);
   Sampling.Sample (Default_Century, UTC, Mono, R);
   Check (R.Reason = Sampling.Decode_Failed and then UTC = 0, "decode failure");
   Check_Text (Sampling.Failure_Text (R),
     "BCD decode failed: seconds register 16#5A# has a nibble above 9 (status B 16#02#: BCD, 24-hour)",
     "BCD text");

   Reset (BCD_12, 0, 0, 16#13#, 1, 1, 0);
   Sampling.Sample (Default_Century, UTC, Mono, R);
   Check_Text (Sampling.Failure_Text (R),
     "12-hour decode failed: hours register 16#13# is not 1..12 after the PM flag (status B 16#00#: BCD, 12-hour)",
     "12-hour text");

   Reset (Binary_24, 0, 0, 0, 1, 13, 26);
   Sampling.Sample (Default_Century, UTC, Mono, R);
   Check_Text (Sampling.Failure_Text (R), "out-of-range month: register 16#0D# (binary, 24-hour)",
     "range text");

   Reset (BCD_24, 0, 0, 0, 16#31#, 16#04#, 16#26#);
   Sampling.Sample (Default_Century, UTC, Mono, R);
   Check_Text (Sampling.Failure_Text (R), "invalid date 2026-04-31", "invalid date text");

   Reset (BCD_24, 0, 0, 0, 1, 1, 16#26#);
   Monotonic_Value := Unsigned_64'Last;
   Sampling.Sample (Default_Century, UTC, Mono, R);
   Check (R.Reason = Sampling.Monotonic_Unavailable and then UTC = 0, "monotonic unavailable");
   Check_Text (Sampling.Failure_Text (R), "monotonic clock unavailable", "monotonic text");

   if Failures = 0 then
      Put_Line ("boot-rtc: PASS");
   else
      Put_Line ("boot-rtc: FAIL" & Failures'Image);
      raise Program_Error;
   end if;
end Main;
