------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  MC146818-compatible CMOS RTC registers and the decoding of one stable
--  register snapshot into a civil date (pure; no port I/O).
--
--  Status register B selects the encoding: DM (binary or BCD) and 24/12-hour.
--  In 12-hour mode bit 7 of the hours register is PM, in either encoding.
--  The year register holds two digits. devmgr has no FADT (the century
--  register's CMOS index lives there), so the century is a parameter: the
--  boot seed supplies Default_Century and the clock service's build-time
--  floor rejects a reading from the wrong century.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Civil_Time;

package RTC_Decode with SPARK_Mode, Pure is
   --  CMOS register selectors (written to the index port).
   subtype CMOS_Register is Unsigned_8 range 16#00# .. 16#7F#;
   Seconds_Register  : constant CMOS_Register := 16#00#;
   Minutes_Register  : constant CMOS_Register := 16#02#;
   Hours_Register    : constant CMOS_Register := 16#04#;
   Day_Register      : constant CMOS_Register := 16#07#;
   Month_Register    : constant CMOS_Register := 16#08#;
   Year_Register     : constant CMOS_Register := 16#09#;
   Status_A_Register : constant CMOS_Register := 16#0A#;
   Status_B_Register : constant CMOS_Register := 16#0B#;
   Status_D_Register : constant CMOS_Register := 16#0D#;

   --  Status A: an update cycle is in progress or about to start.
   Update_In_Progress : constant Unsigned_8 := 16#80#;
   --  Status B: data mode binary (else BCD); 24-hour (else 12-hour).
   Binary_Mode : constant Unsigned_8 := 16#04#;
   Hour_24_Mode : constant Unsigned_8 := 16#02#;
   --  Status D: valid RAM and time (clear after power/battery loss).
   Valid_RAM_And_Time : constant Unsigned_8 := 16#80#;
   --  Hours register in 12-hour mode: PM flag; the rest is the hour.
   PM_Flag : constant Unsigned_8 := 16#80#;
   Hour_Mask : constant Unsigned_8 := 16#7F#;

   Years_Per_Century : constant := 100;
   subtype Century_Base is Civil_Time.Year_Number range 2000 .. 2300
     with Dynamic_Predicate => Century_Base mod Years_Per_Century = 0;
   Default_Century : constant Century_Base := 2000;

   type Snapshot is record
      Second, Minute, Hour, Day, Month, Year, Status_B : Unsigned_8 := 0;
   end record;

   type Field is (Second_Field, Minute_Field, Hour_Field, Day_Field, Month_Field, Year_Field);
   type Outcome is
     (Decoded,
      --  A nibble above 9 in BCD mode.
      Bad_BCD,
      --  12-hour mode with an hour outside 1 .. 12.
      Bad_12_Hour,
      --  A decoded field outside its calendar range.
      Out_Of_Range,
      --  Every field in range but no such day (31 April, 29 February...).
      Invalid_Date);

   type Result is record
      Status : Outcome := Out_Of_Range;
      --  The first field that failed (meaningless when Decoded/Invalid_Date).
      Failed : Field := Second_Field;
      --  That field's raw register byte.
      Raw : Unsigned_8 := 0;
      Date : Civil_Time.Date_Time;
   end record;

   function Is_Binary (Status_B : Unsigned_8) return Boolean is
     ((Status_B and Binary_Mode) /= 0);
   function Is_24_Hour (Status_B : Unsigned_8) return Boolean is
     ((Status_B and Hour_24_Mode) /= 0);

   function Decode (Registers : Snapshot; Century : Century_Base) return Result
     with Post => (if Decode'Result.Status = Decoded then Civil_Time.Valid (Decode'Result.Date));
end RTC_Decode;
