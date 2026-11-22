------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Taking one stable CMOS RTC snapshot, and saying exactly why not.
--
--  Generic over register access, a short pause and the monotonic clock, so
--  the retry and diagnosis logic runs against a simulated CMOS on the host
--  (tests/boot-rtc) and against the real ports in devmgr (Boot_RTC).
--
--  Algorithm: wait for status A's update-in-progress flag to read clear,
--  read every time register twice, and accept the first attempt where both
--  copies agree and the flag is still clear afterwards (an update cannot have
--  happened in between). Bounded: Maximum_Attempts pauses at most. Status D's
--  VRT flag is reported but not fatal (some chipsets do not implement it;
--  battery loss resets the date, which the clock service's build-time floor
--  rejects). Every failure carries the check that failed.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Civil_Time;
with RTC_Decode;

generic
   --  The CMOS index and data ports (for the log text).
   Index_Port, Data_Port : Unsigned_16;
   --  Read one CMOS register; OK is False when the port access was refused.
   --  Index_Refused tells which of the two port accesses failed.
   with procedure Read_Register
     (Register : RTC_Decode.CMOS_Register; Value : out Unsigned_8;
      OK : out Boolean; Index_Refused : out Boolean);
   --  A short bounded wait (milliseconds), longer than one RTC update cycle.
   with procedure Pause;
   --  The kernel monotonic clock in milliseconds; Unsigned_64'Last if unavailable.
   with function Monotonic_Milliseconds return Unsigned_64;
package RTC_Sampling is
   Maximum_Attempts : constant := 20;
   subtype Attempt_Count is Natural range 0 .. Maximum_Attempts;

   type Failure is
     (None,
      --  Writing the CMOS index port or reading the data port was refused
      --  (the I/O-port authority does not cover it).
      Port_Refused,
      --  Status A's update-in-progress flag never read clear.
      Update_Never_Clear,
      --  No two consecutive reads agreed (or an update started in between).
      Snapshots_Unstable,
      --  A stable snapshot did not decode (RTC_Decode.Outcome says how).
      Decode_Failed,
      Monotonic_Unavailable);

   type Diagnosis is record
      Reason : Failure := None;
      --  Port_Refused: the register being read, and which port.
      Register : RTC_Decode.CMOS_Register := 0;
      Index_Port : Boolean := False;
      --  The last stable (or last attempted) snapshot and status D.
      Registers : RTC_Decode.Snapshot;
      Status_D : Unsigned_8 := 0;
      Have_Snapshot : Boolean := False;
      Decoding : RTC_Decode.Result;
      --  Attempts that found update-in-progress set, and attempts whose two
      --  copies differed.
      Busy, Mismatched : Attempt_Count := 0;
   end record;

   --  On success Reason = None, Decoding.Status = Decoded and UTC_Seconds is
   --  the snapshot read as UTC (the hardware must hold UTC).
   procedure Sample
     (Century : RTC_Decode.Century_Base;
      UTC_Seconds, Monotonic_Ms : out Unsigned_64;
      Result : out Diagnosis);

   --  Log text (no prefix, no line end) for Result: the failure, the raw
   --  registers, and the VRT warning.
   function Failure_Text (Result : Diagnosis) return String
     with Pre => Result.Reason /= None;
   function Registers_Text (Result : Diagnosis) return String;
   --  "2026-10-19 06:12:00".
   function Date_Text (Date : Civil_Time.Date_Time) return String;
   function VRT_Clear (Result : Diagnosis) return Boolean is
     (Result.Have_Snapshot and then (Result.Status_D and RTC_Decode.Valid_RAM_And_Time) = 0);
end RTC_Sampling;
