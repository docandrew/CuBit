with CuBit.Messages; use CuBit.Messages;
with Interfaces; use Interfaces;
with RTC_Decode;
with RTC_Sampling;
package body Boot_RTC is
   CMOS_Index : constant Unsigned_16 := 16#70#;
   CMOS_Data  : constant Unsigned_16 := 16#71#;
   --  Longer than one RTC update cycle (under 2 ms).
   Update_Pause_Ms : constant Unsigned_64 := 2;
   Largest_Byte : constant Unsigned_64 := Unsigned_64 (Unsigned_8'Last);
   Port_Refused : constant Unsigned_64 := Unsigned_64'Last;
   LF : Character renames ASCII.LF;
   function Image (Value : Natural) return String is
      Text : constant String := Natural'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   procedure Read (Ports : CuBit.Handles.IO_Ports;
                   UTC_Seconds, Monotonic_Ms : out Unsigned_64;
                   Valid : out Boolean) is
      procedure Read_Register
        (Register : RTC_Decode.CMOS_Register; Value : out Unsigned_8;
         OK : out Boolean; Index_Refused : out Boolean)
      is
         Result : Unsigned_64;
      begin
         Value := 0;
         --  Kernel boot CMOS users have finished. Match its NMI-enabled
         --  selector convention; no competing runtime CMOS owner is admitted.
         Result := portOutp8 (Ports, CMOS_Index, Register);
         Index_Refused := Result = Port_Refused;
         if Index_Refused then OK := False; return; end if;
         Result := portInp8 (Ports, CMOS_Data);
         OK := Result <= Largest_Byte;
         if OK then Value := Unsigned_8 (Result); end if;
      end Read_Register;
      procedure Pause is
         Ignore : constant Unsigned_64 := syscall (SYSCALL_SLEEP, Update_Pause_Ms);
      begin null; end Pause;
      function Monotonic return Unsigned_64 is (syscall (SYSCALL_GETTIME));
      package Sampling is new RTC_Sampling
        (Index_Port => CMOS_Index, Data_Port => CMOS_Data, Read_Register => Read_Register,
         Pause => Pause, Monotonic_Milliseconds => Monotonic);
      use type Sampling.Failure;
      Result : Sampling.Diagnosis;
   begin
      Sampling.Sample (RTC_Decode.Default_Century, UTC_Seconds, Monotonic_Ms, Result);
      Valid := Result.Reason = Sampling.None;
      debugPrint ("devmgr: RTC registers " & Sampling.Registers_Text (Result) & LF);
      --  devmgr has no FADT, whose century field names the CMOS century
      --  register; the clock service's build-time floor rejects a wrong century.
      debugPrint ("devmgr: RTC century register not read (no FADT century index in devmgr); assuming " &
                  Image (RTC_Decode.Default_Century) & ".." &
                  Image (RTC_Decode.Default_Century + RTC_Decode.Years_Per_Century - 1) & LF);
      if Sampling.VRT_Clear (Result) then
         debugPrint ("devmgr: RTC status D VRT clear (RTC power or battery lost?); " &
                     "reading anyway, the clock floor decides" & LF);
      end if;
      if Valid then
         debugPrint ("devmgr: RTC reads " & Sampling.Date_Text (Result.Decoding.Date) & " (taken as UTC)" & LF);
      else
         debugPrint ("devmgr: RTC seed unavailable: " & Sampling.Failure_Text (Result) & LF);
      end if;
   end Read;
end Boot_RTC;
