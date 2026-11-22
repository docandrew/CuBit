package body RTC_Sampling is
   use type RTC_Decode.Outcome;
   use type RTC_Decode.Snapshot;
   package R renames RTC_Decode;

   function Image (Value : Natural) return String is
      Text : constant String := Natural'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   Decimal_Base : constant := 10;
   --  Two decimal digits, zero-filled ("06").
   function Two (Value : Natural) return String is
     ((if Value < Decimal_Base then "0" else "") & Image (Value));

   Hex_Digits : constant String := "0123456789ABCDEF";
   Nibble_Bits : constant := 4;
   Nibble_Mask : constant Unsigned_16 := 16#0F#;
   function Nibble (Value : Unsigned_16; Shift : Natural) return Character is
     (Hex_Digits (Hex_Digits'First + Natural (Shift_Right (Value, Shift) and Nibble_Mask)));
   function Hex (Value : Unsigned_8) return String is
     ("16#" & Nibble (Unsigned_16 (Value), Nibble_Bits) & Nibble (Unsigned_16 (Value), 0) & "#");
   function Hex (Value : Unsigned_16) return String is
     ("16#" & Nibble (Value, 3 * Nibble_Bits) & Nibble (Value, 2 * Nibble_Bits) &
      Nibble (Value, Nibble_Bits) & Nibble (Value, 0) & "#");

   function Date_Text (Date : Civil_Time.Date_Time) return String is
     (Image (Date.Year) & "-" & Two (Date.Month) & "-" & Two (Date.Day) & " " &
      Two (Date.Hour) & ":" & Two (Date.Minute) & ":" & Two (Date.Second));

   function Field_Name (Which : R.Field) return String is
     (case Which is
        when R.Second_Field => "seconds",
        when R.Minute_Field => "minutes",
        when R.Hour_Field => "hours",
        when R.Day_Field => "day",
        when R.Month_Field => "month",
        when R.Year_Field => "year");

   function Mode_Text (Status_B : Unsigned_8) return String is
     ((if R.Is_Binary (Status_B) then "binary" else "BCD") & ", " &
      (if R.Is_24_Hour (Status_B) then "24-hour" else "12-hour"));

   function Registers_Text (Result : Diagnosis) return String is
      S : R.Snapshot renames Result.Registers;
   begin
      if not Result.Have_Snapshot then
         return "no register snapshot";
      end if;
      return "sec=" & Hex (S.Second) & " min=" & Hex (S.Minute) & " hour=" & Hex (S.Hour) &
        " day=" & Hex (S.Day) & " month=" & Hex (S.Month) & " year=" & Hex (S.Year) &
        " B=" & Hex (S.Status_B) & " D=" & Hex (Result.Status_D) & " (" & Mode_Text (S.Status_B) & ")";
   end Registers_Text;

   function Failure_Text (Result : Diagnosis) return String is
      D : R.Result renames Result.Decoding;
   begin
      case Result.Reason is
         when None => return "";
         when Port_Refused =>
            return "CMOS port access refused (" &
              (if Result.Index_Port then "index port " & Hex (Index_Port) else "data port " & Hex (Data_Port)) &
              ", register " & Hex (Result.Register) & ")";
         when Update_Never_Clear =>
            return "update-in-progress (status A bit 7) set on all" & Natural'Image (Maximum_Attempts) &
              " attempts";
         when Snapshots_Unstable =>
            return "no stable snapshot in" & Natural'Image (Maximum_Attempts) & " attempts (update in progress" &
              Natural'Image (Result.Busy) & ", copies differed" & Natural'Image (Result.Mismatched) & ")";
         when Monotonic_Unavailable =>
            return "monotonic clock unavailable";
         when Decode_Failed =>
            case D.Status is
               when R.Decoded => return "";
               when R.Bad_BCD =>
                  return "BCD decode failed: " & Field_Name (D.Failed) & " register " & Hex (D.Raw) &
                    " has a nibble above 9 (status B " & Hex (Result.Registers.Status_B) & ": " &
                    Mode_Text (Result.Registers.Status_B) & ")";
               when R.Bad_12_Hour =>
                  return "12-hour decode failed: hours register " & Hex (D.Raw) &
                    " is not 1..12 after the PM flag (status B " & Hex (Result.Registers.Status_B) & ": " &
                    Mode_Text (Result.Registers.Status_B) & ")";
               when R.Out_Of_Range =>
                  return "out-of-range " & Field_Name (D.Failed) & ": register " & Hex (D.Raw) &
                    " (" & Mode_Text (Result.Registers.Status_B) & ")";
               when R.Invalid_Date =>
                  return "invalid date " & Image (D.Date.Year) & "-" & Two (D.Date.Month) & "-" &
                    Two (D.Date.Day);
            end case;
      end case;
   end Failure_Text;

   procedure Sample
     (Century : RTC_Decode.Century_Base;
      UTC_Seconds, Monotonic_Ms : out Unsigned_64;
      Result : out Diagnosis)
   is
      Port_Failed : Boolean := False;

      function Get (Register : R.CMOS_Register) return Unsigned_8 is
         Value : Unsigned_8;
         OK, Index_Refused : Boolean;
      begin
         if Port_Failed then return 0; end if;
         Read_Register (Register, Value, OK, Index_Refused);
         if not OK then
            Port_Failed := True;
            Result.Reason := Port_Refused;
            Result.Register := Register;
            Result.Index_Port := Index_Refused;
            return 0;
         end if;
         return Value;
      end Get;

      function Updating return Boolean is
        ((Get (R.Status_A_Register) and R.Update_In_Progress) /= 0);

      function Read_Time return R.Snapshot is
         S : R.Snapshot;
      begin
         S.Second := Get (R.Seconds_Register);
         S.Minute := Get (R.Minutes_Register);
         S.Hour := Get (R.Hours_Register);
         S.Day := Get (R.Day_Register);
         S.Month := Get (R.Month_Register);
         S.Year := Get (R.Year_Register);
         S.Status_B := Get (R.Status_B_Register);
         return S;
      end Read_Time;

      First, Second : R.Snapshot;
   begin
      UTC_Seconds := 0;
      Monotonic_Ms := 0;
      Result := (others => <>);
      for Attempt in 1 .. Maximum_Attempts loop
         if Updating then
            exit when Port_Failed;
            Result.Busy := Result.Busy + 1;
         else
            exit when Port_Failed;
            First := Read_Time;
            Second := Read_Time;
            Monotonic_Ms := Monotonic_Milliseconds;
            exit when Port_Failed;
            if First /= Second then
               Result.Mismatched := Result.Mismatched + 1;
            elsif Updating then
               exit when Port_Failed;
               Result.Busy := Result.Busy + 1;
            else
               Result.Registers := First;
               Result.Have_Snapshot := True;
               Result.Status_D := Get (R.Status_D_Register);
               exit when Port_Failed;
               Result.Decoding := R.Decode (First, Century);
               if Result.Decoding.Status /= R.Decoded then
                  Result.Reason := Decode_Failed;
               elsif Monotonic_Ms = Unsigned_64'Last then
                  Result.Reason := Monotonic_Unavailable;
               else
                  UTC_Seconds := Unsigned_64 (Civil_Time.Seconds (Result.Decoding.Date));
               end if;
               return;
            end if;
         end if;
         Pause;
      end loop;
      if not Port_Failed then
         Result.Reason := (if Result.Busy = Maximum_Attempts then Update_Never_Clear else Snapshots_Unstable);
         --  The last copy read, for the log.
         if Result.Mismatched > 0 then
            Result.Registers := First;
            Result.Have_Snapshot := True;
         end if;
      end if;
      Monotonic_Ms := 0;
   end Sample;
end RTC_Sampling;
