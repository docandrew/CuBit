package body RTC_Decode with SPARK_Mode is
   Nibble_Mask : constant Unsigned_8 := 16#0F#;
   Nibble_Bits : constant := 4;
   Largest_Digit : constant := 9;
   Decimal_Base : constant := 10;
   Hours_Per_Half_Day : constant := 12;

   subtype Register_Value is Natural range 0 .. Natural (Unsigned_8'Last);

   --  A register in the snapshot's encoding; Valid is False for a BCD byte
   --  with a nibble above 9.
   procedure Number
     (Raw : Unsigned_8; Binary : Boolean; Value : out Register_Value; Valid : out Boolean)
   is
      Low : constant Unsigned_8 := Raw and Nibble_Mask;
      High : constant Unsigned_8 := Shift_Right (Raw, Nibble_Bits);
   begin
      if Binary then
         Value := Register_Value (Raw);
         Valid := True;
      elsif Low > Largest_Digit or else High > Largest_Digit then
         Value := 0;
         Valid := False;
      else
         Value := Natural (Low) + Decimal_Base * Natural (High);
         Valid := True;
      end if;
   end Number;

   function Decode (Registers : Snapshot; Century : Century_Base) return Result is
      Binary : constant Boolean := Is_Binary (Registers.Status_B);
      Hour_24 : constant Boolean := Is_24_Hour (Registers.Status_B);
      PM : constant Boolean := not Hour_24 and then (Registers.Hour and PM_Flag) /= 0;
      Hour_Raw : constant Unsigned_8 :=
        (if Hour_24 then Registers.Hour else Registers.Hour and Hour_Mask);
      S, N, H, D, M, Y : Register_Value;
      OK : Boolean;
      Failure : Result;

      function Failed (Status : Outcome; Which : Field; Raw : Unsigned_8) return Result is
        ((Status => Status, Failed => Which, Raw => Raw, Date => <>));
   begin
      Number (Registers.Second, Binary, S, OK);
      if not OK then return Failed (Bad_BCD, Second_Field, Registers.Second); end if;
      Number (Registers.Minute, Binary, N, OK);
      if not OK then return Failed (Bad_BCD, Minute_Field, Registers.Minute); end if;
      Number (Hour_Raw, Binary, H, OK);
      if not OK then return Failed (Bad_BCD, Hour_Field, Registers.Hour); end if;
      Number (Registers.Day, Binary, D, OK);
      if not OK then return Failed (Bad_BCD, Day_Field, Registers.Day); end if;
      Number (Registers.Month, Binary, M, OK);
      if not OK then return Failed (Bad_BCD, Month_Field, Registers.Month); end if;
      Number (Registers.Year, Binary, Y, OK);
      if not OK then return Failed (Bad_BCD, Year_Field, Registers.Year); end if;

      if not Hour_24 then
         --  12 AM is midnight, 12 PM noon.
         if H not in 1 .. Hours_Per_Half_Day then
            return Failed (Bad_12_Hour, Hour_Field, Registers.Hour);
         end if;
         H := H mod Hours_Per_Half_Day + (if PM then Hours_Per_Half_Day else 0);
      end if;

      if S not in Civil_Time.Second_Number then
         Failure := Failed (Out_Of_Range, Second_Field, Registers.Second);
      elsif N not in Civil_Time.Minute_Number then
         Failure := Failed (Out_Of_Range, Minute_Field, Registers.Minute);
      elsif H not in Civil_Time.Hour_Number then
         Failure := Failed (Out_Of_Range, Hour_Field, Registers.Hour);
      elsif D not in Civil_Time.Day_Number then
         Failure := Failed (Out_Of_Range, Day_Field, Registers.Day);
      elsif M not in Civil_Time.Month_Number then
         Failure := Failed (Out_Of_Range, Month_Field, Registers.Month);
      elsif Y >= Years_Per_Century then
         Failure := Failed (Out_Of_Range, Year_Field, Registers.Year);
      else
         declare
            Date : constant Civil_Time.Date_Time :=
              (Year => Century + Y, Month => M, Day => D, Hour => H, Minute => N, Second => S);
         begin
            if Civil_Time.Valid (Date) then
               return (Status => Decoded, Failed => Second_Field, Raw => 0, Date => Date);
            end if;
            return (Status => Invalid_Date, Failed => Day_Field, Raw => Registers.Day, Date => Date);
         end;
      end if;
      return Failure;
   end Decode;
end RTC_Decode;
