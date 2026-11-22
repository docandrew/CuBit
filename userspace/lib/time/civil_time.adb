package body Civil_Time with SPARK_Mode is
   function Days (Year : Year_Number; Month : Month_Number) return Day_Number is
   begin
      case Month is
         when 2 => return (if Leap (Year) then 29 else 28);
         when 4 | 6 | 9 | 11 => return 30;
         when others => return 31;
      end case;
   end Days;
   function Seconds (Value : Date_Time) return Timestamp is
      Count : Integer_64 := 0;
   begin
      for Y in 1970 .. Value.Year - 1 loop
         Count := Count + (if Leap (Y) then 366 else 365);
      end loop;
      for M in 1 .. Value.Month - 1 loop
         Count := Count + Integer_64 (Days (Value.Year, M));
      end loop;
      return (Count + Integer_64 (Value.Day - 1)) * 86_400 +
        Integer_64 (Value.Hour) * 3600 + Integer_64 (Value.Minute) * 60 +
        Integer_64 (Value.Second);
   end Seconds;
   function Split (Value : Timestamp) return Date_Time is
      Result : Date_Time;
      Remaining : Natural := Natural (Value / 86_400);
      Year_Days : Positive;
   begin
      while Result.Year < Year_Number'Last loop
         Year_Days := (if Leap (Result.Year) then 366 else 365);
         exit when Remaining < Year_Days;
         Remaining := Remaining - Year_Days;
         Result.Year := Result.Year + 1;
      end loop;
      while Result.Month < 12 and then Remaining >= Days (Result.Year, Result.Month) loop
         Remaining := Remaining - Days (Result.Year, Result.Month);
         Result.Month := Result.Month + 1;
      end loop;
      Result.Day := Remaining + 1;
      Result.Hour := Natural ((Value / 3600) mod 24);
      Result.Minute := Natural ((Value / 60) mod 60);
      Result.Second := Natural (Value mod 60);
      return Result;
   end Split;
end Civil_Time;
