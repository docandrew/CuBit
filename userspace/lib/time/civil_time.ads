with Interfaces; use Interfaces;
package Civil_Time with SPARK_Mode, Pure is
   subtype Year_Number is Natural range 1970 .. 2399;
   subtype Month_Number is Positive range 1 .. 12;
   subtype Day_Number is Positive range 1 .. 31;
   subtype Hour_Number is Natural range 0 .. 23;
   subtype Minute_Number is Natural range 0 .. 59;
   subtype Second_Number is Natural range 0 .. 59;
   subtype Timestamp is Integer_64 range 0 .. 13_569_465_599;
   type Date_Time is record
      Year : Year_Number := 1970;
      Month : Month_Number := 1;
      Day : Day_Number := 1;
      Hour : Hour_Number := 0;
      Minute : Minute_Number := 0;
      Second : Second_Number := 0;
   end record;
   function Leap (Year : Year_Number) return Boolean is
     (Year mod 4 = 0 and then (Year mod 100 /= 0 or else Year mod 400 = 0));
   function Days (Year : Year_Number; Month : Month_Number) return Day_Number;
   function Valid (Value : Date_Time) return Boolean is
     (Value.Day <= Days (Value.Year, Value.Month));
   function Seconds (Value : Date_Time) return Timestamp with Pre => Valid (Value);
   function Split (Value : Timestamp) return Date_Time;
end Civil_Time;
