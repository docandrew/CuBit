pragma Ada_2022;

package body Desktop_Power_Text with SPARK_Mode is

   Per_Percent : constant := 10;
   Seconds_Per_Minute : constant := 60;
   Minutes_Per_Hour : constant := 60;

   --  A number's decimal digits, no sign (at most six: hours of a
   --  Maximum_Quantity runtime are 596523).
   Largest : constant := 999_999;
   function Digit (N : Natural) return Character is (Character'Val (Character'Pos ('0') + N mod 10));
   function Digits_Of (N : Natural) return String is
     (if N < 10 then [1 => Digit (N)]
      elsif N < 100 then [Digit (N / 10), Digit (N)]
      elsif N < 1_000 then [Digit (N / 100), Digit (N / 10), Digit (N)]
      elsif N < 10_000 then [Digit (N / 1_000), Digit (N / 100), Digit (N / 10), Digit (N)]
      elsif N < 100_000 then [Digit (N / 10_000), Digit (N / 1_000), Digit (N / 100), Digit (N / 10), Digit (N)]
      else [Digit (N / 100_000), Digit (N / 10_000), Digit (N / 1_000), Digit (N / 100), Digit (N / 10), Digit (N)])
   with Pre => N <= Largest, Post => Digits_Of'Result'Length in 1 .. 6 and then Digits_Of'Result'First = 1;

   function Percent_Text (C : PS.Per_Mille) return String is
     (if C.Known then Digits_Of (Natural (C.Value) / Per_Percent) & "%" else "--%");

   function Lower_State (S : PS.Summary) return String is
     (case S.State is
        when PS.Charging     => "charging",
        when PS.Discharging  => "discharging",
        when PS.Full         => "full",
        when PS.Not_Charging => "not charging",
        when PS.Online       => "online",
        when PS.Offline      => "offline",
        when PS.On_Battery   => "on battery",
        when PS.Unknown      => "state unknown");

   function State_Text (S : PS.Summary) return String is
     (case S.State is
        when PS.Charging     => "Charging",
        when PS.Discharging  => "Discharging",
        when PS.Full         => "Full",
        when PS.Not_Charging => "Not charging",
        when PS.Online       => "Online",
        when PS.Offline      => "Offline",
        when PS.On_Battery   => "On battery",
        when PS.Unknown      => "State unknown");

   function Runtime_Text (R : PS.Runtime) return String is
      Minutes : constant Natural := Natural (R.Seconds) / Seconds_Per_Minute;
      Hours : constant Natural := Minutes / Minutes_Per_Hour;
      Rest : constant Natural := Minutes mod Minutes_Per_Hour;
      Where : constant String := (if R.Kind = PS.To_Full then " to full" else " left");
   begin
      if R.Kind = PS.Unknown then
         return "";
      elsif Hours = 0 then
         return Digits_Of (Rest) & " min" & Where;
      else
         return Digits_Of (Hours) & " h " & Digits_Of (Rest) & " min" & Where;
      end if;
   end Runtime_Text;

   function Row_Text (S : PS.Summary) return String is
      Runtime : constant String := Runtime_Text (S.Left);
   begin
      --  "100%" already says full (user, image 37b): the state word is
      --  dropped then, kept for a full battery whose charge is unknown.
      return Percent_Text (S.Charge)
        & (if S.State = PS.Full and then S.Charge.Known and then Natural (S.Charge.Value) = PS.Whole
           then "" else ", " & Lower_State (S))
        & (if Runtime'Length > 0 then ", " & Runtime else "")
        & (if Plugged_Words (S) then ", plugged in" else "")
        & (if S.Low = PS.Set then ", low" else "")
        & (if S.Fresh = PS.Stale then ", last reading" else "");
   end Row_Text;

   function Fill_Width (C : PS.Per_Mille; Width : Natural) return Natural is
      Value : constant Natural := Natural (C.Value);
   begin
      if not C.Known then
         return 0;
      end if;
      pragma Assert (Value * Width <= PS.Whole * Width);
      pragma Assert ((PS.Whole * Width + (PS.Whole - 1)) / PS.Whole = Width);
      return (Value * Width + (PS.Whole - 1)) / PS.Whole;
   end Fill_Width;
end Desktop_Power_Text;
