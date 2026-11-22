pragma Ada_2022;

package body Charts_Format with SPARK_Mode => Off is
   Base : constant := 10;
   Thousand : constant := 1_000;
   Kibi : constant := 1_024;
   Seconds_Per_Minute : constant := 60;
   Minutes_Per_Hour : constant := 60;
   Microseconds_Per_Millisecond : constant := 1_000;
   Microseconds_Per_Second : constant := 1_000_000;
   Microseconds_Per_Minute : constant := Seconds_Per_Minute * Microseconds_Per_Second;
   Microseconds_Per_Hour : constant := Minutes_Per_Hour * Microseconds_Per_Minute;
   --  Values shown with two decimals below this, one below Base * this.
   Two_Decimals_Below : constant := 10;
   One_Decimal_Below : constant := 100;

   function Decimal (V : Unsigned_64) return String is
     (if V < Base then [Character'Val (Character'Pos ('0') + Natural (V))]
      else Decimal (V / Base) & [Character'Val (Character'Pos ('0') + Natural (V mod Base))]);

   function Two_Digits (V : Unsigned_64) return String is
     ((if V mod One_Decimal_Below < Base then "0" else "") & Decimal (V mod One_Decimal_Below));

   function Three_Digits (V : Unsigned_64) return String is
     ((if V mod Thousand < One_Decimal_Below then "0" else "") &
      (if V mod Thousand < Base then "0" else "") & Decimal (V mod Thousand));

   --  V / Divisor with three significant digits, trailing zeros dropped.
   function Scaled (V, Divisor : Unsigned_64) return String
     with Pre => Divisor >= 1 and then Divisor <= Unsigned_64'Last / One_Decimal_Below;
   function Scaled (V, Divisor : Unsigned_64) return String is
      Whole : constant Unsigned_64 := V / Divisor;
      Rest : constant Unsigned_64 := V mod Divisor;
   begin
      if Whole >= One_Decimal_Below then
         return Decimal (Whole);
      elsif Whole >= Two_Decimals_Below then
         declare
            Tenth : constant Unsigned_64 := Rest * Base / Divisor;
         begin
            return Decimal (Whole) & (if Tenth = 0 then "" else "." & Decimal (Tenth));
         end;
      else
         declare
            Hundredths : constant Unsigned_64 := Rest * One_Decimal_Below / Divisor;
         begin
            if Hundredths = 0 then
               return Decimal (Whole);
            elsif Hundredths mod Base = 0 then
               return Decimal (Whole) & "." & Decimal (Hundredths / Base);
            else
               return Decimal (Whole) & "." & Two_Digits (Hundredths);
            end if;
         end;
      end if;
   end Scaled;

   function Value_Text (V : Unsigned_64; U : Unit) return String is
      Million : constant := Thousand * Thousand;
      Billion : constant := Thousand * Million;
      Mebi : constant := Kibi * Kibi;
      Gibi : constant := Kibi * Mebi;
   begin
      case U is
         when Microseconds =>
            return (if V < Thousand then Decimal (V) & " us"
                    elsif V < Million then Scaled (V, Thousand) & " ms"
                    else Scaled (V, Million) & " s");
         when Nanoseconds =>
            return (if V < Thousand then Decimal (V) & " ns"
                    elsif V < Million then Scaled (V, Thousand) & " us"
                    elsif V < Billion then Scaled (V, Million) & " ms"
                    else Scaled (V, Billion) & " s");
         when Bytes =>
            return (if V < Kibi then Decimal (V) & " B"
                    elsif V < Mebi then Scaled (V, Kibi) & " KiB"
                    elsif V < Gibi then Scaled (V, Mebi) & " MiB"
                    else Scaled (V, Gibi) & " GiB");
         when Count | Pixels =>
            declare
               Suffix : constant String := (if U = Pixels then " px" else "");
            begin
               return (if V < Base * Thousand then Decimal (V)
                       elsif V < Million then Scaled (V, Thousand) & "k"
                       elsif V < Billion then Scaled (V, Million) & "M"
                       else Scaled (V, Billion) & "G") & Suffix;
            end;
      end case;
   end Value_Text;

   function Axis_Text (V, Top : Unsigned_64; U : Unit) return String is
      Million : constant := Thousand * Thousand;
      Billion : constant := Thousand * Million;
   begin
      if V = 0 then
         return "0";
      end if;
      case U is
         when Microseconds =>
            return (if Top < Thousand then Decimal (V) & " us"
                    elsif Top < Million then Scaled (V, Thousand) & " ms"
                    else Scaled (V, Million) & " s");
         when Nanoseconds =>
            return (if Top < Thousand then Decimal (V) & " ns"
                    elsif Top < Million then Scaled (V, Thousand) & " us"
                    elsif Top < Billion then Scaled (V, Million) & " ms"
                    else Scaled (V, Billion) & " s");
         when Bytes | Count | Pixels =>
            return Value_Text (V, U);
      end case;
   end Axis_Text;

   function Clock_Text (T : Unsigned_64; Step : Unsigned_64) return String is
      Hours : constant Unsigned_64 := T / Microseconds_Per_Hour;
      Minutes : constant Unsigned_64 := T / Microseconds_Per_Minute mod Minutes_Per_Hour;
      Seconds : constant Unsigned_64 := T / Microseconds_Per_Second mod Seconds_Per_Minute;
      Millis : constant Unsigned_64 := T / Microseconds_Per_Millisecond mod Thousand;
      Clock : constant String :=
        (if Hours > 0 then Decimal (Hours) & ":" & Two_Digits (Minutes) else Decimal (Minutes))
        & ":" & Two_Digits (Seconds);
   begin
      return (if Step < Microseconds_Per_Second then Clock & "." & Three_Digits (Millis) else Clock);
   end Clock_Text;

   function Span_Text (Span_Us : Unsigned_64) return String is
      Microseconds_Per_Day : constant := 24 * Microseconds_Per_Hour;
   begin
      if Span_Us >= Microseconds_Per_Day and then Span_Us mod Microseconds_Per_Day = 0 then
         return Decimal (Span_Us / Microseconds_Per_Day) & (if Span_Us = Microseconds_Per_Day then " day" else " days");
      elsif Span_Us >= Microseconds_Per_Hour and then Span_Us mod Microseconds_Per_Hour = 0 then
         return Decimal (Span_Us / Microseconds_Per_Hour) & " h";
      elsif Span_Us >= Microseconds_Per_Minute and then Span_Us mod Microseconds_Per_Minute = 0 then
         return Decimal (Span_Us / Microseconds_Per_Minute) & " min";
      elsif Span_Us >= Microseconds_Per_Second then
         return Scaled (Span_Us, Microseconds_Per_Second) & " s";
      else
         return Scaled (Span_Us, Microseconds_Per_Millisecond) & " ms";
      end if;
   end Span_Text;
end Charts_Format;
