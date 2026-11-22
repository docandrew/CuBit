with Interfaces; use Interfaces;

package body CCL.Units with SPARK_Mode is
   function Unit_Of (Type_Name : String) return Unit is
     (if Type_Name = "Bytes" then Bytes
      elsif Type_Name = "Timestamp" then Timestamp
      elsif Type_Name = "UNIX_File_Permissions" then UNIX_File_Permissions
      elsif Type_Name = "Milliseconds" then Milliseconds
      else No_Unit);

   MAXIMUM_DIGITS : constant := 19;

   --  A non-negative decimal of at most MAXIMUM_DIGITS digits.
   procedure Parse (Text : String; Value : out Unsigned_64; Valid : out Boolean) is
   begin
      Value := 0;
      Valid := Text'Length in 1 .. MAXIMUM_DIGITS;
      if not Valid then return; end if;
      for C of Text loop
         if C not in '0' .. '9' then
            Valid := False; return;
         end if;
         --  At most 19 digits: below 10 ** 19, within 64 bits.
         Value := Value * 10 + Unsigned_64 (Character'Pos (C) - Character'Pos ('0'));
      end loop;
   end Parse;

   --  The decimal digits of any Unsigned_64.
   UNSIGNED_DIGITS : constant := 20;

   --  Text, indexed from 1.
   function Rebased (Text : String) return String
     with Post => Rebased'Result'First = 1 and then Rebased'Result'Length = Text'Length
   is
      Result : constant String (1 .. Text'Length) := Text;
   begin
      return Result;
   end Rebased;

   function Decimal (Value : Unsigned_64) return String
     with Post => Decimal'Result'First = 1 and then Decimal'Result'Length in 1 .. UNSIGNED_DIGITS
   is
      Text : String (1 .. UNSIGNED_DIGITS) := [others => '0'];
      NUMERALS : constant String (1 .. 10) := "0123456789";
      Rest : Unsigned_64 := Value;
      First : Positive range 1 .. UNSIGNED_DIGITS := UNSIGNED_DIGITS;
   begin
      loop
         declare
            Digit : constant Unsigned_64 := Rest mod 10;
         begin
            pragma Assert (Digit < 10);
            Text (First) := NUMERALS (Natural (Digit) + 1);
         end;
         Rest := Rest / 10;
         exit when Rest = 0 or else First = 1;
         First := First - 1;
         pragma Loop_Variant (Decreases => First);
      end loop;
      return Rebased (Text (First .. UNSIGNED_DIGITS));
   end Decimal;

   function Two (Value : Unsigned_64) return String is
     (if Value < 10 then "0" & Decimal (Value) else Decimal (Value mod 100))
   with Post => Two'Result'First = 1 and then Two'Result'Length in 1 .. UNSIGNED_DIGITS + 1;

   --  Value scaled by a power of 1024, with one decimal.
   function Size (Value : Unsigned_64) return String is
      type Prefix is (B, KiB, MiB, GiB, TiB, PiB, EiB);
      function Spelling (Item : Prefix) return String is
        (case Item is
            when B => "B", when KiB => "KiB", when MiB => "MiB", when GiB => "GiB",
            when TiB => "TiB", when PiB => "PiB", when EiB => "EiB");
      KIBI : constant := 1_024;
      SCALES : constant array (Prefix) of Unsigned_64 :=
        [B => 1, KiB => KIBI, MiB => KIBI ** 2, GiB => KIBI ** 3, TiB => KIBI ** 4,
         PiB => KIBI ** 5, EiB => KIBI ** 6];
      Step : Prefix := KiB;
   begin
      if Value < KIBI then return Decimal (Value) & " B"; end if;
      --  The largest prefix whose scale Value reaches.
      for Next in Prefix range MiB .. EiB loop
         exit when Value < SCALES (Next);
         Step := Next;
         pragma Loop_Invariant (Step in KiB .. EiB);
      end loop;
      declare
         Scale : constant Unsigned_64 := SCALES (Step);
         Whole : constant Unsigned_64 := Value / Scale;
         --  Scale / 10 rounds down, so the quotient can reach 10: show 9.
         Tenths : constant Unsigned_64 := Unsigned_64'Min (9, (Value mod Scale) / (Scale / 10));
      begin
         return Decimal (Whole) & "." & Decimal (Tenths) & " " & Spelling (Step);
      end;
   end Size;

   --  Milliseconds since the Unix epoch as a UTC date and time, through
   --  the days-to-civil algorithm (H. Hinnant, "chrono-Compatible Low-Level
   --  Date Algorithms").
   function Instant (Value : Unsigned_64) return String is
      MS_PER_MINUTE : constant := 60_000;
      MINUTES_PER_DAY : constant := 1_440;
      DAYS_PER_ERA : constant := 146_097;
      Minutes : constant Unsigned_64 := Value / MS_PER_MINUTE;
      Days : constant Unsigned_64 := Minutes / MINUTES_PER_DAY;
      Minute_Of_Day : constant Unsigned_64 := Minutes mod MINUTES_PER_DAY;
      Z : constant Unsigned_64 := Days + 719_468;
      Era : constant Unsigned_64 := Z / DAYS_PER_ERA;
      Day_Of_Era : constant Unsigned_64 := Z - Era * DAYS_PER_ERA;
      Year_Of_Era : constant Unsigned_64 :=
        (Day_Of_Era - Day_Of_Era / 1_460 + Day_Of_Era / 36_524 - Day_Of_Era / 146_096) / 365;
      Day_Of_Year : constant Unsigned_64 :=
        Day_Of_Era - (365 * Year_Of_Era + Year_Of_Era / 4 - Year_Of_Era / 100);
      Shifted_Month : constant Unsigned_64 := (5 * Day_Of_Year + 2) / 153;
      Day : constant Unsigned_64 := Day_Of_Year - (153 * Shifted_Month + 2) / 5 + 1;
      Month : constant Unsigned_64 := (if Shifted_Month < 10 then Shifted_Month + 3 else Shifted_Month - 9);
      Year : constant Unsigned_64 := Year_Of_Era + Era * 400 + (if Month <= 2 then 1 else 0);
   begin
      return Decimal (Year) & "-" & Two (Month) & "-" & Two (Day) & " " &
        Two (Minute_Of_Day / 60) & ":" & Two (Minute_Of_Day mod 60);
   end Instant;

   --  The stored type bits and rwx for owner, group and others.
   function Mode (Value : Unsigned_64) return String is
      TYPE_MASK : constant := 8#170000#;
      DIRECTORY_TYPE : constant := 8#040000#;
      LINK_TYPE : constant := 8#120000#;
      FILE_TYPE : constant := 8#100000#;
      Kind : constant Unsigned_64 := Value and TYPE_MASK;
      Letters : constant String := "rwx";
      Result : String (1 .. 10) := [others => '-'];
   begin
      Result (1) := (if Kind = DIRECTORY_TYPE then 'd' elsif Kind = LINK_TYPE then 'l'
                     elsif Kind = FILE_TYPE or else Kind = 0 then '-' else '?');
      for Bit in 0 .. 8 loop
         if (Value and Shift_Left (1, 8 - Bit)) /= 0 then
            Result (2 + Bit) := Letters (1 + Bit mod 3);
         end if;
      end loop;
      return Result;
   end Mode;

   function Duration (Value : Unsigned_64) return String is
      MS_PER_SECOND : constant := 1_000;
      SECONDS_PER_MINUTE : constant := 60;
   begin
      if Value < MS_PER_SECOND then
         return Decimal (Value) & " ms";
      elsif Value < SECONDS_PER_MINUTE * MS_PER_SECOND then
         return Decimal (Value / MS_PER_SECOND) & "." &
           Decimal ((Value mod MS_PER_SECOND) / 100) & " s";
      else
         return Decimal (Value / (SECONDS_PER_MINUTE * MS_PER_SECOND)) & " min " &
           Two ((Value / MS_PER_SECOND) mod SECONDS_PER_MINUTE) & " s";
      end if;
   end Duration;

   function Humanize (Of_Unit : Unit; Text : String) return String is
      Value : Unsigned_64;
      Valid : Boolean;
   begin
      if Of_Unit = No_Unit then return Text; end if;
      Parse (Text, Value, Valid);
      if not Valid then return Text; end if;
      return (case Of_Unit is
                 when Bytes => Size (Value),
                 --  An unknown time, or a volume that records none.
                 when Timestamp => (if Value = 0 then "-" else Instant (Value)),
                 when UNIX_File_Permissions => (if Value = 0 then "-" else Mode (Value)),
                 when Milliseconds => Duration (Value),
                 when No_Unit => Text);
   end Humanize;
end CCL.Units;
