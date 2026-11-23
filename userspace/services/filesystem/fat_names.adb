------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  FAT32 names (see the spec).
------------------------------------------------------------------------------
package body Fat_Names with SPARK_Mode is

   --  Characters by their code (ASCII; UTF-16 units below 16#80#).
   Unit_Space      : constant Unsigned_16 := 16#20#;
   Unit_Quote      : constant Unsigned_16 := 16#22#;
   Unit_Asterisk   : constant Unsigned_16 := 16#2A#;
   Unit_Dot        : constant Unsigned_16 := 16#2E#;
   Unit_Slash      : constant Unsigned_16 := 16#2F#;
   Unit_Zero       : constant Unsigned_16 := 16#30#;
   Unit_Nine       : constant Unsigned_16 := 16#39#;
   Unit_Colon      : constant Unsigned_16 := 16#3A#;
   Unit_Less       : constant Unsigned_16 := 16#3C#;
   Unit_Greater    : constant Unsigned_16 := 16#3E#;
   Unit_Question   : constant Unsigned_16 := 16#3F#;
   Unit_Upper_A    : constant Unsigned_16 := 16#41#;
   Unit_Upper_Z    : constant Unsigned_16 := 16#5A#;
   Unit_Backslash  : constant Unsigned_16 := 16#5C#;
   Unit_Lower_A    : constant Unsigned_16 := 16#61#;
   Unit_Lower_Z    : constant Unsigned_16 := 16#7A#;
   Unit_Bar        : constant Unsigned_16 := 16#7C#;
   First_Printable : constant Unsigned_16 := 16#20#;
   ASCII_Last      : constant Unsigned_16 := 16#7F#;
   Case_Distance   : constant Unsigned_16 := 16#20#;
   Byte_Slash      : constant Unsigned_8 := 16#2F#;
   Byte_Tilde      : constant Unsigned_8 := 16#7E#;
   Byte_Underscore : constant Unsigned_8 := 16#5F#;
   Byte_Zero       : constant Unsigned_8 := 16#30#;
   Byte_Nine       : constant Unsigned_8 := 16#39#;
   Byte_Upper_A    : constant Unsigned_8 := 16#41#;
   Byte_Upper_Z    : constant Unsigned_8 := 16#5A#;
   Byte_High       : constant Unsigned_8 := 16#80#;

   --  UTF-16 surrogates and the code point ranges of UTF-8 forms.
   High_Surrogate_First : constant := 16#D800#;
   High_Surrogate_Last  : constant := 16#DBFF#;
   Low_Surrogate_First  : constant := 16#DC00#;
   Low_Surrogate_Last   : constant := 16#DFFF#;
   Supplementary_First  : constant := 16#1_0000#;
   Code_Point_Last      : constant := 16#10_FFFF#;
   Two_Byte_First       : constant := 16#80#;
   Three_Byte_First     : constant := 16#800#;
   Continuation_Mask    : constant Unsigned_8 := 16#C0#;
   Continuation_Tag     : constant Unsigned_8 := 16#80#;
   Continuation_Bits    : constant Unsigned_8 := 16#3F#;
   Surrogate_Bits       : constant := 16#3FF#;

   function Continuation (B : Unsigned_8) return Boolean is
     ((B and Continuation_Mask) = Continuation_Tag);

   --  One code point of Name at I: its value and byte count, or Count 0.
   procedure Decode_UTF8 (Name : String; I : Positive; Point : out Unsigned_32; Count : out Natural)
     with Pre  => I in Name'Range,
          Post => Count <= 4 and then (if Count > 0 then Count - 1 <= Name'Last - I
                                       and then Point <= Code_Point_Last
                                       and then Point not in High_Surrogate_First .. Low_Surrogate_Last
                                       and then (Point >= Supplementary_First) = (Count = 4))
   is
      B0 : constant Unsigned_8 := Character'Pos (Name (I));
      function At_Offset (K : Positive) return Unsigned_8 is (Character'Pos (Name (I + K)))
        with Pre => I in Name'Range and then K <= Name'Last - I;
   begin
      Point := 0;
      Count := 0;
      if B0 < 16#80# then
         Point := Unsigned_32 (B0);
         Count := 1;
      elsif B0 in 16#C2# .. 16#DF# then
         if 1 <= Name'Last - I and then Continuation (At_Offset (1)) then
            Point := Shift_Left (Unsigned_32 (B0 and 16#1F#), 6)
              or Unsigned_32 (At_Offset (1) and Continuation_Bits);
            Count := 2;
         end if;
      elsif B0 in 16#E0# .. 16#EF# then
         if 2 <= Name'Last - I and then Continuation (At_Offset (1))
           and then Continuation (At_Offset (2))
         then
            Point := Shift_Left (Unsigned_32 (B0 and 16#0F#), 12)
              or Shift_Left (Unsigned_32 (At_Offset (1) and Continuation_Bits), 6)
              or Unsigned_32 (At_Offset (2) and Continuation_Bits);
            if Point >= Three_Byte_First
              and then Point not in High_Surrogate_First .. Low_Surrogate_Last
            then
               Count := 3;
            else
               Point := 0;
            end if;
         end if;
      elsif B0 in 16#F0# .. 16#F4# then
         if 3 <= Name'Last - I and then Continuation (At_Offset (1))
           and then Continuation (At_Offset (2)) and then Continuation (At_Offset (3))
         then
            Point := Shift_Left (Unsigned_32 (B0 and 16#07#), 18)
              or Shift_Left (Unsigned_32 (At_Offset (1) and Continuation_Bits), 12)
              or Shift_Left (Unsigned_32 (At_Offset (2) and Continuation_Bits), 6)
              or Unsigned_32 (At_Offset (3) and Continuation_Bits);
            if Point in Supplementary_First .. Code_Point_Last then
               Count := 4;
            else
               Point := 0;
            end if;
         end if;
      end if;
      pragma Assert (if Count = 2 then Point >= Two_Byte_First and then Point < Three_Byte_First);
   end Decode_UTF8;

   function Reserved_Character (Unit : Unsigned_16) return Boolean is
     (Unit in Unit_Quote | Unit_Asterisk | Unit_Slash | Unit_Colon | Unit_Less | Unit_Greater
            | Unit_Question | Unit_Backslash | Unit_Bar);

   --  The device names Windows reserves in every directory.
   function Device_Name (Units : Long_Units; Length : Long_Length) return Boolean is
      Base : Natural := 0;   --  units before the first dot
      function Upper (K : Positive) return Unsigned_16 is (Fold (Units (K)))
        with Pre => K <= Maximum_Long_Units;
      function Has (K : Positive; C : Character) return Boolean is
        (Upper (K) = Character'Pos (C))
        with Pre => K <= Maximum_Long_Units;
   begin
      for K in 1 .. Length loop
         exit when Units (K) = Unit_Dot;
         Base := K;
         pragma Loop_Invariant (Base = K);
      end loop;
      if Base = 3 then
         return (Has (1, 'C') and then Has (2, 'O') and then Has (3, 'N'))
           or else (Has (1, 'P') and then Has (2, 'R') and then Has (3, 'N'))
           or else (Has (1, 'A') and then Has (2, 'U') and then Has (3, 'X'))
           or else (Has (1, 'N') and then Has (2, 'U') and then Has (3, 'L'));
      elsif Base = 4 then
         return ((Has (1, 'C') and then Has (2, 'O') and then Has (3, 'M'))
                 or else (Has (1, 'L') and then Has (2, 'P') and then Has (3, 'T')))
           and then Units (4) in Unit_Zero + 1 .. Unit_Nine;
      end if;
      return False;
   end Device_Name;

   procedure To_Units
     (Name : String; Strict : Boolean; Units : out Long_Units; Length : out Long_Length;
      Result : out Name_Check)
   is
      I : Positive := 1;
      Count : Long_Length := 0;
      Point : Unsigned_32;
      Bytes : Natural;
   begin
      Units := [others => Name_Padding];
      Length := 0;
      if Name'Length = 0 then
         Result := Name_Empty;
         return;
      elsif Strict and then (Name = "." or else Name = "..") then
         Result := Name_Dots;
         return;
      end if;
      loop
         pragma Loop_Invariant (I in Name'Range and then Count <= I - 1);
         Decode_UTF8 (Name, I, Point, Bytes);
         if Bytes = 0 then
            Result := Name_Not_UTF8;
            return;
         elsif Strict and then Point < Unsigned_32 (First_Printable) then
            Result := Name_Control;
            return;
         elsif Strict and then Point <= Unsigned_32 (ASCII_Last)
           and then Reserved_Character (Unsigned_16 (Point))
         then
            Result := Name_Reserved_Character;
            return;
         end if;
         if Point >= Supplementary_First then
            if Count > Maximum_Long_Units - 2 then
               Result := Name_Too_Long;
               return;
            end if;
            Units (Count + 1) := Unsigned_16 (High_Surrogate_First
              + Shift_Right (Point - Supplementary_First, 10));
            Units (Count + 2) := Unsigned_16 (Low_Surrogate_First
              + ((Point - Supplementary_First) and Surrogate_Bits));
            Count := Count + 2;
         else
            if Count = Maximum_Long_Units then
               Result := Name_Too_Long;
               return;
            end if;
            Units (Count + 1) := Unsigned_16 (Point);
            Count := Count + 1;
         end if;
         exit when I > Name'Last - Bytes;
         I := I + Bytes;
      end loop;
      Length := Count;
      if Strict and then Units (Count) in Unit_Dot | Unit_Space then
         Result := Name_Trailing;
      elsif Strict and then Device_Name (Units, Count) then
         Result := Name_Device;
      else
         Result := Name_Valid;
      end if;
   end To_Units;

   procedure From_Units
     (Units : Long_Units; Length : Long_Length; Text : out Name_Text; Text_Length : out Name_Length;
      OK : out Boolean)
   is
      K : Positive := 1;
      Count : Name_Length := 0;
      Point : Unsigned_32;
      Unit : Unsigned_16;
      Bytes : Positive;

      procedure Put (B : Unsigned_32)
        with Pre  => Count < Maximum_Name_Bytes and then B <= 16#FF#,
             Post => Count = Count'Old + 1
      is
      begin
         Count := Count + 1;
         Text (Count) := Character'Val (B);
      end Put;
   begin
      Text := [others => ' '];
      Text_Length := 0;
      OK := False;
      if Length = 0 then
         return;
      end if;
      loop
         pragma Loop_Invariant (K in 1 .. Length);
         Unit := Units (K);
         if Unit in High_Surrogate_First .. High_Surrogate_Last then
            if K = Length or else Units (K + 1) not in Low_Surrogate_First .. Low_Surrogate_Last then
               return;   --  unpaired
            end if;
            Point := Supplementary_First
              + Shift_Left (Unsigned_32 (Unit) - High_Surrogate_First, 10)
              + (Unsigned_32 (Units (K + 1)) - Low_Surrogate_First);
            K := K + 1;
         elsif Unit in Low_Surrogate_First .. Low_Surrogate_Last then
            return;   --  unpaired
         elsif Unit < First_Printable or else Unit = Unit_Slash then
            return;
         else
            Point := Unsigned_32 (Unit);
         end if;
         Bytes := (if Point < Two_Byte_First then 1 elsif Point < Three_Byte_First then 2
                   elsif Point < Supplementary_First then 3 else 4);
         if Count > Maximum_Name_Bytes - Bytes then
            return;   --  needs more than 255 bytes
         end if;
         case Bytes is
            when 1 =>
               Put (Point);
            when 2 =>
               Put (16#C0# or Shift_Right (Point, 6));
               Put (16#80# or (Point and 16#3F#));
            when 3 =>
               Put (16#E0# or Shift_Right (Point, 12));
               Put (16#80# or (Shift_Right (Point, 6) and 16#3F#));
               Put (16#80# or (Point and 16#3F#));
            when others =>
               Put (16#F0# or Shift_Right (Point, 18));
               Put (16#80# or (Shift_Right (Point, 12) and 16#3F#));
               Put (16#80# or (Shift_Right (Point, 6) and 16#3F#));
               Put (16#80# or (Point and 16#3F#));
         end case;
         exit when K = Length;
         K := K + 1;
      end loop;
      Text_Length := Count;
      OK := Count >= 1;
   end From_Units;

   function Fold (Unit : Unsigned_16) return Unsigned_16 is
      Latin_Lower_First   : constant := 16#E0#;
      Latin_Lower_Last    : constant := 16#FE#;
      Division_Sign       : constant := 16#F7#;
      Y_Diaeresis         : constant := 16#FF#;
      Upper_Y_Diaeresis   : constant := 16#178#;
      Greek_Lower_First   : constant := 16#3B1#;
      Greek_Lower_Last    : constant := 16#3C9#;
      Greek_Final_Sigma   : constant := 16#3C2#;
      Cyrillic_Lower_First : constant := 16#430#;
      Cyrillic_Lower_Last  : constant := 16#44F#;
      Cyrillic_Extra_First : constant := 16#450#;
      Cyrillic_Extra_Last  : constant := 16#45F#;
      Cyrillic_Extra_Distance : constant := 16#50#;
   begin
      if Unit in Unit_Lower_A .. Unit_Lower_Z then
         return Unit - Case_Distance;
      elsif Unit in Latin_Lower_First .. Latin_Lower_Last and then Unit /= Division_Sign then
         return Unit - Case_Distance;
      elsif Unit = Y_Diaeresis then
         return Upper_Y_Diaeresis;
      elsif Unit in Greek_Lower_First .. Greek_Lower_Last and then Unit /= Greek_Final_Sigma then
         return Unit - Case_Distance;
      elsif Unit in Cyrillic_Lower_First .. Cyrillic_Lower_Last then
         return Unit - Case_Distance;
      elsif Unit in Cyrillic_Extra_First .. Cyrillic_Extra_Last then
         return Unit - Cyrillic_Extra_Distance;
      end if;
      return Unit;
   end Fold;

   function Same_Name (A : Long_Units; A_Length : Long_Length; B : Long_Units; B_Length : Long_Length)
     return Boolean is
   begin
      if A_Length /= B_Length then
         return False;
      end if;
      for K in 1 .. A_Length loop
         if Fold (A (K)) /= Fold (B (K)) then
            return False;
         end if;
      end loop;
      return True;
   end Same_Name;

   procedure Short_Units
     (Name : Short_Name; Case_Flags : Unsigned_8; Units : out Long_Units; Length : out Long_Length;
      OK : out Boolean)
   is
      Base_Last : Integer range -1 .. Base_Bytes - 1 := Base_Bytes - 1;
      Ext_Last : Integer range Base_Bytes - 1 .. Short_Name_Bytes - 1 := Short_Name_Bytes - 1;

      procedure Add (Byte : Unsigned_8; Lower : Boolean; Good : in out Boolean)
        with Pre  => Length < Maximum_Short_Units,
             Post => Length = Length'Old + 1
      is
         Unit : Unsigned_16;
      begin
         if Byte >= Byte_High then
            Unit := Code_Page_437 (Byte);
         elsif Byte < Space or else Byte = Byte_Slash then
            Good := False;
            Unit := Unsigned_16 (Byte_Underscore);
         elsif Lower and then Byte in Byte_Upper_A .. Byte_Upper_Z then
            Unit := Unsigned_16 (Byte) + Case_Distance;
         else
            Unit := Unsigned_16 (Byte);
         end if;
         Length := Length + 1;
         Units (Length) := Unit;
      end Add;

      Good : Boolean := True;
   begin
      Units := [others => Name_Padding];
      Length := 0;
      while Base_Last >= 0 and then Name (Base_Last) = Space loop
         pragma Loop_Variant (Decreases => Base_Last);
         Base_Last := Base_Last - 1;
      end loop;
      while Ext_Last >= Base_Bytes and then Name (Ext_Last) = Space loop
         pragma Loop_Variant (Decreases => Ext_Last);
         Ext_Last := Ext_Last - 1;
      end loop;
      if Base_Last < 0 then
         OK := False;
         return;
      end if;
      for K in 0 .. Base_Last loop
         pragma Loop_Invariant (Length = K);
         Add ((if K = 0 and then Name (K) = Kanji_Lead then Free_Marker else Name (K)),
              (Case_Flags and Lower_Base) /= 0, Good);
      end loop;
      if Ext_Last >= Base_Bytes then
         Length := Length + 1;
         Units (Length) := Unit_Dot;
         for K in Base_Bytes .. Ext_Last loop
            pragma Loop_Invariant (Length = Base_Last + 2 + (K - Base_Bytes));
            Add (Name (K), (Case_Flags and Lower_Extension) /= 0, Good);
         end loop;
      end if;
      OK := Good;
   end Short_Units;

   --  A byte FATGEN section 6.1 allows in a short name (besides upper-case
   --  letters, digits and high bytes): $ % ' - _ @ ~ ` ! ( ) { } ^ # &.
   function Short_Special (Unit : Unsigned_16) return Boolean is
     (Unit in 16#24# | 16#25# | 16#27# | 16#2D# | 16#5F# | 16#40# | 16#7E# | 16#60# | 16#21#
            | 16#28# | 16#29# | 16#7B# | 16#7D# | 16#5E# | 16#23# | 16#26#);

   --  A unit kept as it is in a short name (ASCII only: high bytes would
   --  depend on the OEM code page).
   function Short_Legal (Unit : Unsigned_16) return Boolean is
     (Unit in Unit_Upper_A .. Unit_Upper_Z or else Unit in Unit_Zero .. Unit_Nine
      or else Short_Special (Unit));

   procedure Exact_Short (Units : Long_Units; Length : Long_Length; Short : out Short_Name; Exact : out Boolean) is
      Dot_At : Natural := 0;
   begin
      Short := [others => Space];
      Exact := False;
      if Length = 0 or else Length > Maximum_Short_Units then
         return;
      end if;
      for K in 1 .. Length loop
         if Units (K) = Unit_Dot then
            if Dot_At /= 0 then
               return;
            end if;
            Dot_At := K;
         elsif not Short_Legal (Units (K)) then
            return;
         end if;
         pragma Loop_Invariant (Dot_At <= K);
      end loop;
      if Dot_At = 0 then
         if Length > Base_Bytes then
            return;
         end if;
         for K in 1 .. Length loop
            Short (K - 1) := Unsigned_8 (Units (K) and 16#7F#);
         end loop;
      else
         if Dot_At = 1 or else Dot_At - 1 > Base_Bytes or else Length = Dot_At
           or else Length - Dot_At > Extension_Bytes
         then
            return;
         end if;
         for K in 1 .. Dot_At - 1 loop
            Short (K - 1) := Unsigned_8 (Units (K) and 16#7F#);
         end loop;
         for K in Dot_At + 1 .. Length loop
            Short (Base_Bytes + K - Dot_At - 1) := Unsigned_8 (Units (K) and 16#7F#);
         end loop;
      end if;
      Exact := True;
   end Exact_Short;

   function Make_Basis (Units : Long_Units; Length : Long_Length) return Basis is
      B : Basis := (Base => [others => Space], Length => 1, Ext => [others => Space], Lossy => False);
      Last_Dot : Natural := 0;
      First_Real : Positive := 1;
      Base_Count, Ext_Count : Natural := 0;
      Base_End : Natural;

      --  The short-name byte for a unit; Lossy when it had to change.
      function Byte_For (Unit : Unsigned_16) return Unsigned_8 is
        (if Unit in Unit_Lower_A .. Unit_Lower_Z then Unsigned_8 (Unit - Case_Distance)
         elsif Short_Legal (Unit) then Unsigned_8 (Unit and 16#7F#)
         else Byte_Underscore);
      function Changes (Unit : Unsigned_16) return Boolean is
        (Unit not in Unit_Lower_A .. Unit_Lower_Z and then not Short_Legal (Unit));
   begin
      --  Leading dots and spaces are dropped (FATGEN: "strip all leading
      --  periods" and spaces).
      while First_Real < Length and then Units (First_Real) in Unit_Dot | Unit_Space loop
         pragma Loop_Variant (Increases => First_Real);
         First_Real := First_Real + 1;
      end loop;
      for K in First_Real .. Length loop
         if Units (K) = Unit_Dot then
            Last_Dot := K;
         end if;
         pragma Loop_Invariant (Last_Dot = 0 or else Last_Dot in First_Real .. K);
      end loop;
      Base_End := (if Last_Dot = 0 then Length else Last_Dot - 1);
      if First_Real > 1 then
         B.Lossy := True;
      end if;
      for K in First_Real .. Base_End loop
         pragma Loop_Invariant (Base_Count <= Base_Bytes);
         if Units (K) in Unit_Dot | Unit_Space then
            B.Lossy := True;
         elsif Base_Count = Base_Bytes then
            B.Lossy := True;   --  cut
         else
            B.Base (Base_Count) := Byte_For (Units (K));
            if Changes (Units (K)) then
               B.Lossy := True;
            end if;
            Base_Count := Base_Count + 1;
         end if;
      end loop;
      if Last_Dot /= 0 then
         for K in Last_Dot + 1 .. Length loop
            pragma Loop_Invariant (Ext_Count <= Extension_Bytes);
            if Units (K) = Unit_Space then
               B.Lossy := True;
            elsif Ext_Count = Extension_Bytes then
               B.Lossy := True;
            else
               B.Ext (Ext_Count) := Byte_For (Units (K));
               if Changes (Units (K)) then
                  B.Lossy := True;
               end if;
               Ext_Count := Ext_Count + 1;
            end if;
         end loop;
      end if;
      if Base_Count = 0 then
         B.Base (0) := Byte_Underscore;
         Base_Count := 1;
         B.Lossy := True;
      end if;
      B.Length := Base_Count;
      return B;
   end Make_Basis;

   function Plain_Alias (B : Basis) return Short_Name is
      Name : Short_Name := [others => Space];
   begin
      for K in 0 .. Base_Bytes - 1 loop
         Name (K) := B.Base (K);
      end loop;
      for K in 0 .. Extension_Bytes - 1 loop
         Name (Base_Bytes + K) := B.Ext (K);
      end loop;
      return Name;
   end Plain_Alias;

   subtype Digit_Count is Positive range 1 .. 6;
   function Digits_Of (N : Tail_Number) return Digit_Count is
     (if N < 10 then 1 elsif N < 100 then 2 elsif N < 1_000 then 3
      elsif N < 10_000 then 4 elsif N < 100_000 then 5 else 6);

   --  How much of the base an alias with D tail digits keeps.
   function Kept (B : Basis; D : Digit_Count) return Natural is
     (Natural'Min (B.Length, Base_Bytes - 1 - D))
     with Post => Kept'Result + 1 + D <= Base_Bytes;

   function Alias (B : Basis; N : Tail_Number) return Short_Name is
      D : constant Digit_Count := Digits_Of (N);
      K : constant Natural := Kept (B, D);
      Name : Short_Name := [others => Space];
      Rest : Natural := N;
   begin
      for I in 0 .. K - 1 loop
         Name (I) := B.Base (I);
      end loop;
      Name (K) := Byte_Tilde;
      for I in reverse K + 1 .. K + D loop
         Name (I) := Byte_Zero + Unsigned_8 (Rest mod 10);
         Rest := Rest / 10;
      end loop;
      for I in 0 .. Extension_Bytes - 1 loop
         Name (Base_Bytes + I) := B.Ext (I);
      end loop;
      return Name;
   end Alias;

   function Tail_Of (Name : Short_Name; B : Basis) return Natural is
      Powers_Of_Ten : constant array (0 .. Digit_Count'Last) of Positive :=
        [1, 10, 100, 1_000, 10_000, 100_000, 1_000_000];
   begin
      for I in 0 .. Extension_Bytes - 1 loop
         if Name (Base_Bytes + I) /= B.Ext (I) then
            return 0;
         end if;
      end loop;
      for D in Digit_Count loop
         declare
            K : constant Natural := Kept (B, D);
            Value : Natural := 0;
            Matches : Boolean := Name (K) = Byte_Tilde and then Name (K + 1) /= Byte_Zero;
         begin
            for I in 0 .. K - 1 loop
               Matches := Matches and then Name (I) = B.Base (I);
            end loop;
            for I in K + 1 .. K + D loop
               pragma Loop_Invariant (Value < Powers_Of_Ten (I - K - 1));
               if Name (I) in Byte_Zero .. Byte_Nine then
                  Value := Value * 10 + Natural (Name (I) - Byte_Zero);
               else
                  Matches := False;
               end if;
            end loop;
            for I in K + D + 1 .. Base_Bytes - 1 loop
               Matches := Matches and then Name (I) = Space;
            end loop;
            if Matches and then Value in Tail_Number and then Digits_Of (Value) = D then
               return Value;
            end if;
         end;
      end loop;
      return 0;
   end Tail_Of;

end Fat_Names;
