------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
package body CuBit.Locators with SPARK_Mode is

   use CuBit.Net_Address;

   function Digit (C : Character) return Natural is
     (Character'Pos (C) - Character'Pos ('0'))
   with Pre => Is_Digit (C), Post => Digit'Result <= 9;

   function Hex (C : Character) return Natural is
     (case C is
        when '0' .. '9' => Character'Pos (C) - Character'Pos ('0'),
        when 'a' .. 'f' => Character'Pos (C) - Character'Pos ('a') + 10,
        when others     => Character'Pos (C) - Character'Pos ('A') + 10)
   with Pre => Is_Hex (C), Post => Hex'Result <= 15;

   procedure Split
     (Text : String; Authority_Last : out Natural; Rest : out Positive;
      OK : out Boolean)
   is
      I : Positive := 2;
   begin
      Authority_Last := 0;
      Rest := 1;
      OK := False;
      if Text'Length < 3 or else Text (1) /= '@' then
         return;
      end if;
      while I <= Text'Last and then I <= Maximum_Authority + 1 and then
        Is_Word (Text (I))
      loop
         pragma Loop_Invariant (I >= 2);
         pragma Loop_Invariant (for all K in 2 .. I => Is_Word (Text (K)));
         I := I + 1;
      end loop;
      if I = 2 or else I > Text'Last or else Text (I) /= ':' then
         return;
      end if;
      Authority_Last := I - 1;
      Rest := I + 1;
      OK := True;
   end Split;

   procedure Next_Field
     (Text : String; From : Positive; First : out Positive;
      Last : out Natural; Next : out Positive; Bracketed : out Boolean;
      At_End : out Boolean; OK : out Boolean)
   is
      J : Positive := From;
   begin
      First := From;
      Last := From - 1;
      Next := From;
      Bracketed := False;
      At_End := True;
      OK := False;
      if From > Text'Last then
         OK := True;   --  an empty last field
         return;
      end if;
      if Text (From) = '[' then
         J := From + 1;
         while J <= Text'Last and then Text (J) /= ']' loop
            pragma Loop_Invariant (J > From);
            J := J + 1;
         end loop;
         if J > Text'Last then
            return;   --  no closing bracket
         end if;
         First := From + 1;
         Last := J - 1;
         Bracketed := True;
         if J = Text'Last then
            Next := Text'Last + 1;
         elsif Text (J + 1) = ':' then
            Next := J + 2;
            At_End := False;
         else
            return;   --  text after the bracket
         end if;
         OK := Next <= Text'Last + 1;
         return;
      end if;
      while J <= Text'Last and then Text (J) /= ':' loop
         pragma Loop_Invariant (J >= From);
         pragma Loop_Invariant (for all K in From .. J => Text (K) /= ':');
         J := J + 1;
      end loop;
      Last := J - 1;
      if J > Text'Last then
         Next := Text'Last + 1;
      else
         Next := J + 1;
         At_End := False;
      end if;
      OK := True;
   end Next_Field;

   procedure Parse_Port
     (Field : String; Port : out Port_Number; OK : out Boolean)
   is
      Value : Natural range 0 .. 99_999 := 0;
   begin
      Port := Port_Number'First;
      OK := False;
      if Field'Length not in 1 .. 5 then
         return;
      end if;
      for I in Field'Range loop
         pragma Loop_Invariant (I - Field'First <= 4);
         pragma Loop_Invariant
           (Value <= (case I - Field'First is
                        when 0 => 0, when 1 => 9, when 2 => 99,
                        when 3 => 999, when others => 9_999));
         if not Is_Digit (Field (I)) then
            return;
         end if;
         Value := Value * 10 + Digit (Field (I));
      end loop;
      if Value in Port_Number then
         Port := Value;
         OK := True;
      end if;
   end Parse_Port;

   procedure Parse_IPv4
     (Field : String; Result : out CuBit.Net_Address.Address; OK : out Boolean)
   is
      type Octets is array (0 .. 3) of Unsigned_8;
      Parts : Octets := [others => 0];
      Octet : Natural range 0 .. 999 := 0;
      Count : Natural range 0 .. 3 := 0;   --  digits in this octet
      Index : Natural range 0 .. 3 := 0;
   begin
      Result := Unspecified;
      OK := False;
      for I in Field'Range loop
         pragma Loop_Invariant
           (Octet <= (case Count is when 0 => 0, when 1 => 9, when 2 => 99,
                                   when others => 999));
         if Field (I) = '.' then
            if Count = 0 or else Octet > 255 or else Index = 3 then
               return;
            end if;
            Parts (Index) := Unsigned_8 (Octet);
            Index := Index + 1;
            Octet := 0;
            Count := 0;
         elsif Is_Digit (Field (I)) and then Count < 3 and then
           not (Count > 0 and then Octet = 0)
         then
            --  No leading zeros: "01" could be read as octal.
            Octet := Octet * 10 + Digit (Field (I));
            Count := Count + 1;
         else
            return;
         end if;
      end loop;
      if Index = 3 and then Count > 0 and then Octet <= 255 then
         Parts (3) := Unsigned_8 (Octet);
         Result := Mapped
           (Shift_Left (Unsigned_32 (Parts (0)), 24) or
            Shift_Left (Unsigned_32 (Parts (1)), 16) or
            Shift_Left (Unsigned_32 (Parts (2)), 8) or
            Unsigned_32 (Parts (3)));
         OK := True;
      end if;
   end Parse_IPv4;

   procedure Parse_IPv6
     (Field : String; Result : out CuBit.Net_Address.Address; OK : out Boolean)
   with Pre => Field'Last < Natural'Last and then
               Field'Length <= Maximum_Length;

   procedure Parse_IPv6
     (Field : String; Result : out CuBit.Net_Address.Address; OK : out Boolean)
   is
      subtype Group_Count is Natural range 0 .. 8;
      type Groups is array (0 .. 7) of Unsigned_16;
      Head, Tail : Groups := [others => 0];
      Head_N, Tail_N : Group_Count := 0;
      Double : Boolean := False;
      I : Integer := Field'First;
      J : Integer;
      Value : Natural range 0 .. 16#FFFF#;

      procedure Add (V : Unsigned_16; Room : out Boolean)
      with Post => Room = (Head_N'Old + Tail_N'Old < 8) and then
                   (if Room then Head_N + Tail_N = Head_N'Old + Tail_N'Old + 1
                    else Head_N = Head_N'Old and Tail_N = Tail_N'Old);

      procedure Add (V : Unsigned_16; Room : out Boolean) is
      begin
         Room := Head_N + Tail_N < 8;
         if not Room then
            return;
         end if;
         if Double then
            Tail (Tail_N) := V;
            Tail_N := Tail_N + 1;
         else
            Head (Head_N) := V;
            Head_N := Head_N + 1;
         end if;
      end Add;

      Room : Boolean;
   begin
      Result := Unspecified;
      OK := False;
      if Field'Length < 2 then
         return;
      end if;
      if Field (Field'First) = ':' then
         if Field (Field'First + 1) /= ':' then
            return;   --  a single leading ':'
         end if;
         Double := True;
         I := Field'First + 2;
      end if;
      while I <= Field'Last loop
         pragma Loop_Invariant (I >= Field'First);
         pragma Loop_Invariant (Head_N + Tail_N <= 8);
         pragma Loop_Variant (Increases => I);
         --  One group of 1 .. 4 hex digits.
         J := I;
         Value := 0;
         while J <= Field'Last and then J - I < 4 and then
           Is_Hex (Field (J))
         loop
            pragma Loop_Invariant (J >= I and then J - I < 4);
            pragma Loop_Invariant
              (Value <= (case J - I is when 0 => 0, when 1 => 16#F#,
                                       when 2 => 16#FF#,
                                       when others => 16#FFF#));
            Value := Value * 16 + Hex (Field (J));
            J := J + 1;
         end loop;
         if J = I then
            return;   --  an empty group
         end if;
         if J <= Field'Last and then Field (J) = '.' then
            --  A dotted IPv4 tail: the last two groups.
            declare
               V4 : CuBit.Net_Address.Address;
               V4_OK : Boolean;
            begin
               Parse_IPv4 (Field (I .. Field'Last), V4, V4_OK);
               if not V4_OK or else Head_N + Tail_N > 6 then
                  return;
               end if;
               Add (Shift_Left (Unsigned_16 (V4 (12)), 8) or
                    Unsigned_16 (V4 (13)), Room);
               Add (Shift_Left (Unsigned_16 (V4 (14)), 8) or
                    Unsigned_16 (V4 (15)), Room);
               I := Field'Last + 1;
            end;
            exit;
         end if;
         Add (Unsigned_16 (Value), Room);
         if not Room then
            return;
         end if;
         if J > Field'Last then
            I := J;
            exit;
         end if;
         if Field (J) /= ':' or else J = Field'Last then
            --  A fifth hex digit, a stray character, or a trailing ':'.
            return;
         end if;
         if Field (J + 1) = ':' then
            if Double then
               return;   --  a second "::"
            end if;
            Double := True;
            I := J + 2;
         else
            I := J + 1;
         end if;
      end loop;
      if (Double and then Head_N + Tail_N > 7) or else
        (not Double and then Head_N + Tail_N /= 8)
      then
         return;
      end if;
      --  Head at the front, Tail at the back, zeros between.
      for K in 0 .. Head_N - 1 loop
         Result (2 * K) := Unsigned_8 (Shift_Right (Head (K), 8));
         Result (2 * K + 1) := Unsigned_8 (Head (K) and 16#FF#);
      end loop;
      for K in 0 .. Tail_N - 1 loop
         pragma Loop_Invariant (Head_N + Tail_N <= 8);
         Result (2 * (8 - Tail_N + K)) :=
           Unsigned_8 (Shift_Right (Tail (K), 8));
         Result (2 * (8 - Tail_N + K) + 1) := Unsigned_8 (Tail (K) and 16#FF#);
      end loop;
      OK := True;
   end Parse_IPv6;

   procedure Parse_Address
     (Field : String; Result : out CuBit.Net_Address.Address; OK : out Boolean)
   is
   begin
      if (for some C of Field => C = ':') then
         Parse_IPv6 (Field, Result, OK);
      else
         Parse_IPv4 (Field, Result, OK);
      end if;
   end Parse_Address;

end CuBit.Locators;
