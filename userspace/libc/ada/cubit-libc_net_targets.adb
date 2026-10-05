------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Libc_Net_Targets with SPARK_Mode is

   --  The decimal digits of Value into Result, least significant last.
   procedure Decimal (Value : Unsigned_32; Result : out String; Length : out Natural)
   with Pre => Result'First = 1 and then Result'Length >= 10,
        Post => Length in 1 .. 10;

   procedure Decimal (Value : Unsigned_32; Result : out String; Length : out Natural) is
      Digits_Text : String (1 .. 10) := [others => '0'];
      First : Positive range 1 .. 10 := 10;
      V : Unsigned_32 := Value;
   begin
      Result := [others => '0'];
      loop
         Digits_Text (First) := Character'Val (Character'Pos ('0') + Natural (V mod 10));
         V := V / 10;
         exit when V = 0 or else First = 1;
         First := First - 1;
      end loop;
      Length := 10 - First + 1;
      Result (1 .. Length) := Digits_Text (First .. 10);
   end Decimal;

   procedure Dotted (IPv4 : CuBit.Libc_Net_Addresses.Octets;
                     Result : out String; Length : out Natural)
   is
      function Digit (Value : Unsigned_8) return Character is
        (Character'Val (Character'Pos ('0') + Natural (Value mod 10)));
   begin
      Result := [others => ' '];
      Length := 0;
      for I in IPv4'Range loop
         declare
            B : constant Unsigned_8 := IPv4 (I);
         begin
            if B >= 100 then
               Length := Length + 1;
               Result (Length) := Digit (B / 100);
            end if;
            if B >= 10 then
               Length := Length + 1;
               Result (Length) := Digit (B / 10);
            end if;
            Length := Length + 1;
            Result (Length) := Digit (B);
            if I < 4 then
               Length := Length + 1;
               Result (Length) := '.';
            end if;
         end;
         pragma Loop_Invariant (Length >= 2 * I - (if I = 4 then 1 else 0)
                                and then Length <= 4 * I - (if I = 4 then 1 else 0));
      end loop;
   end Dotted;

   procedure Format
     (Prefix, Host : String; Port : Unsigned_16;
      Result : out Target; Length : out Target_Length; Fits : out Boolean)
   is
      Port_Text : String (1 .. 10);
      Port_Length : Natural;
      Total : Natural;
   begin
      Result := [others => ' '];
      Length := 0;
      Decimal (Unsigned_32 (Port), Port_Text, Port_Length);
      Total := Prefix'Length + Host'Length + 1 + Port_Length;
      Fits := Total <= Maximum_Target;
      if not Fits then
         return;
      end if;
      Result (1 .. Prefix'Length) := Prefix;
      Result (Prefix'Length + 1 .. Prefix'Length + Host'Length) := Host;
      Result (Prefix'Length + Host'Length + 1) := ':';
      Result (Prefix'Length + Host'Length + 2 .. Total) := Port_Text (1 .. Port_Length);
      Length := Total;
   end Format;

end CuBit.Libc_Net_Targets;
