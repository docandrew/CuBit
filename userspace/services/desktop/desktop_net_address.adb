------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Desktop_Net_Address with SPARK_Mode is

   Octets         : constant := 4;
   Largest_Octet  : constant := 255;
   Longest_Octet  : constant := 3;     --  decimal digits
   Address_Bits   : constant := 32;
   --  First octets that are no host's address: "this network", loopback,
   --  and multicast and above (RFC 6890).
   This_Network_Octet : constant := 0;
   Loopback_Octet     : constant := 127;
   First_Multicast    : constant := 224;

   function Is_Digit (C : Character) return Boolean is (C in '0' .. '9');

   procedure Parse_Address (Text : String; Value : out IPv4_Word; OK : out Boolean) is
      Parts : array (0 .. Octets - 1) of Natural := [others => 0];
      Part : Natural := 0;
      Digits_In_Part : Natural := 0;
      Leading_Zero : Boolean := False;
   begin
      Value := No_Address;
      OK := False;
      if Text'Length = 0 or else Text'Length > Maximum_Field then
         return;
      end if;
      for I in Text'Range loop
         pragma Loop_Invariant (Part < Octets and then Digits_In_Part <= Longest_Octet
                                and then (for all P of Parts => P <= Largest_Octet));
         if Is_Digit (Text (I)) then
            if Digits_In_Part = Longest_Octet or else Leading_Zero then
               return;
            end if;
            Leading_Zero := Digits_In_Part = 0 and then Text (I) = '0';
            if Parts (Part) * 10 + (Character'Pos (Text (I)) - Character'Pos ('0')) > Largest_Octet then
               return;
            end if;
            Parts (Part) := Parts (Part) * 10 + (Character'Pos (Text (I)) - Character'Pos ('0'));
            Digits_In_Part := Digits_In_Part + 1;
         elsif Text (I) = '.' then
            if Digits_In_Part = 0 or else Part = Octets - 1 then
               return;
            end if;
            Part := Part + 1;
            Digits_In_Part := 0;
            Leading_Zero := False;
         else
            return;
         end if;
      end loop;
      if Part /= Octets - 1 or else Digits_In_Part = 0 then
         return;
      end if;
      Value := Word_Of (Parts (0), Parts (1), Parts (2), Parts (3));
      OK := True;
   end Parse_Address;

   procedure Parse_Address_Prefix
     (Text : String; Value : out IPv4_Word; Length : out Prefix_Length; OK : out Boolean)
   is
      Slash : Natural := 0;
      Bits : Natural := 0;
   begin
      Value := No_Address;
      Length := 0;
      OK := False;
      for I in Text'Range loop
         if Text (I) = '/' then
            Slash := I;
            exit;
         end if;
      end loop;
      if Slash = 0 or else Slash = Text'Last or else Text'Last - Slash > 2 then
         return;
      end if;
      for I in Slash + 1 .. Text'Last loop
         pragma Loop_Invariant (Bits <= Address_Bits);
         if not Is_Digit (Text (I)) or else (I > Slash + 1 and then Text (Slash + 1) = '0') then
            return;
         end if;
         Bits := Bits * 10 + (Character'Pos (Text (I)) - Character'Pos ('0'));
         if Bits > Address_Bits then
            return;
         end if;
      end loop;
      Parse_Address (Text (Text'First .. Slash - 1), Value, OK);
      if OK then
         Length := Bits;
      end if;
   end Parse_Address_Prefix;

   procedure Parse_DNS (Text : String; Primary, Secondary : out IPv4_Word; OK : out Boolean) is
      Found : Natural := 0;
   begin
      Primary := No_Address;
      Secondary := No_Address;
      OK := True;
      if Text'Length = 0 then
         return;
      end if;
      declare
      I : Positive := Text'First;
      begin
      while I <= Text'Last loop
         pragma Loop_Invariant (I in Text'Range and then Found <= 2);
         pragma Loop_Variant (Increases => I);
         if Text (I) in ' ' | ',' then
            I := I + 1;
         else
            declare
               Last : Natural := I;
               W : IPv4_Word;
               Parsed : Boolean;
            begin
               while Last < Text'Last and then Text (Last + 1) not in ' ' | ',' loop
                  pragma Loop_Invariant (Last in I .. Text'Last - 1);
                  pragma Loop_Variant (Increases => Last);
                  Last := Last + 1;
               end loop;
               Parse_Address (Text (I .. Last), W, Parsed);
               if not Parsed or else Found = 2 then
                  OK := False;
                  Primary := No_Address;
                  Secondary := No_Address;
                  return;
               end if;
               Found := Found + 1;
               if Found = 1 then
                  Primary := W;
               else
                  Secondary := W;
               end if;
               exit when Last = Text'Last;
               I := Last + 1;
            end;
         end if;
      end loop;
      end;
   end Parse_DNS;

   function Decimal (N : Natural) return String
   with Pre => N <= Largest_Octet, Post => Decimal'Result'Length in 1 .. Longest_Octet;

   function Decimal (N : Natural) return String is
      Digit : constant String (1 .. 10) := "0123456789";
   begin
      if N >= 100 then
         return [Digit (N / 100 + 1), Digit (N / 10 mod 10 + 1), Digit (N mod 10 + 1)];
      elsif N >= 10 then
         return [Digit (N / 10 + 1), Digit (N mod 10 + 1)];
      else
         return [1 => Digit (N + 1)];
      end if;
   end Decimal;

   function Image (W : IPv4_Word) return String is
      Longest : constant := 15;
      Result : String (1 .. Longest) := [others => ' '];
      Length : Natural := 0;
   begin
      for A in 0 .. Octets - 1 loop
         pragma Loop_Invariant (Length in (if A = 0 then 0 else 2 * A - 1) .. (if A = 0 then 0 else 4 * A - 1));
         if A > 0 then
            Length := Length + 1;
            Result (Length) := '.';
         end if;
         declare
            D : constant String := Decimal (Octet (W, A));
            Start : constant Natural := Length;
         begin
            for K in D'Range loop
               pragma Loop_Invariant (Length = Start + (K - D'First));
               Length := Length + 1;
               Result (Length) := D (K);
            end loop;
            pragma Assert (Length = Start + D'Length);
         end;
      end loop;
      return Result (1 .. Length);
   end Image;

   --  The address as one number, the first octet most significant.
   function Ordered (W : IPv4_Word) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (Octet (W, 0)), 24) or Shift_Left (Unsigned_32 (Octet (W, 1)), 16)
      or Shift_Left (Unsigned_32 (Octet (W, 2)), 8) or Unsigned_32 (Octet (W, 3)));

   function Unordered (N : Unsigned_32) return IPv4_Word is
     (Word_Of (Natural (Shift_Right (N, 24)), Natural (Shift_Right (N, 16) and 16#FF#),
               Natural (Shift_Right (N, 8) and 16#FF#), Natural (N and 16#FF#)));

   function Mask (Length : Prefix_Length) return Unsigned_32 is
     (if Length = 0 then 0 else Shift_Left (Unsigned_32'Last, Address_Bits - Length));

   function Network_Of (W : IPv4_Word; Length : Prefix_Length) return IPv4_Word is
     (Unordered (Ordered (W) and Mask (Length)));

   function Host_Address (W : IPv4_Word) return Boolean is
     (W /= No_Address and then Octet (W, 0) /= This_Network_Octet and then Octet (W, 0) /= Loopback_Octet
      and then Octet (W, 0) < First_Multicast);

   function Check (S : Static_Choice) return Problem is
      Host : constant Unsigned_32 := Ordered (S.Address) and not Mask (S.Length);
   begin
      if not Host_Address (S.Address) then
         return Address_Reserved;
      elsif S.Length < Shortest_Prefix then
         return Prefix_Too_Short;
      elsif S.Length <= Longest_Subnet and then Host = 0 then
         return Address_Is_Network;
      elsif S.Length <= Longest_Subnet and then Host = not Mask (S.Length) then
         return Address_Is_Broadcast;
      elsif S.Gateway /= No_Address and then not Host_Address (S.Gateway) then
         return Gateway_Malformed;
      elsif S.Gateway /= No_Address and then S.Gateway = S.Address then
         return Gateway_Is_Address;
      elsif S.Gateway /= No_Address and then not Same_Network (S.Address, S.Gateway, S.Length) then
         return Gateway_Outside;
      end if;
      return None;
   end Check;

end Desktop_Net_Address;
