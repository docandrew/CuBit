------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Net_Manager_Report with SPARK_Mode is

   Byte_Mask : constant Unsigned_32 := 16#FF#;
   Half_Mask : constant Unsigned_32 := 16#FFFF#;
   Kind_Shift      : constant := 8;
   Condition_Shift : constant := 16;
   Policy_Shift    : constant := 24;
   Metric_Shift    : constant := 16;
   Applies_Shift   : constant := 16;
   Total_Shift     : constant := 16;
   Characters_Per_Word : constant := 4;
   Name_Words : constant := NL.Name_Characters / Characters_Per_Word;
   pragma Compile_Time_Error
     (Name_Words * Characters_Per_Word /= NL.Name_Characters, "whole name words");

   --  Record word offsets.
   Head_At     : constant := 0;
   Name_At     : constant := 1;
   Address_At  : constant := 5;
   Prefix_At   : constant := 6;
   Gateway_At  : constant := 7;
   MAC_Low_At  : constant := 8;
   MAC_High_At : constant := 9;
   DHCP_At     : constant := 10;
   pragma Compile_Time_Error (Name_At + Name_Words /= Address_At, "the name precedes the address");

   function Record_At (I : Record_Index) return Word_Index is (Header_Words + (I - 1) * Record_Words)
   with Post => Record_At'Result + Record_Words - 1 <= Word_Index'Last;

   function Character_Of (W : Unsigned_32; K : Natural) return Character is
     (Character'Val (Natural (Shift_Right (W, 8 * K) and Byte_Mask)))
   with Pre => K < Characters_Per_Word;

   function Encode (R : Report) return Words is
      W : Words := [others => 0];
   begin
      W (0) := R.Generation;
      W (1) := Unsigned_32 (R.Count) or Shift_Left (Unsigned_32 (R.Total), Total_Shift);
      W (2) := R.DNS_1;
      W (3) := R.DNS_2;
      W (4) := Unsigned_32 (DNS_Source'Enum_Rep (R.DNS_From));
      for I in 1 .. R.Count loop
         pragma Loop_Invariant (W (0) = R.Generation);
         declare
            S : Interface_Status renames R.Interfaces (I);
            B : constant Word_Index := Record_At (I);
         begin
            W (B + Head_At) := Unsigned_32 (S.Name.Length)
              or Shift_Left (Unsigned_32 (NL.Link_Kind'Enum_Rep (S.Kind)), Kind_Shift)
              or Shift_Left (Unsigned_32 (NL.Link_Condition'Enum_Rep (S.Condition)), Condition_Shift)
              or Shift_Left (Unsigned_32 (Policy'Enum_Rep (S.Setting)), Policy_Shift);
            for C in 1 .. S.Name.Length loop
               pragma Loop_Invariant (W (0) = R.Generation);
               W (B + Name_At + (C - 1) / Characters_Per_Word) :=
                 W (B + Name_At + (C - 1) / Characters_Per_Word)
                 or Shift_Left (Unsigned_32 (Character'Pos (S.Name.Text (C))),
                                8 * ((C - 1) mod Characters_Per_Word));
            end loop;
            W (B + Address_At) := S.Settings.Address;
            W (B + Prefix_At) := Unsigned_32 (S.Settings.Prefix)
              or Shift_Left (Unsigned_32 (S.Settings.Metric), Metric_Shift);
            W (B + Gateway_At) := S.Settings.Gateway;
            W (B + MAC_Low_At) := Unsigned_32 (S.MAC (0)) or Shift_Left (Unsigned_32 (S.MAC (1)), 8)
              or Shift_Left (Unsigned_32 (S.MAC (2)), 16) or Shift_Left (Unsigned_32 (S.MAC (3)), 24);
            W (B + MAC_High_At) := Unsigned_32 (S.MAC (4)) or Shift_Left (Unsigned_32 (S.MAC (5)), 8);
            W (B + DHCP_At) := Unsigned_32 (DHCP_Phase'Enum_Rep (S.DHCP))
              or Shift_Left (S.Applies, Applies_Shift);
         end;
      end loop;
      return W;
   end Encode;

   function Decode (W : Words) return Decoded_Report is
      R : Report;
      Count_Value : constant Unsigned_32 := W (1) and Half_Mask;
   begin
      if Count_Value > Maximum_Records
        or else Count_Value > Shift_Right (W (1), Total_Shift)
        or else W (4) > Unsigned_32 (DNS_Source'Enum_Rep (DNS_Source'Last))
      then
         return (Valid => False);
      end if;
      R.Generation := W (0);
      R.Count := Natural (Count_Value);
      R.Total := Natural (Shift_Right (W (1), Total_Shift));
      R.DNS_1 := W (2);
      R.DNS_2 := W (3);
      R.DNS_From := DNS_Source'Enum_Val (W (4));
      for I in 1 .. R.Count loop
         declare
            B : constant Word_Index := Record_At (I);
            Head : constant Unsigned_32 := W (B + Head_At);
            Length_Value : constant Unsigned_32 := Head and Byte_Mask;
            Kind_Value : constant Unsigned_32 := Shift_Right (Head, Kind_Shift) and Byte_Mask;
            Condition_Value : constant Unsigned_32 := Shift_Right (Head, Condition_Shift) and Byte_Mask;
            Policy_Value : constant Unsigned_32 := Shift_Right (Head, Policy_Shift);
            Prefix_Value : constant Unsigned_32 := W (B + Prefix_At) and Byte_Mask;
            Phase_Value : constant Unsigned_32 := W (B + DHCP_At) and Byte_Mask;
            S : Interface_Status;
         begin
            if Length_Value > NL.Name_Characters
              or else Kind_Value not in Unsigned_32 (NL.Link_Kind'Enum_Rep (NL.Link_Kind'First))
                                     .. Unsigned_32 (NL.Link_Kind'Enum_Rep (NL.Link_Kind'Last))
              or else Condition_Value not in Unsigned_32 (NL.Link_Condition'Enum_Rep (NL.Link_Condition'First))
                                          .. Unsigned_32 (NL.Link_Condition'Enum_Rep (NL.Link_Condition'Last))
              or else Policy_Value not in Unsigned_32 (Policy'Enum_Rep (Policy'First))
                                       .. Unsigned_32 (Policy'Enum_Rep (Policy'Last))
              or else Prefix_Value > Unsigned_32 (NM.IPv4_Prefix'Last)
              or else Phase_Value > Unsigned_32 (DHCP_Phase'Enum_Rep (DHCP_Phase'Last))
            then
               return (Valid => False);
            end if;
            S.Name.Length := Natural (Length_Value);
            for C in 1 .. NL.Name_Characters loop
               S.Name.Text (C) :=
                 (if C <= S.Name.Length
                  then Character_Of (W (B + Name_At + (C - 1) / Characters_Per_Word), (C - 1) mod Characters_Per_Word)
                  else ' ');
            end loop;
            S.Kind := NL.Link_Kind'Enum_Val (Kind_Value);
            S.Condition := NL.Link_Condition'Enum_Val (Condition_Value);
            S.Setting := Policy'Enum_Val (Policy_Value);
            S.Settings := (Address => W (B + Address_At),
                           Prefix  => NM.IPv4_Prefix (Prefix_Value),
                           Gateway => W (B + Gateway_At),
                           Metric  => Shift_Right (W (B + Prefix_At), Metric_Shift));
            S.MAC := [0 => Unsigned_8 (W (B + MAC_Low_At) and Byte_Mask),
                      1 => Unsigned_8 (Shift_Right (W (B + MAC_Low_At), 8) and Byte_Mask),
                      2 => Unsigned_8 (Shift_Right (W (B + MAC_Low_At), 16) and Byte_Mask),
                      3 => Unsigned_8 (Shift_Right (W (B + MAC_Low_At), 24)),
                      4 => Unsigned_8 (W (B + MAC_High_At) and Byte_Mask),
                      5 => Unsigned_8 (Shift_Right (W (B + MAC_High_At), 8) and Byte_Mask)];
            S.DHCP := DHCP_Phase'Enum_Val (Phase_Value);
            S.Applies := Shift_Right (W (B + DHCP_At), Applies_Shift);
            R.Interfaces (I) := S;
         end;
      end loop;
      if not Normal (R) or else Encode (R) /= W then
         return (Valid => False);
      end if;
      return (Valid => True, Item => R);
   end Decode;

end CuBit.Net_Manager_Report;
