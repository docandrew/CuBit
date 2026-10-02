pragma Ada_2022;
-- Fixed-width table identifiers. Extraction is not checksum admission, and a
-- match confers no memory or hardware authority. ASL string conversion is a
-- separate interpreter operation, not implicit padding/trimming here.
package Firmware_Tables.Identifiers with SPARK_Mode, Pure is
   use type Byte;
   subtype OEM_Name is String (1 .. 6);
   subtype OEM_Table_Name is String (1 .. 8);
   type Identity is record
      Name : Signature;
      OEM : OEM_Name;
      OEM_Table : OEM_Table_Name;
   end record;
   type Selection is record
      Name : Signature;
      Match_OEM : Boolean := False;
      OEM : OEM_Name := [others => Character'Val (0)];
      Match_OEM_Table : Boolean := False;
      OEM_Table : OEM_Table_Name := [others => Character'Val (0)];
   end record;
   function Read_Identity (Data : Bytes) return Identity with
     Pre => Data'Length >= Table_Header_Size,
     Post => (for all I in 1 .. 4 =>
       Character'Pos (Read_Identity'Result.Name (I)) = Data (Data'First + I - 1))
       and then (for all I in 1 .. 6 =>
         Character'Pos (Read_Identity'Result.OEM (I)) = Data (Data'First + 9 + I))
       and then (for all I in 1 .. 8 =>
         Character'Pos (Read_Identity'Result.OEM_Table (I)) = Data (Data'First + 15 + I));
   function Matches (Actual : Identity; Requested : Selection) return Boolean is
     (Actual.Name = Requested.Name
       and then (not Requested.Match_OEM or else Actual.OEM = Requested.OEM)
       and then (not Requested.Match_OEM_Table or else Actual.OEM_Table = Requested.OEM_Table));
end Firmware_Tables.Identifiers;
