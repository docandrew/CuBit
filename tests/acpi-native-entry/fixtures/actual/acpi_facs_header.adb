pragma Ada_2022;
package body ACPI_FACS_Header with SPARK_Mode is
   use Interfaces;
   function Decode_Prefix (Data : Firmware_Tables.Bytes) return Description is
      Length_Offset : constant Natural := 4;
      Octet_Bits : constant Positive := 8;
      Dword_Octets : constant Positive := 4;
      Length : Unsigned_32 := 0;
   begin
      if Data'Length < Minimum_Length then return (others => <>); end if;
      if Data (Data'First) /= Character'Pos ('F')
        or else Data (Data'First + 1) /= Character'Pos ('A')
        or else Data (Data'First + 2) /= Character'Pos ('C')
        or else Data (Data'First + 3) /= Character'Pos ('S')
      then return (Status => Bad_Signature, others => <>); end if;
      for I in 0 .. Dword_Octets - 1 loop
         Length := Length or Shift_Left
           (Unsigned_32 (Data (Data'First + Length_Offset + I)), I * Octet_Bits);
      end loop;
      if Length < Unsigned_32 (Minimum_Length) then return (Status => Bad_Length, others => <>); end if;
      return (Recognized_Header, Length, Data (Data'First + Version_Offset));
   end Decode_Prefix;
end ACPI_FACS_Header;
