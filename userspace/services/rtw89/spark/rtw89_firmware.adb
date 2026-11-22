------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body RTW89_Firmware with SPARK_Mode is

   function LE32 (File : Byte_Array; At_Index : File_Index) return Unsigned_32 is
     (Unsigned_32 (File (At_Index))
      or Shift_Left (Unsigned_32 (File (At_Index + 1)), 8)
      or Shift_Left (Unsigned_32 (File (At_Index + 2)), 16)
      or Shift_Left (Unsigned_32 (File (At_Index + 3)), 24))
   with Pre => File'First = 0 and then File'Length >= 4
               and then At_Index <= File'Length - 4;

   function Code (Kind : Image_Type) return Unsigned_8 is
     (case Kind is when Normal => Normal_Code, when Normal_CE => Normal_CE_Code);

   function Select_Image (File : Byte_Array; Kind : Image_Type; Cut : Unsigned_8)
     return Image
   is
      Count  : Natural;
      Best   : Image;
      Best_Cut : Unsigned_8 := 0;
      Base   : Natural;
      Shift, Size : Unsigned_32;
   begin
      if File'Length < MFW_Header_Bytes then
         return (others => <>);
      end if;

      if File (0) /= MFW_Signature then
         --  Legacy single image: only the Normal type.
         if Kind = Normal then
            return (Found => True, Offset => 0, Size => File'Length);
         end if;
         return (others => <>);
      end if;

      Count := Natural (File (1));
      if Count = 0 or else MFW_Header_Bytes + MFW_Entry_Bytes * Count > File'Length then
         return (others => <>);
      end if;

      for E in 0 .. Count - 1 loop
         pragma Loop_Invariant
           (MFW_Header_Bytes + MFW_Entry_Bytes * Count <= File'Length
            and then (if Best.Found then
                        Best.Size > 0 and then Best.Offset + Best.Size <= File'Length));
         Base := MFW_Header_Bytes + MFW_Entry_Bytes * E;
         if File (Base + 1) = Code (Kind)
           and then File (Base) <= Cut
           and then File (Base + 2) = 0
           and then (not Best.Found or else File (Base) > Best_Cut)
         then
            Shift := LE32 (File, Base + 4);
            Size := LE32 (File, Base + 8);
            if Long_Long_Integer (Size) > 0
              and then Long_Long_Integer (Shift) + Long_Long_Integer (Size)
                       <= Long_Long_Integer (File'Length)
            then
               Best := (Found  => True,
                        Offset => Natural (Long_Long_Integer (Shift)),
                        Size   => Natural (Long_Long_Integer (Size)));
               Best_Cut := File (Base);
            end if;
         end if;
      end loop;
      return Best;
   end Select_Image;

   function Plan_Image (File : Byte_Array; Where : Image) return Plan is
      Result : Plan;
      Image_End : constant Natural := Where.Offset + Where.Size;
      Count  : Natural;
      Base_Header : Natural;
      Header_Length : Natural;
      Total_Length : Natural;
      Dynamic_Length : Unsigned_32;
      Cursor : Natural;
      W0, W1, W2 : Unsigned_32;
      Length, Signatures : Long_Long_Integer;
      Row    : Natural;
      Dynamic_Header_Bit : constant Unsigned_32 := 2 ** 16;
      Checksum_Bit : constant Unsigned_32 := 2 ** 28;
      Type_Shift : constant := 24;
      Type_Mask  : constant Unsigned_32 := 16#0F#;
      Byte_Mask  : constant Unsigned_32 := 16#FF#;
   begin
      Result.Header := Where.Offset;
      if Where.Size < Header_Fixed_Bytes then
         return Result;
      end if;

      --  w3[31:24]: header version; only v0 exists for the 8852B.
      if File (Where.Offset + 15) /= 0 then
         Result.Status := Unsupported_Version;
         return Result;
      end if;

      Count := Natural (File (Where.Offset + 25));          --  w6[15:8]
      if Count < 1 or else Count > Maximum_Sections then
         Result.Status := Bad_Section_Count;
         return Result;
      end if;
      Base_Header := Header_Fixed_Bytes + Section_Header_Bytes * Count;
      if Base_Header > Where.Size then
         return Result;
      end if;

      Header_Length := Base_Header;
      if (LE32 (File, Where.Offset + 28) and Dynamic_Header_Bit) /= 0 then
         Total_Length := Natural (File (Where.Offset + 14));  --  w3[23:16]
         if Total_Length > Base_Header + 3 and then Total_Length <= Where.Size then
            Dynamic_Length := LE32 (File, Where.Offset + Base_Header);
            if Dynamic_Length = Unsigned_32 (Total_Length - Base_Header) then
               Header_Length := Total_Length;
            end if;
         end if;
      end if;

      --  The FWHDR_DL H2C carries the fixed header and section headers;
      --  the dynamic header is not sent.
      Result.Header_Bytes := Base_Header;
      Cursor := Where.Offset + Header_Length;

      for S in 1 .. Count loop
         pragma Loop_Invariant
           (Cursor <= Image_End
            and then Result.Sections = S - 1
            and then Result.Header_Bytes = Base_Header
            and then Result.Header = Where.Offset
            and then (for all I in 1 .. S - 1 =>
                        Result.Items (I).Data + Result.Items (I).Length <= Image_End));
         Row := Where.Offset + Header_Fixed_Bytes + Section_Header_Bytes * (S - 1);
         W0 := LE32 (File, Row);
         W1 := LE32 (File, Row + 4);
         W2 := LE32 (File, Row + 8);
         Length := Long_Long_Integer (W1 and Section_Size_Mask)
           + (if (W1 and Checksum_Bit) /= 0 then Checksum_Bytes else 0);
         Signatures := 0;
         if Unsigned_8 (Shift_Right (W1, Type_Shift) and Type_Mask) = Security_Section then
            if (W2 and Byte_Mask) = Formatted_MSSC then
               Result.Status := Formatted_Signatures;
               return Result;
            end if;
            Signatures := Long_Long_Integer (W2) * Signature_Bytes;
         end if;
         if Length = 0 then
            Result.Status := Empty_Section;
            return Result;
         elsif Length + Signatures > Long_Long_Integer (Image_End - Cursor) then
            Result.Status := Section_Overruns_Image;
            return Result;
         end if;
         declare
            Payload : constant Natural := Natural (Length);
            Step    : constant Natural := Natural (Length + Signatures);
         begin
            pragma Assert (Payload <= Step and then Step <= Image_End - Cursor);
            Result.Items (S) :=
              (Data    => Cursor,
               Length  => Payload,
               Address => W0 and Download_Address_Mask,
               Kind    => Unsigned_8 (Shift_Right (W1, Type_Shift) and Type_Mask));
            Result.Sections := S;
            Cursor := Cursor + Step;
         end;
      end loop;

      if Cursor /= Image_End then
         Result.Status := Size_Mismatch;
         return Result;
      end if;
      Result.Status := Planned;
      return Result;
   end Plan_Image;

end RTW89_Firmware;
