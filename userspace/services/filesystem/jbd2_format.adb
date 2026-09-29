package body Jbd2_Format with SPARK_Mode is
   --  Reflected Castagnoli polynomial.
   Crc32c_Polynomial : constant Unsigned_32 := 16#82F6_3B78#;
   --  IEEE 802.3 polynomial, most significant bit first.
   Crc32_Polynomial : constant Unsigned_32 := 16#04C1_1DB7#;
   Top_Bit : constant Unsigned_32 := 16#8000_0000#;

   --  Journal superblock field offsets (big-endian words).
   Block_Size_Offset : constant := 16#0C#;
   Max_Length_Offset : constant := 16#10#;
   First_Offset      : constant := 16#14#;
   Sequence_Offset   : constant := 16#18#;
   Start_Offset      : constant := 16#1C#;
   Errno_Offset      : constant := 16#20#;
   Compat_Offset     : constant := 16#24#;
   Incompat_Offset   : constant := 16#28#;
   Ro_Compat_Offset  : constant := 16#2C#;
   UUID_Offset       : constant := 16#30#;
   Users_Offset      : constant := 16#40#;
   Checksum_Type_Offset : constant := 16#50#;

   --  Descriptor tag field offsets.
   Tag_Flags_V3_Offset    : constant := 4;
   Tag_High_Offset        : constant := 8;
   Tag_Checksum_V3_Offset : constant := 12;
   Tag_Checksum_Offset    : constant := 4;
   Tag_Flags_Offset       : constant := 6;

   procedure Decode_Superblock
     (Data : Block; Size : Block_Bytes; Journal_Blocks : Unsigned_32;
      Stored_Checksum_Matches : Boolean;
      Super : out Journal_Superblock; Valid : out Boolean)
   is
   begin
      Super := Empty_Superblock;
      Valid := False;
      if Be32 (Data, 0) /= Magic then
         return;
      end if;
      Super.Kind := Be32 (Data, 4);
      Super.Journal_Block_Bytes := Be32 (Data, Block_Size_Offset);
      Super.Max_Length := Be32 (Data, Max_Length_Offset);
      Super.First := Be32 (Data, First_Offset);
      Super.Sequence := Be32 (Data, Sequence_Offset);
      Super.Start := Be32 (Data, Start_Offset);
      Super.Errno := Be32 (Data, Errno_Offset);
      if Super.Kind = Superblock_V2_Kind then
         Super.Compat := Be32 (Data, Compat_Offset);
         Super.Incompat := Be32 (Data, Incompat_Offset);
         Super.Ro_Compat := Be32 (Data, Ro_Compat_Offset);
         Super.Users := Be32 (Data, Users_Offset);
         Super.Checksum_Type := Data (Checksum_Type_Offset);
         for I in UUID'Range loop
            Super.Identity (I) := Data (UUID_Offset + I);
         end loop;
      elsif Super.Kind /= Superblock_V1_Kind then
         return;
      end if;
      if Super.Journal_Block_Bytes /= Unsigned_32 (Size) or else
        Super.Max_Length > Journal_Blocks or else
        Super.First < 1 or else Super.First >= Super.Max_Length or else
        (Super.Start /= 0 and then
         (Super.Start < Super.First or else Super.Start >= Super.Max_Length)) or else
        (Super.Compat and not Supported_Compat) /= 0 or else
        (Super.Incompat and not Supported_Incompat) /= 0 or else
        Super.Ro_Compat /= 0 or else Super.Users > Maximum_Users or else
        (Checksummed (Super.Incompat) and then
         (Super.Checksum_Type /= Checksum_Type_Crc32c or else
          not Stored_Checksum_Matches))
      then
         return;
      end if;
      Valid := True;
   end Decode_Superblock;

   procedure Next_Tag
     (Data : Block; Limit : Natural; Incompat : Unsigned_32;
      Offset : in out Natural; Tag : out Block_Tag; Found : out Boolean)
   is
      Bytes : constant Tag_Length := Tag_Bytes (Incompat);
      Wide : constant Boolean := Has (Incompat, Incompat_64bit);
   begin
      Tag := (Home => 0, Flags => 0, Checksum => 0);
      Found := False;
      if Limit - Offset < Bytes then
         return;
      end if;
      Tag.Home := Unsigned_64 (Be32 (Data, Offset));
      if Has (Incompat, Incompat_Csum_V3) then
         Tag.Flags := Be32 (Data, Offset + Tag_Flags_V3_Offset);
         Tag.Checksum := Be32 (Data, Offset + Tag_Checksum_V3_Offset);
      else
         Tag.Checksum := Be16 (Data, Offset + Tag_Checksum_Offset);
         Tag.Flags := Be16 (Data, Offset + Tag_Flags_Offset);
      end if;
      if Wide then
         pragma Assert (Bytes >= Tag_High_Offset + 4);
         Tag.Home := Tag.Home or
           Shift_Left (Unsigned_64 (Be32 (Data, Offset + Tag_High_Offset)), 32);
      end if;
      Offset := Offset + Bytes;
      if not Has (Tag.Flags, Tag_Same_UUID) then
         --  A UUID reaching past the limit leaves no room for another tag.
         Offset := Natural'Min (Offset + UUID_Bytes, Limit);
      end if;
      Found := True;
   end Next_Tag;

   function Revoke_Records
     (Data : Block; Size : Block_Bytes; Incompat : Unsigned_32) return Natural
   is
      Area : constant Unsigned_32 := Be32 (Data, Header_Bytes) - Revoke_Header_Bytes;
   begin
      pragma Assert (Be32 (Data, Header_Bytes) <= Unsigned_32 (Size));
      pragma Assert (Area <= Maximum_Block_Bytes - Revoke_Header_Bytes);
      --  A literal divisor per record size keeps the bounds linear.
      if Has (Incompat, Incompat_64bit) then
         pragma Assert (Area / 8 * 8 <= Area);
         return Natural (Area / 8);
      else
         pragma Assert (Area / 4 * 4 <= Area);
         return Natural (Area / 4);
      end if;
   end Revoke_Records;

   function Revoked_Block
     (Data : Block; Size : Block_Bytes; Incompat : Unsigned_32; Index : Natural)
      return Unsigned_64
   is
      Records : constant Natural := Revoke_Records (Data, Size, Incompat);
      Count : constant Natural := Natural (Be32 (Data, Header_Bytes));
   begin
      --  Record sizes are literal per branch: every step stays linear.
      if Has (Incompat, Incompat_64bit) then
         pragma Assert (Revoke_Header_Bytes + Records * 8 <= Count);
         pragma Assert (Revoke_Header_Bytes + (Index + 1) * 8 <= Count);
         declare
            Offset : constant Natural := Revoke_Header_Bytes + Index * 8;
         begin
            return Shift_Left (Unsigned_64 (Be32 (Data, Offset)), 32) or
              Unsigned_64 (Be32 (Data, Offset + 4));
         end;
      else
         pragma Assert (Revoke_Header_Bytes + Records * 4 <= Count);
         pragma Assert (Revoke_Header_Bytes + (Index + 1) * 4 <= Count);
         declare
            Offset : constant Natural := Revoke_Header_Bytes + Index * 4;
         begin
            return Unsigned_64 (Be32 (Data, Offset));
         end;
      end if;
   end Revoked_Block;

   function Crc32c_Byte (Crc : Unsigned_32; Value : Unsigned_8) return Unsigned_32 is
      Result : Unsigned_32 := Crc xor Unsigned_32 (Value);
   begin
      for Bit in 1 .. 8 loop
         Result :=
           (if (Result and 1) /= 0 then Shift_Right (Result, 1) xor Crc32c_Polynomial
            else Shift_Right (Result, 1));
      end loop;
      return Result;
   end Crc32c_Byte;

   function Crc32c (Seed : Unsigned_32; Data : Block; First, Length : Natural)
      return Unsigned_32
   is
      Crc : Unsigned_32 := Seed;
   begin
      for I in First .. First + Length - 1 loop
         Crc := Crc32c_Byte (Crc, Data (I));
      end loop;
      return Crc;
   end Crc32c;

   function Crc32_Be (Seed : Unsigned_32; Data : Block; First, Length : Natural)
      return Unsigned_32
   is
      Crc : Unsigned_32 := Seed;
   begin
      for I in First .. First + Length - 1 loop
         Crc := Crc xor Shift_Left (Unsigned_32 (Data (I)), 24);
         for Bit in 1 .. 8 loop
            Crc := (if (Crc and Top_Bit) /= 0 then Shift_Left (Crc, 1) xor Crc32_Polynomial
                    else Shift_Left (Crc, 1));
         end loop;
      end loop;
      return Crc;
   end Crc32_Be;

   function Crc32c_UUID (Seed : Unsigned_32; Identity : UUID) return Unsigned_32 is
      Crc : Unsigned_32 := Seed;
   begin
      for Value of Identity loop
         Crc := Crc32c_Byte (Crc, Value);
      end loop;
      return Crc;
   end Crc32c_UUID;

   function Crc32c_Be32 (Seed, Value : Unsigned_32) return Unsigned_32 is
     (Crc32c_Byte
        (Crc32c_Byte
           (Crc32c_Byte
              (Crc32c_Byte (Seed, Unsigned_8 (Shift_Right (Value, 24))),
               Unsigned_8 (Shift_Right (Value, 16) and 16#FF#)),
            Unsigned_8 (Shift_Right (Value, 8) and 16#FF#)),
         Unsigned_8 (Value and 16#FF#)));
end Jbd2_Format;
