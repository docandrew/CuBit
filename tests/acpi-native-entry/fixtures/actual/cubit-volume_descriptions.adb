pragma Ada_2022;

package body CuBit.Volume_Descriptions with SPARK_Mode is

   Byte_Bits : constant := 8;

   procedure Put (Into : in out Record_Image; At_Byte : Record_Index; Value : Unsigned_64; Width : Positive)
   with Pre => Width <= 8 and then At_Byte <= Record_Bytes - Width;
   procedure Put (Into : in out Record_Image; At_Byte : Record_Index; Value : Unsigned_64; Width : Positive) is
   begin
      for K in 0 .. Width - 1 loop
         Into (At_Byte + K) := Unsigned_8 (Shift_Right (Value, K * Byte_Bits) and 16#FF#);
      end loop;
   end Put;

   function Get (From : Record_Image; At_Byte : Record_Index; Width : Positive) return Unsigned_64
   with Pre => Width <= 8 and then At_Byte <= Record_Bytes - Width;
   function Get (From : Record_Image; At_Byte : Record_Index; Width : Positive) return Unsigned_64 is
      Value : Unsigned_64 := 0;
   begin
      for K in reverse 0 .. Width - 1 loop
         Value := Shift_Left (Value, Byte_Bits) or Unsigned_64 (From (At_Byte + K));
      end loop;
      return Value;
   end Get;

   procedure Encode (Item : Description; Into : out Record_Image) is
   begin
      Into := [others => 0];
      Put (Into, Version_At, Version, 2);
      Put (Into, Kind_At, Unsigned_64 (Kind_Codes (Item.Kind)), 1);
      Put (Into, Flags_At, Unsigned_64 (Item.Flags), 1);
      Put (Into, Name_Length_At, Unsigned_64 (Item.Length), 1);
      Put (Into, Block_Size_At, Unsigned_64 (Item.Block), 4);
      Put (Into, Total_Blocks_At, Item.Total_Blocks, 8);
      Put (Into, Free_Blocks_At, Item.Free_Blocks, 8);
      Put (Into, Releasing_Blocks_At, Item.Releasing_Blocks, 8);
      Put (Into, Total_Inodes_At, Item.Total_Inodes, 8);
      Put (Into, Free_Inodes_At, Item.Free_Inodes, 8);
      for I in 1 .. Item.Length loop
         Into (Name_At + I - 1) := Item.Name (I);
      end loop;
   end Encode;

   procedure Decode (From : Record_Image; Item : out Description; OK : out Boolean) is
      Code : constant Unsigned_8 := From (Kind_At);
      Stored_Length : constant Unsigned_8 := From (Name_Length_At);
      Block : constant Unsigned_64 := Get (From, Block_Size_At, 4);
      Kind : Volume_Kind := Ext2;
      Known : Boolean := False;
      Candidate : Description;
   begin
      Item := (others => <>);
      OK := False;
      for K in Volume_Kind loop
         if Kind_Codes (K) = Code then
            Kind := K;
            Known := True;
         end if;
      end loop;
      if not Known or else Get (From, Version_At, 2) /= Version
        or else Stored_Length > Maximum_Name_Bytes
        or else Block not in Smallest_Block .. Largest_Block
        or else (for some I in Name_Length_At + 1 .. Block_Size_At - 1 => From (I) /= 0)
        or else (for some I in Block_Size_At + 4 .. Total_Blocks_At - 1 => From (I) /= 0)
        or else (for some I in Name_At + Natural (Stored_Length) .. Record_Bytes - 1 => From (I) /= 0)
      then
         return;
      end if;
      Candidate :=
        (Kind => Kind, Flags => From (Flags_At), Block => Unsigned_32 (Block),
         Total_Blocks => Get (From, Total_Blocks_At, 8),
         Free_Blocks => Get (From, Free_Blocks_At, 8),
         Releasing_Blocks => Get (From, Releasing_Blocks_At, 8),
         Total_Inodes => Get (From, Total_Inodes_At, 8),
         Free_Inodes => Get (From, Free_Inodes_At, 8),
         Name => [others => 0], Length => Natural (Stored_Length));
      for I in 1 .. Candidate.Length loop
         Candidate.Name (I) := From (Name_At + I - 1);
      end loop;
      if Valid (Candidate) then
         Item := Candidate;
         OK := True;
      end if;
   end Decode;

end CuBit.Volume_Descriptions;
