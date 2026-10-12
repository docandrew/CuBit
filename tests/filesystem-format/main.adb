--  Hosted tests of the pure ext2 core (Ext2_Format, Ext2_Geometry,
--  Ext2_Bitmaps) against the arithmetic the driver used before the split
--  (copied here as the oracle): random and edge-case geometries, directory
--  records and bitmaps. Run: build/main (prints FORMAT-CORE-CHECK: PASS).
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with Ext2_Format; use Ext2_Format;
with Ext2_Geometry; use Ext2_Geometry;
with Ext2_Bitmaps;

procedure Main is
   package Random_Words is new Ada.Numerics.Discrete_Random (Unsigned_32);
   Gen : Random_Words.Generator;
   Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Put_Line ("FORMAT-CORE-CHECK: FAIL " & What);
         raise Program_Error;
      end if;
   end Check;

   function Word return Unsigned_32 is (Random_Words.Random (Gen));
   function Below (Limit : Unsigned_32) return Unsigned_32 is
     (if Limit = 0 then 0 else Word mod Limit);

   --  The superblock check the driver used before Ext2_Geometry.
   function Old_Supported (F : Superblock_Fields) return Boolean is
      Size, Inode_Bytes, Allocatable : Unsigned_32;
      Count : constant Unsigned_32 := Unsigned_32 (F.Block_Count);
      First : constant Unsigned_32 := Unsigned_32 (F.First_Data_Block);
   begin
      if F.Signature /= 16#EF53# or else F.Block_Shift > 2 or else Count = 0 or else
        Count <= First or else F.Inode_Count = 0 or else
        F.Blocks_Per_Group = 0 or else F.Inodes_Per_Group = 0
      then
         return False;
      end if;
      Size := Shift_Left (Unsigned_32'(1024), Natural (F.Block_Shift));
      Inode_Bytes := (if F.Revision >= 1 then Unsigned_32 (F.Inode_Bytes) else 128);
      Allocatable := Count - First;
      return Inode_Bytes >= 128 and then Inode_Bytes <= Size and then
        (Inode_Bytes and (Inode_Bytes - 1)) = 0 and then
        Unsigned_32 (F.Blocks_Per_Group) <= Size * 8 and then
        Unsigned_32 (F.Inodes_Per_Group) <= Size * 8 and then
        Unsigned_32 (F.Free_Blocks) <= Allocatable and then
        F.Free_Inodes <= F.Inode_Count;
   end Old_Supported;

   function Pick (Edge : Unsigned_32) return Unsigned_32 is
     (case Word mod 6 is
        when 0 => 0, when 1 => 1, when 2 => Edge, when 3 => Edge + 1,
        when 4 => Unsigned_32'Last, when others => Word);

   procedure Superblocks is
      F : Superblock_Fields;
   begin
      for Trial in 1 .. 300_000 loop
         declare
            Shift : constant Unsigned_32 := Below (4);
            Size : constant Unsigned_32 := Shift_Left (Unsigned_32'(1024), Natural (Shift));
            Count : constant Unsigned_32 := Pick (Below (2 ** 20) + 2);
         begin
            F := (Signature        => (if Word mod 16 = 0 then 16#EF52# else 16#EF53#),
                  Block_Shift      => (if Word mod 32 = 0 then Word else Shift),
                  Block_Count      => Count_32 (Count),
                  First_Data_Block => Count_32 (if Word mod 2 = 0 then 0 else Pick (1)),
                  Blocks_Per_Group => Count_32 (Pick (Size * 8)),
                  Inode_Count      => Count_32 (Pick (Below (2 ** 20))),
                  Inodes_Per_Group => Count_32 (Pick (Size * 8)),
                  Free_Blocks      => Count_32 (Pick (Count)),
                  Free_Inodes      => Count_32 (Pick (Below (2 ** 20))),
                  Revision         => Below (3),
                  Inode_Bytes      => Unsigned_16
                    (case Word mod 7 is
                       when 0 => 128, when 1 => 256, when 2 => 512, when 3 => 4096,
                       when 4 => 8192, when 5 => 384, when others => Word mod 65536));
            Check (Supported (F) = Old_Supported (F), "Supported");
            if Supported (F) then
               Check (Valid (Shape_Of (F)), "Supported implies Valid");
            end if;
         end;
      end loop;
   end Superblocks;

   function Random_Shape return Geometry is
      Shift : constant Unsigned_32 := Below (3);
      Size : constant Unsigned_32 := Shift_Left (Unsigned_32'(1024), Natural (Shift));
      First : constant Unsigned_32 := (if Shift = 0 then 1 else 0);
      Per_Group : constant Unsigned_32 := 1 + Below (Size * 8);
      Count : constant Unsigned_32 :=
        First + 1 + (if Word mod 8 = 0 then Unsigned_32'Last - First - 1 else Below (2 ** 24));
      Inode_Size : constant Unsigned_32 :=
        Shift_Left (Unsigned_32'(128), Natural (Below (Shift + 4)));
   begin
      return (Block_Count      => Count_32 (Count),
              First_Data_Block => Count_32 (First),
              Blocks_Per_Group => Count_32 (Per_Group),
              Inode_Count      => Count_32 (1 + Below (2 ** 24)),
              Inodes_Per_Group => Count_32 (1 + Below (Size * 8)),
              Block_Bytes      => Count_32 (Size),
              Inode_Bytes      => Count_32 (Unsigned_32'Min (Inode_Size, Size)));
   end Random_Shape;

   procedure Locations is
   begin
      for Trial in 1 .. 300_000 loop
         declare
            S : constant Geometry := Random_Shape;
            BC : constant Unsigned_32 := Unsigned_32 (S.Block_Count);
            FDB : constant Unsigned_32 := Unsigned_32 (S.First_Data_Block);
            BPG : constant Unsigned_32 := Unsigned_32 (S.Blocks_Per_Group);
            IPG : constant Unsigned_32 := Unsigned_32 (S.Inodes_Per_Group);
            BB : constant Unsigned_32 := Unsigned_32 (S.Block_Bytes);
            Inode : constant Unsigned_32 := 1 + Below (Unsigned_32 (S.Inode_Count));
            Block : constant Unsigned_32 := FDB + Below (BC - FDB);
            Table : constant Unsigned_32 := Word;
            --  The driver's formulas before the split.
            Group : constant Unsigned_32 := (Inode - 1) / IPG;
            Index : constant Unsigned_32 := (Inode - 1) mod IPG;
            Old_Descriptor : constant Unsigned_64 :=
              (Unsigned_64 (FDB) + 1) * Unsigned_64 (BB) + Unsigned_64 (Group) * 32;
            Old_Inode : constant Unsigned_64 :=
              Unsigned_64 (Table) * Unsigned_64 (BB) +
              Unsigned_64 (Index) * Unsigned_64 (S.Inode_Bytes);
            Block_Group : constant Unsigned_32 := (Block - FDB) / BPG;
            Group_First : constant Unsigned_32 := FDB + Block_Group * BPG;
            Old_Groups : constant Unsigned_32 := 1 + (BC - FDB - 1) / BPG;
         begin
            Check (Valid (S), "random shape valid");
            Check (Group_Of_Inode (S, Inode_Number (Inode)) = Group_Number (Group), "inode group");
            Check (Index_In_Group (S, Inode_Number (Inode)) = Count_32 (Index), "inode index");
            Check (Descriptor_Offset (S, Group_Number (Group)) = Byte_Offset (Old_Descriptor),
                   "descriptor offset");
            Check (Inode_Offset (S, Block_Number (Table), Count_32 (Index)) =
                     Byte_Offset (Old_Inode), "inode offset");
            Check (Block_Group_Count (S) = Count_32 (Old_Groups), "group count");
            Check (Inode_Group_Count (S) =
                     Count_32 (1 + (Unsigned_32 (S.Inode_Count) - 1) / IPG), "inode groups");
            Check (Group_Of_Block (S, Block_Number (Block)) = Group_Number (Block_Group),
                   "block group");
            Check (Bit_Of_Block (S, Block_Number (Block)) = Count_32 (Block - Group_First),
                   "block bit");
            Check (Group_First_Block (S, Group_Number (Block_Group)) = Block_Number (Group_First),
                   "group first block");
            Check (Blocks_In_Group (S, Group_Number (Block_Group)) =
                     Count_32 (Unsigned_32'Min (BPG, BC - Group_First)), "blocks in group");
            Check (Block_Bitmap_Bytes (S) = Count_32 (Unsigned_32'Min ((BPG + 7) / 8, 4096)),
                   "block bitmap bytes");
            Check (Inode_Bitmap_Bytes (S) = Count_32 ((IPG + 7) / 8), "inode bitmap bytes");
            Check (Volume_Bytes (S) = Byte_Offset (Unsigned_64 (BC) * Unsigned_64 (BB)),
                   "volume bytes");
         end;
      end loop;
      for Revision in Unsigned_32 range 0 .. 2 loop
         for First in Count_32 range 0 .. 12 loop
            Check (First_Usable_Inode (Revision, First) =
                     (if Revision >= 1 and then First > 0 then Inode_Number (First) else 11),
                   "first usable inode");
         end loop;
      end loop;
   end Locations;

   procedure Records is
      Data : Byte_Array (0 .. 63);
   begin
      for Trial in 1 .. 200_000 loop
         for B of Data loop
            B := Unsigned_8 (Word mod 256);
         end loop;
         declare
            P : constant Natural := Natural (Below (57));
            H : constant Record_Header := Header (Data, P);
            Span : constant Natural := Natural (Data (P + 4)) + 256 * Natural (Data (P + 5));
            Length : constant Natural := Natural (Data (P + 6));
            Number : constant Unsigned_32 :=
              Unsigned_32 (Data (P)) or Shift_Left (Unsigned_32 (Data (P + 1)), 8) or
              Shift_Left (Unsigned_32 (Data (P + 2)), 16) or
              Shift_Left (Unsigned_32 (Data (P + 3)), 24);
            Room : constant Natural := Natural (Below (5000));
         begin
            Check (H.Inode = Inode_Number (Number) and H.Span = Span and H.Length = Length and
                     H.File_Type = Data (P + 7), "record header");
            Check (Fits (H, Room) =
                     not (Span < 8 or else Span mod 4 /= 0 or else Span > Room or else
                          Length > Span - 8), "record fits");
         end;
      end loop;
      for Length in Name_Length loop
         Check (Record_Bytes (Length) = ((8 + Length + 3) / 4) * 4, "record bytes");
      end loop;
      for Mode in Unsigned_16 loop
         Check (Inode_Type (Mode) = Unsigned_8 (Shift_Right (Mode, 12) and 16#F#), "inode type");
      end loop;
      for Kind in Mode_Type loop
         Check (Directory_File_Type (Kind) =
                  (case Kind is
                     when 16#1# => 5, when 16#2# => 3, when 16#4# => 2, when 16#6# => 4,
                     when 16#8# => 1, when 16#A# => 7, when 16#C# => 6, when others => 0),
                "file type");
      end loop;
      for Trial in 1 .. 100_000 loop
         declare
            Low : constant Unsigned_32 := Word;
            High : constant Unsigned_32 := Word;
            Size : constant Unsigned_64 := File_Size (Low, High);
         begin
            Check (Size = Unsigned_64 (High) * 16#1_0000_0000# + Unsigned_64 (Low), "file size");
            Check (Size_Low (Size) = Low and Size_High (Size) = High, "size words");
         end;
      end loop;
   end Records;

   --  The block reservation scan the driver used before Ext2_Bitmaps.
   procedure Bitmaps is
      use Ext2_Bitmaps;
      Before, Map, Reference : Bitmap (0 .. Maximum_Bytes - 1);
      Taken : Bit_List (1 .. 1024);
      Count : Take_Count;
   begin
      for Trial in 1 .. 5_000 loop
         declare
            Bytes : constant Natural := 1 + Natural (Below (Maximum_Bytes));
            Scan : constant Positive := 1 + Natural (Below (Unsigned_32 (Bytes) * 8));
            Start : constant Natural := Natural (Below (Maximum_Bits + 1));
            Limit : constant Natural := Natural (Below (1025));
            Density : constant Unsigned_32 := Below (9);
            Expected : Bit_List (1 .. 1024);
            Expected_Count : Natural := 0;
            Bit : Natural;
            Mask_Bit : Unsigned_8;
         begin
            --  Density 0 (empty) .. 8 (full); otherwise random bytes, set
            --  more often as Density grows.
            for B of Before loop
               B := (case Density is
                       when 0 => 0,
                       when 8 => Unsigned_8'Last,
                       when others =>
                         Unsigned_8 (Word mod 256) or
                         (if Density > 4 then Unsigned_8 (Word mod 256) else 0));
            end loop;
            Map := Before;
            Reference := Before;
            for Scanned in 0 .. Scan - 1 loop
               exit when Expected_Count = Limit;
               Bit := (Start + Scanned) mod Scan;
               Mask_Bit := Shift_Left (Unsigned_8'(1), Bit mod 8);
               if (Reference (Bit / 8) and Mask_Bit) = 0 then
                  Reference (Bit / 8) := Reference (Bit / 8) or Mask_Bit;
                  Expected_Count := Expected_Count + 1;
                  Expected (Expected_Count) := Bit;
               end if;
            end loop;
            Take_Free (Map (0 .. Bytes - 1), Scan, Start, Limit, Taken, Count);
            Check (Count = Expected_Count, "taken count");
            Check (Taken (1 .. Count) = Expected (1 .. Count), "taken bits");
            Check (Map = Reference, "bitmap after reservation");
            for I in 1 .. Count loop
               Clear (Map, Taken (I));
            end loop;
            Check (Map = Before, "release restores the bitmap");
         end;
      end loop;
   end Bitmaps;
begin
   Random_Words.Reset (Gen, 20261011);
   Superblocks;
   Locations;
   Records;
   Bitmaps;
   Put_Line ("FORMAT-CORE-CHECK: PASS" & Checks'Image & " checks");
end Main;
