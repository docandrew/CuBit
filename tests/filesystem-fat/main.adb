------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  Hosted tests of the pure FAT32 core (Fat_Format, Fat_Names, Fat_Chains)
--  against oracles written here from FATGEN 1.03 and Ada.Calendar:
--  boot sectors (valid and each refusal), FAT entries, directory entries,
--  long-name parts and their gathering, names (conversion, refusal,
--  folding, aliases and tails), dates, chain walks over corrupt FATs and
--  the free search. Prints FAT-CORE-CHECK: PASS and the number of checks.
------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Interfaces; use Interfaces;
with Fat_Format; use Fat_Format;
with Fat_Names; use Fat_Names;
with Fat_Chains; use Fat_Chains;
with Fat_Chain_Fixture;
with Fat_Chain_Instances;

procedure Main is
   package Random_Words is new Ada.Numerics.Discrete_Random (Unsigned_32);
   Gen : Random_Words.Generator;
   Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Put_Line ("FAT-CORE-CHECK: FAIL " & What);
         raise Program_Error;
      end if;
   end Check;

   function Word return Unsigned_32 is (Random_Words.Random (Gen));

   ---------------------------------------------------------------------------
   procedure Fields is
      Data : Byte_Array (0 .. 63) := [others => 0];
      Before : Byte_Array (0 .. 63);
      V : Unsigned_32;
      P : Natural;
   begin
      for Round in 1 .. 20_000 loop
         V := Word;
         P := Natural (Word mod 61);
         for I in Data'Range loop
            Data (I) := Unsigned_8 (Word and 16#FF#);
         end loop;
         Before := Data;
         Put32 (Data, P, V);
         Check (Le32 (Data, P) = V, "Put32/Le32");
         Check (Data (P) = Unsigned_8 (V and 16#FF#) and then Data (P + 3) = Unsigned_8 (Shift_Right (V, 24)),
                "Put32 is little-endian");
         for I in Data'Range loop
            if I not in P .. P + 3 then
               Check (Data (I) = Before (I), "Put32 frame");
            end if;
         end loop;
         Put16 (Data, P, Unsigned_16 (V and 16#FFFF#));
         Check (Le16 (Data, P) = Unsigned_16 (V and 16#FFFF#), "Put16/Le16");
      end loop;
   end Fields;

   ---------------------------------------------------------------------------
   --  A FAT32 boot sector as mkfs.fat would write it, with these fields.
   function Boot
     (Sector : Unsigned_16 := 512; Per_Cluster : Unsigned_8 := 1; Reserved : Unsigned_16 := 32;
      Fats : Unsigned_8 := 2; Total : Unsigned_32 := 200_000; Fat_Size : Unsigned_32 := 1_600;
      Root : Unsigned_32 := 2; Info : Unsigned_16 := 1; Backup : Unsigned_16 := 6;
      Flags : Unsigned_16 := 0; Media : Unsigned_8 := 16#F8#) return Boot_Sector
   is
      B : Boot_Sector := [others => 0];
   begin
      B (0) := 16#EB#; B (1) := 16#58#; B (2) := 16#90#;
      Put16 (B, Bytes_Per_Sector_Offset, Sector);
      B (Sectors_Per_Cluster_Offset) := Per_Cluster;
      Put16 (B, Reserved_Sectors_Offset, Reserved);
      B (Fat_Count_Offset) := Fats;
      B (Media_Offset) := Media;
      Put32 (B, Total_Sectors_32_Offset, Total);
      Put32 (B, Fat_Sectors_32_Offset, Fat_Size);
      Put16 (B, Extended_Flags_Offset, Flags);
      Put32 (B, Root_Cluster_Offset, Root);
      Put16 (B, Info_Sector_Offset, Info);
      Put16 (B, Backup_Boot_Offset, Backup);
      B (510) := 16#55#; B (511) := 16#AA#;
      return B;
   end Boot;

   procedure Expect (B : Boot_Sector; Want : Boot_Result; What : String;
                     Device_Block : Device_Block_Bytes := 512; Device : Byte_Count := 2 ** 40) is
      G : Geometry;
      Got : Boot_Result;
   begin
      Decode_Boot (B, Device_Block, Device, G, Got);
      Check (Got = Want, What & ": " & Got'Image);
      if Got = Boot_Valid then
         Check (Valid (G), What & " valid geometry");
      end if;
   end Expect;

   procedure Boots is
      G : Geometry;
      R : Boot_Result;
      B : Boot_Sector := Boot;
   begin
      Decode_Boot (B, 512, 2 ** 40, G, R);
      Check (R = Boot_Valid, "mkfs-like boot sector");
      --  200000 - 32 - 3200 = 196768 data sectors, one per cluster.
      Check (G.Clusters = 196_768 and then G.Data_Start = 3_232 and then G.Root = 2
             and then G.Info_Sector = 1 and then G.Backup_Sector = 6 and then G.Mirrored,
             "geometry fields");
      Check (Cluster_Sector (G, 2) = 3_232 and then Cluster_Sector (G, 3) = 3_233, "cluster sectors");
      Check (Fat_Sector (G, 0, 0) = 32 and then Fat_Sector (G, 0, 128) = 33
             and then Fat_Sector (G, 1, 0) = 1_632, "FAT sectors");
      Check (Fat_Offset (G, 129) = 4, "FAT offset");
      B (510) := 0;
      Expect (B, No_Boot_Signature, "signature");
      B := Boot; B (0) := 16#00#;
      Expect (B, Bad_Jump, "jump");
      Expect (Boot (Sector => 600), Bad_Sector_Size, "sector size");
      Expect (Boot, Sector_Below_Device, "4K device", Device_Block => 4096);
      Expect (Boot (Per_Cluster => 3), Bad_Cluster_Size, "cluster size");
      Expect (Boot (Sector => 4096, Per_Cluster => 32), Cluster_Too_Large, "128K cluster");
      Expect (Boot (Reserved => 0), No_Reserved_Sectors, "reserved");
      Expect (Boot (Fats => 3), Bad_Fat_Count, "FAT count");
      Expect (Boot (Media => 16#12#), Bad_Media, "media");
      B := Boot; Put16 (B, Root_Entries_Offset, 512);
      Expect (B, Not_Fat32_Layout, "root entries");
      B := Boot; Put16 (B, Version_Offset, 1);
      Expect (B, Bad_Version, "version");
      Expect (Boot (Fat_Size => 0), No_Fat_Sectors, "FAT size 0");
      Expect (Boot (Total => 3_232), No_Data_Region, "no data");
      Expect (Boot (Total => 40_000, Fat_Size => 300), Fat12_Or_16, "FAT16-sized");
      Expect (Boot (Fat_Size => 1_000), Fat_Too_Small, "small FAT");
      Expect (Boot (Root => 1), Bad_Root_Cluster, "root 1");
      Expect (Boot (Root => 196_770), Bad_Root_Cluster, "root past the end");
      Expect (Boot (Root => 16#1000_0002#), Bad_Root_Cluster, "root top bits");
      Expect (Boot (Info => 32), Bad_Info_Sector, "FSInfo in the FAT");
      Expect (Boot (Backup => 40), Bad_Backup_Sector, "backup in the FAT");
      Expect (Boot (Info => 16#FFFF#, Backup => 16#FFFF#), Boot_Valid, "no FSInfo, no backup");
      Expect (Boot (Flags => 16#0081#), Boot_Valid, "active FAT 1");
      Expect (Boot (Fats => 1, Fat_Size => 1_600, Flags => 16#0081#), Bad_Active_Fat, "active FAT 1 of 1");
      Expect (Boot, Beyond_Device, "small device", Device => 1_000_000);
      --  Random boot sectors: never an exception, and accepted ones valid.
      for Round in 1 .. 200_000 loop
         declare
            R2 : Boot_Sector := Boot
              (Sector => (case Word mod 5 is when 0 => 512, when 1 => 1024, when 2 => 2048,
                          when 3 => 4096, when others => Unsigned_16 (Word and 16#FFFF#)),
               Per_Cluster => Unsigned_8 (2 ** Natural (Word mod 9) mod 256),
               Reserved => Unsigned_16 (Word mod 70),
               Fats => Unsigned_8 (Word mod 4),
               Total => (if Word mod 2 = 0 then Word else Word mod 4_000_000),
               Fat_Size => (if Word mod 2 = 0 then Word else Word mod 40_000),
               Root => (if Word mod 2 = 0 then Word else Word mod 300_000),
               Info => Unsigned_16 (Word mod 40), Backup => Unsigned_16 (Word mod 40),
               Flags => Unsigned_16 (Word and 16#FF#));
            Gx : Geometry;
            Rx : Boot_Result;
         begin
            if Word mod 8 = 0 then
               R2 (Natural (Word mod 512)) := Unsigned_8 (Word and 16#FF#);
            end if;
            Decode_Boot (R2, 512, Byte_Count (Word) * 512, Gx, Rx);
            if Rx = Boot_Valid then
               Check (Valid (Gx), "random accepted geometry is valid");
               Check (Byte_Count (Cluster_Sector (Gx, Last_Cluster (Gx))) + Byte_Count (Cluster_Sectors (Gx))
                        <= Byte_Count (Gx.Total_Sectors), "last cluster inside");
            end if;
         end;
      end loop;
   end Boots;

   ---------------------------------------------------------------------------
   procedure Entries is
      Raw, Out_Raw : Unsigned_32;
      Last : constant Data_Cluster := 100_001;
      D : Decoded_Entry;
   begin
      Check (Decode_Entry (0, Last).Kind = Free_Entry, "free");
      Check (Decode_Entry (16#F000_0000#, Last).Kind = Free_Entry, "free with top bits");
      Check (Decode_Entry (1, Last).Kind = Invalid_Entry, "1 invalid");
      Check (Decode_Entry (Unsigned_32 (Last), Last).Kind = Next_Entry, "last cluster");
      Check (Decode_Entry (Unsigned_32 (Last) + 1, Last).Kind = Invalid_Entry, "past the end");
      Check (Decode_Entry (16#0FFF_FFF7#, Last).Kind = Bad_Entry, "bad");
      Check (Decode_Entry (16#FFFF_FFF7#, Last).Kind = Bad_Entry, "bad with top bits");
      Check (Decode_Entry (16#0FFF_FFF8#, Last).Kind = End_Entry, "end 8");
      Check (Decode_Entry (16#0FFF_FFFF#, Last).Kind = End_Entry, "end F");
      Check (Decode_Entry (16#0FFF_FFF0#, Last).Kind = Invalid_Entry, "reserved F0");
      for Round in 1 .. 100_000 loop
         Raw := Word;
         Out_Raw := Encode_Entry (Raw, Word and Entry_Mask);
         Check ((Out_Raw and Reserved_Bits) = (Raw and Reserved_Bits), "top bits kept");
         D := Decode_Entry (Raw, Last);
         if D.Kind = Next_Entry then
            Check (Unsigned_32 (D.Next) = (Raw and Entry_Mask) and then D.Next <= Last, "next");
         end if;
      end loop;
      Check (Volume_Clean (16#0FFF_FFFF#) and then not Volume_Clean (Marked_Dirty (16#0FFF_FFFF#))
             and then Volume_Clean (Marked_Clean (Marked_Dirty (16#0FFF_FFFF#))), "clean bit");
      Check (not Volume_Clean (16#0BFF_FFFF#), "hard error bit");
   end Entries;

   ---------------------------------------------------------------------------
   --  FATGEN's checksum, as its C reads.
   function Oracle_Sum (Name : Short_Name) return Unsigned_8 is
      Sum : Unsigned_8 := 0;
   begin
      for I in Name'Range loop
         Sum := (if (Sum and 1) /= 0 then 16#80# else 0) + Shift_Right (Sum, 1) + Name (I);
      end loop;
      return Sum;
   end Oracle_Sum;

   function To_Short (S : String) return Short_Name is
      N : Short_Name := [others => Space];
   begin
      for I in S'Range loop
         N (I - S'First) := Character'Pos (S (I));
      end loop;
      return N;
   end To_Short;

   procedure Directory_Entries is
      Data : Byte_Array (0 .. 95) := [others => 0];
      Item, Back : Short_Entry;
      Valid_Cluster : Boolean;
      Part, Part_Back : Long_Part;
      Good : Boolean;
      S : Long_Name_State := Idle;
      Units : Long_Units;
      Length : Long_Length;
   begin
      for Round in 1 .. 20_000 loop
         declare
            N : Short_Name;
         begin
            for I in N'Range loop
               N (I) := Unsigned_8 (Word and 16#FF#);
            end loop;
            Check (Checksum (N) = Oracle_Sum (N), "checksum");
         end;
      end loop;
      Item := (Name => To_Short ("README  TXT"), Attributes => Attribute_Archive, Case_Flags => Lower_Base,
               Created_Tenth => 7, Created_Time => 16#1234#, Created_Date => 16#5678#,
               Accessed_Date => 16#5679#, Written_Time => 16#4321#, Written_Date => 16#8765#,
               First => 16#0ABC_DEF1#, Size => 16#DEAD_BEEF#);
      Encode_Short (Data, 32, Item);
      Check (Data (0 .. 31) = [0 .. 31 => 0] and then Data (64 .. 95) = [64 .. 95 => 0], "short frame");
      Decode_Short (Data, 32, Back, Valid_Cluster);
      Check (Valid_Cluster and then Back = Item, "short round trip");
      Check (Classify (Data, 32) = File_Entry, "file entry");
      Check (Classify (Data, 0) = End_Of_Directory, "end entry");
      Data (53) := 16#F0#;   --  the cluster high word's top bits
      Decode_Short (Data, 32, Back, Valid_Cluster);
      Check (not Valid_Cluster, "cluster top bits refused");
      Data (32 + Attributes_Offset) := Attribute_Directory;
      Check (Classify (Data, 32) = Subdirectory_Entry, "directory entry");
      Data (32 + Attributes_Offset) := Attribute_Directory or Attribute_Volume_Id;
      Check (Classify (Data, 32) = Malformed_Entry, "directory and label");
      Data (32 + Attributes_Offset) := Attribute_Volume_Id;
      Check (Classify (Data, 32) = Volume_Label, "label");
      Data (32) := Free_Marker;
      Check (Classify (Data, 32) = Deleted, "deleted");
      Data (32 .. 42) := To_Short (".          ");
      Data (32 + Attributes_Offset) := Attribute_Directory;
      Check (Classify (Data, 32) = Dot_Entry, "dot");
      Data (33) := Dot;
      Check (Classify (Data, 32) = Dot_Dot_Entry, "dot dot");
      Data (32 + Attributes_Offset) := 0;
      Check (Classify (Data, 32) = Malformed_Entry, "dot file");
      Data (34) := Character'Pos ('X');
      Data (32 + Attributes_Offset) := Attribute_Directory;
      Check (Classify (Data, 32) = Malformed_Entry, "..X");
      --  Long parts.
      Part := (Ordinal => 2, Last => True, Checksum => 16#5A#,
               Units => [1 => 16#41#, 2 => 16#3B1#, 3 => 0, others => Name_Padding]);
      Data := [others => 16#77#];
      Encode_Long (Data, 32, Part);
      Check (Classify (Data, 32) = Long_Part_Entry, "long part kind");
      Decode_Long (Data, 32, Part_Back, Good);
      Check (Good and then Part_Back = Part, "long round trip");
      Check (Data (0 .. 31) = [0 .. 31 => 16#77#], "long frame");
      Data (32 + Long_Type_Offset) := 1;
      Decode_Long (Data, 32, Part_Back, Good);
      Check (not Good, "long type");
      Data (32 + Long_Type_Offset) := 0;
      Data (32) := 21;
      Decode_Long (Data, 32, Part_Back, Good);
      Check (not Good, "ordinal 21");
      Data (32) := 16#80# or 1;
      Decode_Long (Data, 32, Part_Back, Good);
      Check (not Good, "ordinal flag bits");
      --  Gathering "ABCDEFGHIJKLMNOP" (16 units: two parts) for one name.
      declare
         Name : constant Short_Name := To_Short ("ABCDEF~1   ");
         Sum : constant Unsigned_8 := Checksum (Name);
         P1, P2 : Long_Part;
      begin
         P2 := (Ordinal => 2, Last => True, Checksum => Sum, Units => [others => Name_Padding]);
         P1 := (Ordinal => 1, Last => False, Checksum => Sum, Units => [others => Name_Padding]);
         for I in 1 .. 13 loop
            P1.Units (I) := Unsigned_16 (16#40# + I);
         end loop;
         P2.Units (1) := 16#4E#; P2.Units (2) := 16#4F#; P2.Units (3) := 16#50#; P2.Units (4) := 0;
         S := Idle;
         Gather (S, P2);
         Gather (S, P1);
         Check (Complete_For (S, Name), "gathered");
         Gathered_Name (S, Units, Length, Good);
         Check (Good and then Length = 16 and then Units (1) = 16#41# and then Units (16) = 16#50#,
                "gathered name");
         Check (not Complete_For (S, To_Short ("ABCDEF~2   ")), "other short name");
         S := Idle;
         Gather (S, P1);
         Check (not S.Active, "first part without the last");
         S := Idle;
         Gather (S, P2);
         Gather (S, P2);
         Check (S.Active and then S.Expected = 1, "a repeated last part restarts");
         P1.Checksum := Sum + 1;
         Gather (S, P1);
         Check (not S.Active, "checksum mismatch");
         S := Idle;
         Gather (S, P2);
         Check (not Complete_For (S, Name), "missing part");
         --  A full 20-part name has 260 units: longer than 255 when none ends it.
         S := Idle;
         for K in reverse 1 .. 20 loop
            Gather (S, (Ordinal => K, Last => K = 20, Checksum => Sum, Units => [others => 16#41#]));
         end loop;
         Check (Complete_For (S, Name), "20 parts");
         Gathered_Name (S, Units, Length, Good);
         Check (not Good, "260 units refused");
      end;
   end Directory_Entries;

   ---------------------------------------------------------------------------
   function Refusal (S : String) return Name_Check is
      Units : Long_Units;
      Length : Long_Length;
      Result : Name_Check;
   begin
      To_Units (S, True, Units, Length, Result);
      return Result;
   end Refusal;

   function Round_Trip (S : String) return Boolean is
      Units : Long_Units;
      Length : Long_Length;
      Result : Name_Check;
      Text : Name_Text;
      Text_Length : Fat_Names.Name_Length;
      OK : Boolean;
   begin
      To_Units (S, False, Units, Length, Result);
      if Result /= Name_Valid then
         return False;
      end if;
      From_Units (Units, Length, Text, Text_Length, OK);
      return OK and then Text (1 .. Text_Length) = S;
   end Round_Trip;

   function U (S : String) return Long_Units is
      Units : Long_Units;
      Length : Long_Length;
      Result : Name_Check;
   begin
      To_Units (S, False, Units, Length, Result);
      return Units;
   end U;

   function L (S : String) return Long_Length is
      Units : Long_Units;
      Length : Long_Length;
      Result : Name_Check;
   begin
      To_Units (S, False, Units, Length, Result);
      return Length;
   end L;

   function Utf8 (Code : Natural) return String is
     (if Code < 16#80# then [Character'Val (Code)]
      elsif Code < 16#800# then [Character'Val (16#C0# + Code / 64), Character'Val (16#80# + Code mod 64)]
      elsif Code < 16#1_0000# then
        [Character'Val (16#E0# + Code / 4096), Character'Val (16#80# + Code / 64 mod 64),
         Character'Val (16#80# + Code mod 64)]
      else
        [Character'Val (16#F0# + Code / 262_144), Character'Val (16#80# + Code / 4096 mod 64),
         Character'Val (16#80# + Code / 64 mod 64), Character'Val (16#80# + Code mod 64)]);

   function Short_Text (N : Short_Name) return String is
      R : String (1 .. 11);
   begin
      for I in N'Range loop
         R (I + 1) := Character'Val (N (I));
      end loop;
      return R;
   end Short_Text;

   procedure Names is
      Units : Long_Units;
      Length : Long_Length;
      OK : Boolean;
      Short : Short_Name;
      Exact : Boolean;
      B : Basis;
      Text : Name_Text;
      Text_Length : Fat_Names.Name_Length;
   begin
      Check (Refusal ("readme.txt") = Name_Valid, "plain name");
      Check (Refusal ("") = Name_Empty, "empty");
      Check (Refusal (".") = Name_Dots and then Refusal ("..") = Name_Dots, "dots");
      Check (Refusal (".hidden") = Name_Valid, "leading dot");
      Check (Refusal ("a:b") = Name_Reserved_Character, "colon");
      Check (Refusal ("a\b") = Name_Reserved_Character, "backslash");
      Check (Refusal ("a*b") = Name_Reserved_Character and then Refusal ("a?") = Name_Reserved_Character
             and then Refusal ("a""b") = Name_Reserved_Character and then Refusal ("a|b") = Name_Reserved_Character
             and then Refusal ("a<b") = Name_Reserved_Character and then Refusal ("a>b") = Name_Reserved_Character
             and then Refusal ("a/b") = Name_Reserved_Character, "reserved set");
      Check (Refusal ("a" & ASCII.HT & "b") = Name_Control, "tab");
      Check (Refusal ("name.") = Name_Trailing and then Refusal ("name ") = Name_Trailing, "trailing");
      Check (Refusal (" name") = Name_Valid, "leading space");
      Check (Refusal ("CON") = Name_Device and then Refusal ("con.txt") = Name_Device
             and then Refusal ("Lpt9") = Name_Device and then Refusal ("COM1.tar.gz") = Name_Device
             and then Refusal ("nul") = Name_Device and then Refusal ("aux") = Name_Device, "devices");
      Check (Refusal ("CONSOLE") = Name_Valid and then Refusal ("COM0") = Name_Valid
             and then Refusal ("LPT10") = Name_Valid, "not devices");
      Check (Refusal ([Character'Val (16#C0#), Character'Val (16#80#)]) = Name_Not_UTF8, "overlong");
      Check (Refusal ([Character'Val (16#ED#), Character'Val (16#A0#), Character'Val (16#80#)]) = Name_Not_UTF8,
             "encoded surrogate");
      Check (Refusal ([Character'Val (16#F4#), Character'Val (16#90#), Character'Val (16#80#),
                       Character'Val (16#80#)]) = Name_Not_UTF8, "past U+10FFFF");
      Check (Refusal ("a" & Character'Val (16#E2#)) = Name_Not_UTF8, "truncated");
      Check (Round_Trip ("h" & Utf8 (16#E9#) & "llo " & Utf8 (16#65E5#) & Utf8 (16#1F600#)), "UTF-8 round trip");
      Check (L (Utf8 (16#1F600#)) = 2, "surrogate pair");
      for Round in 1 .. 20_000 loop
         declare
            S : String (1 .. 252);
            Last : Natural := 0;
         begin
            while Last < 248 loop
               declare
                  Code : Natural := Natural (Word mod 16#11_0000#);
               begin
                  if Code < 16#20# or else Code = 16#2F# or else Code in 16#D800# .. 16#DFFF# then
                     Code := 16#41#;
                  end if;
                  declare
                     E : constant String := Utf8 (Code);
                  begin
                     S (Last + 1 .. Last + E'Length) := E;
                     Last := Last + E'Length;
                  end;
               end;
               exit when Word mod 30 = 0;
            end loop;
            Check (Round_Trip (S (1 .. Last)), "random UTF-8 round trip");
         end;
      end loop;
      --  Unrepresentable long names fall back to the short name.
      Units := [others => Name_Padding];
      Units (1) := 16#D800#;
      From_Units (Units, 1, Text, Text_Length, OK);
      Check (not OK, "unpaired high surrogate");
      Units (1) := 16#DC00#;
      From_Units (Units, 1, Text, Text_Length, OK);
      Check (not OK, "unpaired low surrogate");
      Units (1) := 16#2F#;
      From_Units (Units, 1, Text, Text_Length, OK);
      Check (not OK, "slash");
      Units := [others => 16#65E5#];   --  three bytes each: 255 units need 765
      From_Units (Units, 255, Text, Text_Length, OK);
      Check (not OK, "longer than 255 bytes");
      From_Units (Units, 85, Text, Text_Length, OK);
      Check (OK and then Text_Length = 255, "exactly 255 bytes");
      --  Folding and matching.
      Check (Same_Name (U ("ReadMe.TXT"), L ("ReadMe.TXT"), U ("readme.txt"), L ("readme.txt")), "ASCII case");
      Check (Same_Name (U (Utf8 (16#E9#)), 1, U (Utf8 (16#C9#)), 1), "Latin-1 case");
      Check (Same_Name (U (Utf8 (16#3B1#)), 1, U (Utf8 (16#391#)), 1), "Greek case");
      Check (Same_Name (U (Utf8 (16#430#)), 1, U (Utf8 (16#410#)), 1), "Cyrillic case");
      Check (Same_Name (U (Utf8 (16#451#)), 1, U (Utf8 (16#401#)), 1), "Cyrillic io");
      Check (not Same_Name (U ("a"), 1, U ("b"), 1) and then not Same_Name (U ("ab"), 2, U ("a"), 1), "different");
      Check (Fold (16#F7#) = 16#F7# and then Fold (16#FF#) = 16#178#, "Latin-1 edges");
      --  Short names for display.
      Short_Units (To_Short ("README  TXT"), 0, Units, Length, OK);
      Check (OK and then Length = 10 and then Units (7) = 16#2E#, "README.TXT");
      Short_Units (To_Short ("README  TXT"), Lower_Base or Lower_Extension, Units, Length, OK);
      Check (OK and then Same_Name (Units, Length, U ("readme.txt"), 10) and then Units (1) = 16#72#,
             "lower-case flags");
      Short_Units (To_Short ("NOEXT      "), 0, Units, Length, OK);
      Check (OK and then Length = 5, "no extension");
      Short_Units ([16#05#, 16#41#, others => Space], 0, Units, Length, OK);
      Check (OK and then Units (1) = 16#3C3# and then Length = 2, "E5 lead as code page 437");
      Short_Units ([16#41#, 16#2F#, others => Space], 0, Units, Length, OK);
      Check (not OK, "slash in a short name");
      Short_Units ([others => Space], 0, Units, Length, OK);
      Check (not OK, "empty short name");
      --  Exact 8.3 names need no long name.
      Exact_Short (U ("README.TXT"), 10, Short, Exact);
      Check (Exact and then Short_Text (Short) = "README  TXT", "exact 8.3");
      Exact_Short (U ("readme.txt"), 10, Short, Exact);
      Check (not Exact, "lower case is not exact");
      Exact_Short (U ("A.B.C"), 5, Short, Exact);
      Check (not Exact, "two dots");
      Exact_Short (U ("TOOLONGNAME"), 11, Short, Exact);
      Check (not Exact, "nine-byte base");
      Exact_Short (U ("MAKEFILE"), 8, Short, Exact);
      Check (Exact and then Short_Text (Short) = "MAKEFILE   ", "no extension exact");
      --  Bases and tails (FATGEN section 7's examples).
      B := Make_Basis (U ("The quick brown.fox"), 19);
      Check (B.Lossy and then Short_Text (Alias (B, 1)) = "THEQUI~1FOX", "The quick brown.fox");
      B := Make_Basis (U ("readme.txt"), 10);
      Check (not B.Lossy and then Short_Text (Plain_Alias (B)) = "README  TXT", "lower case basis");
      B := Make_Basis (U (".bashrc"), 7);
      Check (B.Lossy and then Short_Text (Alias (B, 1)) = "BASHRC~1   ", ".bashrc");
      B := Make_Basis (U ("a+b=c.tar.gz"), 12);
      Check (B.Lossy and then Short_Text (Alias (B, 12)) = "A_B_C~12GZ ", "a+b=c.tar.gz tail 12");
      B := Make_Basis (U (Utf8 (16#65E5#) & Utf8 (16#672C#)), 2);
      Check (B.Lossy and then Short_Text (Alias (B, 1)) = "__~1       ", "non-ASCII basis");
      B := Make_Basis (U ("verylongname.html"), 17);
      Check (Short_Text (Alias (B, 123_456)) = "V~123456HTM", "six-digit tail");
      for Round in 1 .. 50_000 loop
         declare
            Base : String (1 .. Natural (Word mod 12) + 1);
            N : constant Tail_Number := Tail_Number (Word mod Maximum_Tail) + 1;
         begin
            for I in Base'Range loop
               Base (I) := Character'Val (16#41# + Natural (Word mod 26));
            end loop;
            B := Make_Basis (U (Base & ".dat"), Base'Length + 4);
            Check (Tail_Of (Alias (B, N), B) = N, "tail round trip");
            Check (Tail_Of (Alias (B, N), Make_Basis (U (Base & ".dax"), Base'Length + 4)) = 0,
                   "other extension");
         end;
      end loop;
   end Names;

   ---------------------------------------------------------------------------
   procedure Times is
      Date, Time : Unsigned_16;
      Unix_Epoch : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Time_Of (1970, 1, 1, 0.0);
   begin
      From_Unix (0, Date, Time);
      Check (To_Unix (Date, Time) = Epoch_Unix_Seconds, "clamped to 1980");
      From_Unix (Unsigned_64'Last, Date, Time);
      Check (To_Unix (Date, Time) = Last_Unix_Seconds, "clamped to 2107");
      Check (To_Unix (16#0000#, 0) = 0, "zero date");
      Check (To_Unix (Shift_Left (Unsigned_16 (1), 9) or Shift_Left (Unsigned_16 (2), 5) or 30, 0) = 0,
             "30 February");
      for Round in 1 .. 200_000 loop
         declare
            Seconds : constant Unsigned_64 :=
              Epoch_Unix_Seconds + (Unsigned_64 (Word) * 2 + Unsigned_64 (Word mod 2)) mod
                (Last_Unix_Seconds - Epoch_Unix_Seconds + 1);
            Even : constant Unsigned_64 := Seconds - Seconds mod 2;
            Days : constant Unsigned_64 := Seconds / 86_400;
            Moment : constant Ada.Calendar.Time := Unix_Epoch + Duration (Days) * 86_400.0
              + Duration (Seconds mod 86_400);
            Year : Year_Number;
            Month : Month_Number;
            Day : Day_Number;
            Hour : Ada.Calendar.Formatting.Hour_Number;
            Minute : Ada.Calendar.Formatting.Minute_Number;
            Second : Ada.Calendar.Formatting.Second_Number;
            Sub : Ada.Calendar.Formatting.Second_Duration;
         begin
            From_Unix (Seconds, Date, Time);
            Check (To_Unix (Date, Time) = Even, "Unix round trip" & Seconds'Image);
            Ada.Calendar.Formatting.Split (Moment, Year, Month, Day, Hour, Minute, Second, Sub);
            Check (Natural (Shift_Right (Date, 9)) + 1980 = Year
                   and then Natural (Shift_Right (Date, 5) and 15) = Month
                   and then Natural (Date and 31) = Day
                   and then Natural (Shift_Right (Time, 11)) = Hour
                   and then Natural (Shift_Right (Time, 5) and 63) = Minute
                   and then Natural (Time and 31) * 2 = Second - Second mod 2, "calendar oracle" & Seconds'Image);
         end;
      end loop;
   end Times;

   ---------------------------------------------------------------------------
   procedure Chains is
      use Fat_Chain_Fixture;
      Last : constant Data_Cluster := 11;
      Found : Data_Cluster;
      Reached : Cluster_Count;
      Result : Step_Result;
      Length : Cluster_Count;
      Tail : Data_Cluster;
      L : Ledger;
      C : Data_Cluster;
      Free_Found, Failed : Boolean;
   begin
      Table := new Fat_Chain_Fixture.Entries'(0 .. 11 => 0);
      --  2 -> 3 -> 5 -> end; 4 free; 6 -> 7 -> 6 (a cycle); 8 -> 1; 9 -> 12 (past); 10 bad.
      Table (2) := 3; Table (3) := 5; Table (5) := 16#FFFF_FFFF#;
      Table (6) := 7; Table (7) := 6; Table (8) := 1; Table (9) := 12; Table (10) := Bad_Value;
      Fat_Chain_Instances.Walks.Measure (2, Last, Length, Tail, Result);
      Check (Result = Chain_End and then Length = 3 and then Tail = 5, "good chain");
      Fat_Chain_Instances.Walks.Seek (2, Last, 2, Found, Reached, Result);
      Check (Result = Advanced and then Found = 5 and then Reached = 2, "seek 2");
      Fat_Chain_Instances.Walks.Seek (2, Last, 3, Found, Reached, Result);
      Check (Result = Chain_End and then Found = 5 and then Reached = 2, "seek past the end");
      Fat_Chain_Instances.Walks.Measure (6, Last, Length, Tail, Result);
      Check (Result = Too_Long and then Length <= 3, "a two-cluster cycle found at once");
      Fat_Chain_Instances.Walks.Seek (6, Last, 1_000_000, Found, Reached, Result);
      Check (Result = Too_Long and then Reached <= 9, "seek into a cycle");
      --  A longer tail before a cycle: 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 9 -> 6.
      Table (2) := 3; Table (3) := 4; Table (4) := 5; Table (5) := 6; Table (6) := 7;
      Table (7) := 8; Table (8) := 9; Table (9) := 6;
      Fat_Chain_Instances.Walks.Measure (2, Last, Length, Tail, Result);
      Check (Result = Too_Long and then Length <= 10, "a cycle after a tail");
      Table.all := [others => 0];
      Table (2) := 3; Table (3) := 5; Table (5) := 16#FFFF_FFFF#;
      Table (6) := 7; Table (7) := 6; Table (8) := 1; Table (9) := 12; Table (10) := Bad_Value;
      Fat_Chain_Instances.Walks.Measure (8, Last, Length, Tail, Result);
      Check (Result = Invalid_In_Chain and then Tail = 8, "link to 1");
      Fat_Chain_Instances.Walks.Measure (9, Last, Length, Tail, Result);
      Check (Result = Invalid_In_Chain, "link past the end");
      Fat_Chain_Instances.Walks.Measure (10, Last, Length, Tail, Result);
      Check (Result = Bad_In_Chain, "bad mark");
      Fat_Chain_Instances.Walks.Measure (4, Last, Length, Tail, Result);
      Check (Result = Free_In_Chain, "free in a chain");
      Fail_At := 3;
      Fat_Chain_Instances.Walks.Measure (2, Last, Length, Tail, Result);
      Check (Result = Read_Failed and then Tail = 3 and then Length = 2, "read failure");
      Fail_At := Cluster_Number'Last;
      --  Free search: from the cursor, wrapping.
      L := Opening (10, 2, (Signed => True, Next_Known => True, Next => 9, others => <>));
      Fat_Chain_Instances.Searches.Find_Free (L, C, Free_Found, Failed);
      Check (Free_Found and then C = 11, "free after the cursor");
      Table (11) := 16#FFFF_FFFF#;
      Fat_Chain_Instances.Searches.Find_Free (L, C, Free_Found, Failed);
      Check (Free_Found and then C = 4, "free search wraps");
      Table (4) := 16#F000_0000#;   --  free: only the top bits are set
      Fat_Chain_Instances.Searches.Find_Free (L, C, Free_Found, Failed);
      Check (Free_Found and then C = 4, "reserved bits ignored");
      Table (4) := 16#0FFF_FFFF#;
      Fat_Chain_Instances.Searches.Find_Free (L, C, Free_Found, Failed);
      Check (not Free_Found and then not Failed, "full volume");
      Take (L, 4);
      Check (L.Free = 1 and then L.Cursor = 5, "take");
      Give (L);
      Check (L.Free = 2, "give");
      declare
         Sector : Byte_Array (0 .. 511) := [others => 16#FF#];
      begin
         Put32 (Sector, 8, 16#F000_0000#);
         Put32 (Sector, 12, 0);
         Check (Free_Entries (Sector, 0, 128) = 2 and then Free_Entries (Sector, 3, 1) = 1, "free entries");
      end;
   end Chains;

begin
   Random_Words.Reset (Gen, 20261123);
   Fields;
   Boots;
   Entries;
   Directory_Entries;
   Names;
   Times;
   Chains;
   Put_Line ("FAT-CORE-CHECK: PASS" & Checks'Image & " checks");
end Main;
