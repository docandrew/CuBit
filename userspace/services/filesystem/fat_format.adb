------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Codecs of the FAT32 on-disk format (see the spec).
------------------------------------------------------------------------------
package body Fat_Format with SPARK_Mode is

   procedure Put16 (Data : in out Byte_Array; Position : Natural; Value : Unsigned_16) is
   begin
      Data (Position) := Unsigned_8 (Value and 16#FF#);
      Data (Position + 1) := Unsigned_8 (Shift_Right (Value, 8));
   end Put16;

   procedure Put32 (Data : in out Byte_Array; Position : Natural; Value : Unsigned_32) is
   begin
      Data (Position) := Unsigned_8 (Value and 16#FF#);
      Data (Position + 1) := Unsigned_8 (Shift_Right (Value, 8) and 16#FF#);
      Data (Position + 2) := Unsigned_8 (Shift_Right (Value, 16) and 16#FF#);
      Data (Position + 3) := Unsigned_8 (Shift_Right (Value, 24));
   end Put32;

   ---------------------------------------------------------------------------
   --  Geometry
   ---------------------------------------------------------------------------

   --  A <= B implies A * K <= B * K (ghost).
   procedure Lemma_Product_Order (A, B, K : Byte_Count)
     with Ghost, Global => null,
          Pre  => A <= B and then B <= 2 ** 32 and then K <= 2 ** 16,
          Post => A * K <= B * K;
   procedure Lemma_Product_Order (A, B, K : Byte_Count) is null;

   --  N < F * P implies N / P < F (ghost).
   procedure Lemma_Quotient_Below (N, F, P : Byte_Count)
     with Ghost, Global => null,
          Pre  => P > 0 and then P <= 2 ** 16 and then F <= 2 ** 32 and then N < F * P,
          Post => N / P < F;
   procedure Lemma_Quotient_Below (N, F, P : Byte_Count) is
      pragma Unreferenced (F);
   begin
      pragma Assert ((N / P) * P <= N);
   end Lemma_Quotient_Below;

   function Cluster_Sector (G : Geometry; C : Cluster_Number) return Sector_Number is
      Per_Cluster : constant Byte_Count := Byte_Count (Cluster_Sectors (G));
      Index : constant Byte_Count := Byte_Count (C) - Byte_Count (First_Data_Cluster);
   begin
      Lemma_Product_Order (Index + 1, Byte_Count (G.Clusters), Per_Cluster);
      return Sector_Number (Byte_Count (G.Data_Start) + Index * Per_Cluster);
   end Cluster_Sector;

   function Fat_Sector (G : Geometry; Copy : Fat_Index; C : Cluster_Number) return Sector_Number is
      Per_Sector : constant Byte_Count := Byte_Count (Entries_Per_Sector (G));
      Within : constant Byte_Count := Byte_Count (C) / Per_Sector;
   begin
      Lemma_Quotient_Below (Byte_Count (C), Byte_Count (G.Fat_Sectors), Per_Sector);
      return Sector_Number
        (Byte_Count (G.Reserved_Sectors) + Byte_Count (Copy) * Byte_Count (G.Fat_Sectors) + Within);
   end Fat_Sector;

   function Fat_Offset (G : Geometry; C : Cluster_Number) return Natural is
     (Natural (C) mod Entries_Per_Sector (G) * Fat_Entry_Bytes);

   procedure Decode_Boot
     (Data         : Boot_Sector;
      Device_Block : Device_Block_Bytes;
      Device_Bytes : Byte_Count;
      G            : out Geometry;
      Result       : out Boot_Result)
   is
      Sector_Size : constant Unsigned_16 := Le16 (Data, Bytes_Per_Sector_Offset);
      Per_Cluster : constant Unsigned_8 := Data (Sectors_Per_Cluster_Offset);
      Reserved : constant Unsigned_16 := Le16 (Data, Reserved_Sectors_Offset);
      Fat_Number : constant Unsigned_8 := Data (Fat_Count_Offset);
      Media : constant Unsigned_8 := Data (Media_Offset);
      Total : constant Unsigned_32 := Le32 (Data, Total_Sectors_32_Offset);
      Fat_Size : constant Unsigned_32 := Le32 (Data, Fat_Sectors_32_Offset);
      Flags : constant Unsigned_16 := Le16 (Data, Extended_Flags_Offset);
      Root_Raw : constant Unsigned_32 := Le32 (Data, Root_Cluster_Offset);
      Info : constant Unsigned_16 := Le16 (Data, Info_Sector_Offset);
      Backup : constant Unsigned_16 := Le16 (Data, Backup_Boot_Offset);
      Reserved_At : constant Sector_Number := Sector_Number (Reserved);
      Info_At : constant Sector_Number := (if Info = No_Sector_16 then 0 else Sector_Number (Info));
      Backup_At : constant Sector_Number := (if Backup = No_Sector_16 then 0 else Sector_Number (Backup));
      Sector_Bits : Sector_Shift;
      Cluster_Bits : Cluster_Sector_Shift;
      Fats : Fat_Copies;
      Data_Start, Data_Sectors, Count : Byte_Count;
   begin
      G := (others => <>);
      Result := Boot_Valid;
      if Data (Signature_Offset) /= Signature_Low or else Data (Signature_Offset + 1) /= Signature_High then
         Result := No_Boot_Signature;
         return;
      elsif not ((Data (Jump_Offset) = Jump_Short and then Data (Jump_Offset + 2) = Jump_Short_Nop)
                 or else Data (Jump_Offset) = Jump_Near)
      then
         Result := Bad_Jump;
         return;
      end if;
      case Sector_Size is
         when 512 => Sector_Bits := 9;
         when 1024 => Sector_Bits := 10;
         when 2048 => Sector_Bits := 11;
         when 4096 => Sector_Bits := 12;
         when others =>
            Result := Bad_Sector_Size;
            return;
      end case;
      if Natural (Sector_Size) < Device_Block or else Natural (Sector_Size) mod Device_Block /= 0 then
         Result := Sector_Below_Device;
         return;
      end if;
      case Per_Cluster is
         when 1 => Cluster_Bits := 0;
         when 2 => Cluster_Bits := 1;
         when 4 => Cluster_Bits := 2;
         when 8 => Cluster_Bits := 3;
         when 16 => Cluster_Bits := 4;
         when 32 => Cluster_Bits := 5;
         when 64 => Cluster_Bits := 6;
         when 128 => Cluster_Bits := 7;
         when others =>
            Result := Bad_Cluster_Size;
            return;
      end case;
      if Natural (Sector_Size) * Natural (Per_Cluster) > Maximum_Cluster_Bytes then
         Result := Cluster_Too_Large;
         return;
      elsif Reserved = 0 then
         Result := No_Reserved_Sectors;
         return;
      elsif Fat_Number not in 1 .. 2 then
         Result := Bad_Fat_Count;
         return;
      elsif Media /= Media_Removable and then Media < Media_Lowest_Fixed then
         Result := Bad_Media;
         return;
      elsif Le16 (Data, Root_Entries_Offset) /= 0 or else Le16 (Data, Total_Sectors_16_Offset) /= 0
        or else Le16 (Data, Fat_Sectors_16_Offset) /= 0
      then
         --  A FAT12/16 layout: a fixed root directory and 16-bit sizes.
         Result := Not_Fat32_Layout;
         return;
      elsif Le16 (Data, Version_Offset) /= 0 then
         Result := Bad_Version;
         return;
      elsif Fat_Size = 0 then
         Result := No_Fat_Sectors;
         return;
      end if;
      Fats := Fat_Copies (Fat_Number);
      Data_Start := Byte_Count (Reserved) + Byte_Count (Fats) * Byte_Count (Fat_Size);
      if Data_Start >= Byte_Count (Total) then
         Result := No_Data_Region;
         return;
      end if;
      Data_Sectors := Byte_Count (Total) - Data_Start;
      Count := Data_Sectors / Byte_Count (Per_Cluster);
      if Count < Fat32_Minimum_Clusters then
         Result := Fat12_Or_16;
         return;
      elsif Count > Maximum_Clusters then
         Result := Too_Many_Clusters;
         return;
      elsif Byte_Count (Fat_Size) * Byte_Count (Sector_Size / Fat_Entry_Bytes) < Count + 2 then
         Result := Fat_Too_Small;
         return;
      elsif Byte_Count (Root_Raw and Entry_Mask) /= Byte_Count (Root_Raw)
        or else Byte_Count (Root_Raw) < Byte_Count (First_Data_Cluster)
        or else Byte_Count (Root_Raw) > Count + 1
      then
         Result := Bad_Root_Cluster;
         return;
      elsif Info_At >= Reserved_At then
         Result := Bad_Info_Sector;
         return;
      elsif Backup_At >= Reserved_At then
         Result := Bad_Backup_Sector;
         return;
      elsif (Flags and Mirroring_Disabled) /= 0
        and then Natural (Flags and Active_Fat_Mask) >= Fats
      then
         Result := Bad_Active_Fat;
         return;
      elsif Byte_Count (Total) * Byte_Count (Sector_Size) > Device_Bytes then
         Result := Beyond_Device;
         return;
      end if;
      pragma Assert (Count * Byte_Count (Per_Cluster) <= Data_Sectors);
      G := (Sector_Bits => Sector_Bits,
            Cluster_Bits => Cluster_Bits,
            Reserved_Sectors => Reserved_At,
            Fats => Fats,
            Fat_Sectors => Sector_Number (Fat_Size),
            Total_Sectors => Sector_Number (Total),
            Data_Start => Sector_Number (Data_Start),
            Clusters => Cluster_Count (Count),
            Root => Cluster_Number (Root_Raw),
            Info_Sector => Info_At,
            Backup_Sector => Backup_At,
            Mirrored => (Flags and Mirroring_Disabled) = 0,
            Active_Fat => (if (Flags and Mirroring_Disabled) = 0 then 0
                           else Fat_Index (Flags and Active_Fat_Mask)),
            Volume_Id => Le32 (Data, Volume_Id_Offset));
   end Decode_Boot;

   ---------------------------------------------------------------------------
   --  FAT entries and FSInfo
   ---------------------------------------------------------------------------

   --  The low 28 bits of an entry, as a cluster number.
   function Low_Bits (Raw : Unsigned_32) return Cluster_Number is
     (Cluster_Number (Raw and Entry_Mask))
     with Post => Unsigned_32 (Low_Bits'Result) = (Raw and Entry_Mask);

   function Decode_Entry (Raw : Unsigned_32; Last : Data_Cluster) return Decoded_Entry is
      Value : constant Cluster_Number := Low_Bits (Raw);
   begin
      if Value = Cluster_Number (Free_Value) then
         return (Kind => Free_Entry);
      elsif Value = Cluster_Number (Bad_Value) then
         return (Kind => Bad_Entry);
      elsif Value >= Cluster_Number (First_End_Value) then
         return (Kind => End_Entry);
      elsif Value >= First_Data_Cluster and then Value <= Last then
         return (Kind => Next_Entry, Next => Value);
      else
         return (Kind => Invalid_Entry);
      end if;
   end Decode_Entry;

   function Decode_Info (Data : Boot_Sector; G : Geometry) return Info_Hints is
      Free : constant Unsigned_32 := Le32 (Data, Info_Free_Offset);
      Next : constant Unsigned_32 := Le32 (Data, Info_Next_Offset);
      Hints : Info_Hints;
   begin
      Hints.Signed := Le32 (Data, Info_Lead_Offset) = Info_Lead_Signature
        and then Le32 (Data, Info_Struct_Offset) = Info_Struct_Signature
        and then Le32 (Data, Info_Trail_Offset) = Info_Trail_Signature;
      if Hints.Signed then
         if Free <= Entry_Mask and then Low_Bits (Free) <= G.Clusters then
            Hints.Free_Known := True;
            Hints.Free := Low_Bits (Free);
         end if;
         if Next <= Entry_Mask and then Low_Bits (Next) >= First_Data_Cluster
           and then Low_Bits (Next) <= Last_Cluster (G)
         then
            Hints.Next_Known := True;
            Hints.Next := Low_Bits (Next);
         end if;
      end if;
      return Hints;
   end Decode_Info;

   procedure Encode_Info (Data : in out Boot_Sector; Free : Cluster_Count; Next : Data_Cluster) is
   begin
      Put32 (Data, Info_Lead_Offset, Info_Lead_Signature);
      Put32 (Data, Info_Struct_Offset, Info_Struct_Signature);
      Put32 (Data, Info_Free_Offset, Unsigned_32 (Free));
      Put32 (Data, Info_Next_Offset, Unsigned_32 (Next));
      Put32 (Data, Info_Trail_Offset, Info_Trail_Signature);
   end Encode_Info;

   ---------------------------------------------------------------------------
   --  Directory entries
   ---------------------------------------------------------------------------

   function Classify (Data : Byte_Array; Position : Natural) return Directory_Entry_Kind is
      Lead : constant Unsigned_8 := Data (Position);
      Attributes : constant Unsigned_8 := Data (Position + Attributes_Offset);
      Kinds : constant Unsigned_8 := Attributes and (Attribute_Directory or Attribute_Volume_Id);
      Dot_Name, Dot_Dot_Name : Boolean := Lead = Dot;
   begin
      if Lead = End_Marker then
         return End_Of_Directory;
      elsif Lead = Free_Marker then
         return Deleted;
      elsif (Attributes and Attribute_Long_Mask) = Attribute_Long_Name then
         return Long_Part_Entry;
      elsif Kinds = Attribute_Volume_Id then
         return Volume_Label;
      elsif Kinds = (Attribute_Directory or Attribute_Volume_Id) then
         return Malformed_Entry;
      end if;
      --  "." is a dot and ten spaces, ".." two dots and nine.
      for I in 1 .. Short_Name_Bytes - 1 loop
         Dot_Name := Dot_Name and then Data (Position + I) = Space;
         Dot_Dot_Name := Dot_Dot_Name and then
           Data (Position + I) = (if I = 1 then Dot else Space);
      end loop;
      if Dot_Name or else Dot_Dot_Name then
         return (if Kinds = Attribute_Directory
                 then (if Dot_Name then Dot_Entry else Dot_Dot_Entry)
                 else Malformed_Entry);
      elsif Lead = Dot then
         return Malformed_Entry;   --  no other short name starts with a dot
      end if;
      return (if Kinds = Attribute_Directory then Subdirectory_Entry else File_Entry);
   end Classify;

   procedure Decode_Short
     (Data : Byte_Array; Position : Natural; Item : out Short_Entry; Valid_Cluster : out Boolean)
   is
      High : constant Unsigned_16 := Le16 (Data, Position + Cluster_High_Offset);
      Low : constant Unsigned_16 := Le16 (Data, Position + Cluster_Low_Offset);
      Cluster_Top : constant Unsigned_16 := 16#F000#;
   begin
      Item := (others => <>);
      for I in Short_Name'Range loop
         Item.Name (I) := Data (Position + Name_Offset + I);
      end loop;
      Item.Attributes := Data (Position + Attributes_Offset);
      Item.Case_Flags := Data (Position + Case_Offset);
      Item.Created_Tenth := Data (Position + Created_Tenth_Offset);
      Item.Created_Time := Le16 (Data, Position + Created_Time_Offset);
      Item.Created_Date := Le16 (Data, Position + Created_Date_Offset);
      Item.Accessed_Date := Le16 (Data, Position + Accessed_Date_Offset);
      Item.Written_Time := Le16 (Data, Position + Written_Time_Offset);
      Item.Written_Date := Le16 (Data, Position + Written_Date_Offset);
      Item.Size := Le32 (Data, Position + Size_Offset);
      Valid_Cluster := (High and Cluster_Top) = 0;
      if Valid_Cluster then
         Item.First := Cluster_Number
           (Shift_Left (Unsigned_32 (High), 16) or Unsigned_32 (Low));
      end if;
   end Decode_Short;

   procedure Encode_Short (Data : in out Byte_Array; Position : Natural; Item : Short_Entry) is
      First : constant Unsigned_32 := Unsigned_32 (Item.First);
   begin
      for I in Short_Name'Range loop
         Data (Position + Name_Offset + I) := Item.Name (I);
         pragma Loop_Invariant
           (for all J in Data'Range =>
              (if J not in Position .. Position + (Entry_Bytes - 1) then Data (J) = Data'Loop_Entry (J)));
      end loop;
      Data (Position + Attributes_Offset) := Item.Attributes;
      Data (Position + Case_Offset) := Item.Case_Flags;
      Data (Position + Created_Tenth_Offset) := Item.Created_Tenth;
      Put16 (Data, Position + Created_Time_Offset, Item.Created_Time);
      Put16 (Data, Position + Created_Date_Offset, Item.Created_Date);
      Put16 (Data, Position + Accessed_Date_Offset, Item.Accessed_Date);
      Put16 (Data, Position + Cluster_High_Offset, Unsigned_16 (Shift_Right (First, 16)));
      Put16 (Data, Position + Written_Time_Offset, Item.Written_Time);
      Put16 (Data, Position + Written_Date_Offset, Item.Written_Date);
      Put16 (Data, Position + Cluster_Low_Offset, Unsigned_16 (First and 16#FFFF#));
      Put32 (Data, Position + Size_Offset, Item.Size);
   end Encode_Short;

   function Checksum (Name : Short_Name) return Unsigned_8 is
      Sum : Unsigned_8 := 0;
   begin
      for I in Name'Range loop
         Sum := Rotate_Right (Sum, 1) + Name (I);
      end loop;
      return Sum;
   end Checksum;

   procedure Decode_Long (Data : Byte_Array; Position : Natural; Part : out Long_Part; Valid : out Boolean) is
      Ordinal : constant Unsigned_8 := Data (Position + Long_Ordinal_Offset);
      Number : constant Unsigned_8 := Ordinal and Long_Ordinal_Mask;
   begin
      Part := (others => <>);
      Valid := Number >= 1 and then Number <= Maximum_Parts
        and then (Ordinal and not (Long_Ordinal_Mask or Long_Last_Flag)) = 0
        and then Data (Position + Long_Type_Offset) = 0
        and then Le16 (Data, Position + Long_Cluster_Offset) = 0;
      if not Valid then
         return;
      end if;
      Part.Ordinal := Part_Ordinal (Number);
      Part.Last := (Ordinal and Long_Last_Flag) /= 0;
      Part.Checksum := Data (Position + Long_Checksum_Offset);
      for I in Part_Unit_Index loop
         Part.Units (I) := Le16 (Data, Position + Unit_Offsets (I));
      end loop;
   end Decode_Long;

   procedure Encode_Long (Data : in out Byte_Array; Position : Natural; Part : Long_Part) is
   begin
      Data (Position + Long_Ordinal_Offset) :=
        Unsigned_8 (Part.Ordinal) or (if Part.Last then Long_Last_Flag else 0);
      Data (Position + Attributes_Offset) := Attribute_Long_Name;
      Data (Position + Long_Type_Offset) := 0;
      Data (Position + Long_Checksum_Offset) := Part.Checksum;
      Put16 (Data, Position + Long_Cluster_Offset, 0);
      for I in Part_Unit_Index loop
         Put16 (Data, Position + Unit_Offsets (I), Part.Units (I));
         pragma Loop_Invariant
           (for all J in Data'Range =>
              (if J not in Position .. Position + (Entry_Bytes - 1) then Data (J) = Data'Loop_Entry (J)));
      end loop;
   end Encode_Long;

   procedure Gather (S : in out Long_Name_State; Part : Long_Part) is
      Base : Natural;
   begin
      if Part.Last then
         --  The first part on disk (the name's end) starts a new set.
         S := Idle;
         S.Active := True;
         S.Parts := Part.Ordinal;
         S.Sum := Part.Checksum;
      elsif not S.Active or else Part.Ordinal /= S.Expected or else Part.Checksum /= S.Sum then
         S := Idle;
         return;
      end if;
      Base := (Part.Ordinal - 1) * Units_Per_Part;
      for I in Part_Unit_Index loop
         S.Units (Base + I) := Part.Units (I);
      end loop;
      S.Expected := Part.Ordinal - 1;
   end Gather;

   procedure Gathered_Name
     (S : Long_Name_State; Units : out Long_Units; Length : out Long_Length; Valid : out Boolean)
   is
      Available : constant Natural := S.Parts * Units_Per_Part;
      Count : Natural := 0;
   begin
      Units := [others => Name_Padding];
      Length := 0;
      Valid := False;
      for I in 1 .. Available loop
         exit when S.Units (I) = Name_Terminator;
         if Count = Maximum_Long_Units then
            return;   --  longer than 255
         end if;
         Count := Count + 1;
         Units (Count) := S.Units (I);
         pragma Loop_Invariant (Count = I and then Count <= Maximum_Long_Units);
      end loop;
      Length := Count;
      Valid := Count >= 1;
   end Gathered_Name;

   ---------------------------------------------------------------------------
   --  Dates and times
   ---------------------------------------------------------------------------
   Seconds_Per_Day : constant := 86_400;
   Day_Bits : constant Unsigned_16 := 16#001F#;
   Month_Shift : constant := 5;
   Month_Bits : constant Unsigned_16 := 16#000F#;
   Year_Shift : constant := 9;
   Minute_Shift : constant := 5;
   Minute_Bits : constant Unsigned_16 := 16#003F#;
   Hour_Shift : constant := 11;
   Half_Seconds_Bits : constant Unsigned_16 := 16#001F#;

   subtype Year_Offset is Natural range 0 .. Last_Year - Epoch_Year;
   subtype Month_Number is Natural range 1 .. 12;
   type Month_Days is array (Month_Number) of Natural;
   Days_In_Month : constant Month_Days := [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
   Days_Before_Month : constant Month_Days := [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];

   --  1980 + Offset is a leap year (2100 is not).
   function Leap (Offset : Year_Offset) return Boolean is
     (Offset mod 4 = 0 and then Offset /= 2100 - Epoch_Year);

   function Year_Days (Offset : Year_Offset) return Natural is (if Leap (Offset) then 366 else 365);

   --  Days from 1980-01-01 to the start of year 1980 + Offset.
   function Days_Before_Year (Offset : Year_Offset) return Natural is
     (Offset * 365 + (Offset + 3) / 4 - (if Offset > 2100 - Epoch_Year then 1 else 0))
     with Post => Days_Before_Year'Result <= 128 * 366;

   function To_Unix (Date, Time : Unsigned_16) return Unix_Seconds is
      Day : constant Natural := Natural (Date and Day_Bits);
      Month : constant Natural := Natural (Shift_Right (Date, Month_Shift) and Month_Bits);
      Year : constant Year_Offset := Natural (Shift_Right (Date, Year_Shift));
      Hours : constant Natural := Natural'Min (23, Natural (Shift_Right (Time, Hour_Shift)));
      Minutes : constant Natural := Natural'Min (59, Natural (Shift_Right (Time, Minute_Shift) and Minute_Bits));
      Seconds : constant Natural := Natural'Min (29, Natural (Time and Half_Seconds_Bits)) * 2;
      Length : Natural;
      Days : Natural;
   begin
      if Month not in Month_Number or else Day = 0 then
         return 0;
      end if;
      Length := Days_In_Month (Month) + (if Month = 2 and then Leap (Year) then 1 else 0);
      if Day > Length then
         return 0;
      end if;
      Days := Days_Before_Year (Year) + Days_Before_Month (Month)
        + (if Month > 2 and then Leap (Year) then 1 else 0) + Day - 1;
      return Unix_Seconds (Epoch_Unix_Seconds)
        + Unix_Seconds (Days) * Seconds_Per_Day
        + Unix_Seconds (Hours * 3600 + Minutes * 60 + Seconds);
   end To_Unix;

   procedure From_Unix (Seconds : Unsigned_64; Date, Time : out Unsigned_16) is
      Clamped : constant Unsigned_64 :=
        Unsigned_64'Max (Epoch_Unix_Seconds, Unsigned_64'Min (Seconds, Last_Unix_Seconds));
      Elapsed : constant Unsigned_64 := Clamped - Epoch_Unix_Seconds;
      Days : Natural := Natural (Elapsed / Seconds_Per_Day);
      Of_Day : constant Natural := Natural (Elapsed mod Seconds_Per_Day);
      Year : Year_Offset := 0;
      Month : Month_Number := 1;
      Length : Natural;
   begin
      pragma Assert (Days <= Days_Before_Year (Year_Offset'Last) + 365);
      loop
         pragma Loop_Invariant (Days + Days_Before_Year (Year) <= Days_Before_Year (Year_Offset'Last) + 365);
         exit when Year = Year_Offset'Last or else Days < Year_Days (Year);
         Days := Days - Year_Days (Year);
         Year := Year + 1;
      end loop;
      Days := Natural'Min (Days, Year_Days (Year) - 1);
      loop
         Length := Days_In_Month (Month) + (if Month = 2 and then Leap (Year) then 1 else 0);
         exit when Month = 12 or else Days < Length;
         Days := Days - Length;
         Month := Month + 1;
      end loop;
      Days := Natural'Min (Days, 30);
      Date := Shift_Left (Unsigned_16 (Year), Year_Shift)
        or Shift_Left (Unsigned_16 (Month), Month_Shift)
        or Unsigned_16 (Days + 1);
      Time := Shift_Left (Unsigned_16 (Of_Day / 3600), Hour_Shift)
        or Shift_Left (Unsigned_16 (Of_Day / 60 mod 60), Minute_Shift)
        or Unsigned_16 (Of_Day mod 60 / 2);
   end From_Unix;

end Fat_Format;
