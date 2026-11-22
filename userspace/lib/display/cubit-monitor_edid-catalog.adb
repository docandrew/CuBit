pragma Ada_2022;
package body CuBit.Monitor_EDID.Catalog with SPARK_Mode is

   --  Detailed timing descriptor layout (E-EDID 1.4 table 3.21), as offsets
   --  from the descriptor's first byte.
   Descriptor_Bytes : constant := 18;
   subtype Descriptor_Offset is Natural range 0 .. Block_Bytes - Descriptor_Bytes;
   Base_Descriptor_First : constant := 54;
   --  Display descriptors (clock bytes zero) carry their kind in byte 3.
   Monitor_Name_Tag : constant Unsigned_8 := 16#FC#;
   Range_Limits_Tag : constant Unsigned_8 := 16#FD#;
   Name_First : constant := 5;
   Text_End : constant Unsigned_8 := 16#0A#;
   Range_Max_Clock_Byte : constant := 9;
   Range_Clock_Unit_kHz : constant := 10_000;

   --  Base block fields (table 3.1).
   Vendor_High : constant := 8;
   Vendor_Low : constant := 9;
   Product_Low : constant := 10;
   Serial_First : constant := 12;
   Week_Byte : constant := 16;
   Year_Byte : constant := 17;
   Model_Year_Week : constant Unsigned_8 := 16#FF#;
   Year_Base : constant := 1990;
   Horizontal_CM_Byte : constant := 21;
   Vertical_CM_Byte : constant := 22;
   MM_Per_CM : constant := 10;
   Letter_Bits : constant := 32;

   --  CTA-861 extension (CTA-861-G section 7.5).
   CTA_Tag : constant Unsigned_8 := 16#02#;
   CTA_DTD_Offset_Byte : constant := 2;
   CTA_Data_First : constant := 4;
   Data_Block_Header_Length_Bits : constant := 32;
   Video_Data_Block : constant := 2;
   Vendor_Data_Block : constant := 3;
   --  IEEE OUIs, least significant byte first as stored.
   type OUI_Bytes is array (0 .. 2) of Unsigned_8;
   HDMI_LLC_OUI : constant OUI_Bytes := [16#03#, 16#0C#, 16#00#];
   HDMI_Forum_OUI : constant OUI_Bytes := [16#D8#, 16#5D#, 16#C4#];
   --  Payload bytes (after the header byte).
   HDMI_LLC_Max_TMDS : constant := 6;
   HDMI_Forum_Max_TMDS : constant := 4;
   HDMI_Forum_Flags : constant := 5;
   SCDC_Present : constant Unsigned_8 := 16#80#;
   TMDS_Unit_kHz : constant := 5_000;
   --  Short video descriptors (CTA-861-G 7.5.1 table 64).
   SVD_Native_First : constant := 129;
   SVD_Native_Last : constant := 192;
   SVD_Native_Offset : constant := 128;

   function Valid_Checksum (Data : Block) return Boolean is
      Sum : Unsigned_8 := 0;
   begin
      for Byte of Data loop
         Sum := Sum + Byte;
      end loop;
      return Sum = 0;
   end Valid_Checksum;

   function Display_Descriptor (Data : Block; At_Byte : Descriptor_Offset; Tag : Unsigned_8)
      return Boolean is
     (Data (At_Byte) = 0 and then Data (At_Byte + 1) = 0 and then Data (At_Byte + 2) = 0 and then
      Data (At_Byte + 3) = Tag);

   function Letter (Code : Unsigned_16) return Character is
     (if Code in 1 .. 26 then Character'Val (Character'Pos ('A') + Natural (Code) - 1) else '?');

   function Identify (Base : Block) return Identity is
      Result : Identity;
      Vendor : constant Unsigned_16 :=
        Unsigned_16 (Base (Vendor_High)) * 256 + Unsigned_16 (Base (Vendor_Low));
   begin
      Result.Vendor := [Letter (Vendor / (Letter_Bits * Letter_Bits) mod Letter_Bits),
                        Letter (Vendor / Letter_Bits mod Letter_Bits),
                        Letter (Vendor mod Letter_Bits)];
      Result.Product := Unsigned_16 (Base (Product_Low)) + Unsigned_16 (Base (Product_Low + 1)) * 256;
      Result.Serial := Unsigned_32 (Base (Serial_First)) +
        Unsigned_32 (Base (Serial_First + 1)) * 2 ** 8 +
        Unsigned_32 (Base (Serial_First + 2)) * 2 ** 16 +
        Unsigned_32 (Base (Serial_First + 3)) * 2 ** 24;
      if Base (Week_Byte) /= Model_Year_Week then
         Result.Year := Year_Base + Natural (Base (Year_Byte));
      end if;
      for D in 0 .. Base_Descriptors - 1 loop
         declare
            At_Byte : constant Descriptor_Offset := Base_Descriptor_First + D * Descriptor_Bytes;
         begin
            if Display_Descriptor (Base, At_Byte, Monitor_Name_Tag) then
               Result.Name := (Length => 0, Text => [others => ' ']);
               for I in 1 .. Name_Length loop
                  pragma Loop_Invariant (Result.Name.Length = I - 1);
                  declare
                     Byte : constant Unsigned_8 := Base (At_Byte + Name_First + I - 1);
                  begin
                     exit when Byte = Text_End;
                     Result.Name.Text (I) :=
                       (if Byte in 16#20# .. 16#7E# then Character'Val (Byte) else '?');
                     Result.Name.Length := I;
                  end;
               end loop;
               --  Trailing padding spaces are not part of the name.
               while Result.Name.Length > 0 and then
                 Result.Name.Text (Result.Name.Length) = ' '
               loop
                  pragma Loop_Variant (Decreases => Result.Name.Length);
                  Result.Name.Length := Result.Name.Length - 1;
               end loop;
               exit;
            end if;
         end;
      end loop;
      return Result;
   end Identify;

   ---------------------------------------------------------------------------
   type Detailed_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Timing : Monitor_EDID.Timing;
         when False => null;
      end case;
   end record;

   --  One 18-byte detailed timing (table 3.21). Interlaced, stereo and
   --  bordered timings, display descriptors and malformed geometry are not
   --  modes (as the parent's Decode).
   function Detailed (Data : Block; At_Byte : Descriptor_Offset) return Detailed_Decoding is
      subtype Field_12 is Natural range 0 .. 4095;
      function Low_High (Low, High : Unsigned_8) return Field_12 is
        (Natural (Low) + Natural (High / 16) * 256);
      function Low_Low (Low, High : Unsigned_8) return Field_12 is
        (Natural (Low) + Natural (High mod 16) * 256);
      D : constant Descriptor_Offset := At_Byte;
      Clock : constant Natural range 0 .. 65_535 := Natural (Data (D)) + Natural (Data (D + 1)) * 256;
      Width : constant Field_12 := Low_High (Data (D + 2), Data (D + 4));
      HB : constant Field_12 := Low_Low (Data (D + 3), Data (D + 4));
      Height : constant Field_12 := Low_High (Data (D + 5), Data (D + 7));
      VB : constant Field_12 := Low_Low (Data (D + 6), Data (D + 7));
      HX : constant Natural range 0 .. 1023 := Natural (Data (D + 8)) +
        Natural (Shift_Right (Data (D + 11), 6)) * 256;
      HW : constant Natural range 0 .. 1023 := Natural (Data (D + 9)) +
        Natural (Shift_Right (Data (D + 11), 4) and 3) * 256;
      VX : constant Natural range 0 .. 63 := Natural (Shift_Right (Data (D + 10), 4)) +
        Natural (Shift_Right (Data (D + 11), 2) and 3) * 16;
      VW : constant Natural range 0 .. 63 := Natural (Data (D + 10) and 15) +
        Natural (Data (D + 11) and 3) * 16;
      Flags : constant Unsigned_8 := Data (D + 17);
      Interlace_Stereo : constant Unsigned_8 := 16#E0#;
      Sync : Sync_Info;
   begin
      if Clock = 0 or else (Flags and Interlace_Stereo) /= 0 or else
        Data (D + 15) /= 0 or else Data (D + 16) /= 0 or else
        Width = 0 or else Height = 0 or else HB = 0 or else VB = 0 or else
        HW = 0 or else VW = 0 or else HX + HW > HB or else VX + VW > VB
      then
         return (Valid => False);
      end if;
      case Shift_Right (Flags, 3) and 3 is
         when 0 =>
            Sync := (Kind => Analog_Composite,
                     Analog_Serrations => (Flags and 4) /= 0,
                     Sync_On_All_Channels => (Flags and 2) /= 0);
         when 1 =>
            Sync := (Kind => Bipolar_Analog_Composite,
                     Analog_Serrations => (Flags and 4) /= 0,
                     Sync_On_All_Channels => (Flags and 2) /= 0);
         when 2 =>
            Sync := (Digital_Composite,
                     Composite_Positive => (Flags and 2) /= 0,
                     Digital_Serrations => (Flags and 4) /= 0);
         when others =>
            Sync := (Digital_Separate,
                     Horizontal_Positive => (Flags and 2) /= 0,
                     Vertical_Positive => (Flags and 4) /= 0);
      end case;
      return (True, (Width, Height, HB, VB, Clock,
                     Low_High (Data (D + 12), Data (D + 14)), Low_Low (Data (D + 13), Data (D + 14)),
                     HX, HW, VX, VW, Sync));
   end Detailed;

   function Size_Of (Base : Block) return Physical_Size is
      Preferred : constant Detailed_Decoding := Detailed (Base, Base_Descriptor_First);
   begin
      if Preferred.Valid and then Preferred.Timing.Width_MM /= 0 and then
        Preferred.Timing.Height_MM /= 0
      then
         return (Preferred.Timing.Width_MM, Preferred.Timing.Height_MM);
      elsif Base (Horizontal_CM_Byte) /= 0 and then Base (Vertical_CM_Byte) /= 0 then
         --  One zero byte gives an aspect ratio, not a size.
         return (Dimension_MM (Natural (Base (Horizontal_CM_Byte)) * MM_Per_CM),
                 Dimension_MM (Natural (Base (Vertical_CM_Byte)) * MM_Per_CM));
      end if;
      return (0, 0);
   end Size_Of;

   ---------------------------------------------------------------------------
   --  CTA-861-G table 1 (the codes CuBit knows): totals, porches, sync and
   --  polarity. Pixel clocks in 10 kHz.
   ---------------------------------------------------------------------------
   function Code_Timing (Code : Video_Identification) return Known_Code is
      function Make (W, H, HFront, HSync, HBlank, VFront, VSync, VBlank : Positive;
                     Clock : Pixel_Clock_10kHz; Positive_Sync : Boolean) return Known_Code
      is
        ((Known => True,
          Timing => (Width => W, Height => H, Horizontal_Blank => HBlank,
                     Vertical_Blank => VBlank, Clock => Clock,
                     Width_MM => 0, Height_MM => 0,
                     Horizontal_Front_Porch => HFront, Horizontal_Sync_Width => HSync,
                     Vertical_Front_Porch => VFront, Vertical_Sync_Width => VSync,
                     Sync => (Digital_Separate, Positive_Sync, Positive_Sync))))
        with Pre => W <= Extent'Last and then H <= Extent'Last and then
                    HBlank <= Blanking'Last and then VBlank <= Blanking'Last and then
                    HFront <= 1023 and then HSync <= 1023 and then
                    VFront <= 63 and then VSync <= 63 and then
                    HFront + HSync <= HBlank and then VFront + VSync <= VBlank;
   begin
      case Code is
         --  640x480p59.94 (25.175 MHz, to the nearest 10 kHz).
         when 1 => return Make (640, 480, 16, 96, 160, 10, 2, 45, 2_518, False);
         when 2 | 3 => return Make (720, 480, 16, 62, 138, 9, 6, 45, 2_700, False);
         when 4 => return Make (1280, 720, 110, 40, 370, 5, 5, 30, 7_425, True);
         when 16 => return Make (1920, 1080, 88, 44, 280, 4, 5, 45, 14_850, True);
         when 17 | 18 => return Make (720, 576, 12, 64, 144, 5, 5, 49, 2_700, False);
         when 19 => return Make (1280, 720, 440, 40, 700, 5, 5, 30, 7_425, True);
         when 31 => return Make (1920, 1080, 528, 44, 720, 4, 5, 45, 14_850, True);
         when 32 => return Make (1920, 1080, 638, 44, 830, 4, 5, 45, 7_425, True);
         when 33 => return Make (1920, 1080, 528, 44, 720, 4, 5, 45, 7_425, True);
         when 34 => return Make (1920, 1080, 88, 44, 280, 4, 5, 45, 7_425, True);
         when 63 => return Make (1920, 1080, 88, 44, 280, 4, 5, 45, 29_700, True);
         when 64 => return Make (1920, 1080, 528, 44, 720, 4, 5, 45, 29_700, True);
         when 95 => return Make (3840, 2160, 176, 88, 560, 8, 10, 90, 29_700, True);
         when 97 => return Make (3840, 2160, 176, 88, 560, 8, 10, 90, 59_400, True);
         when others => return (Known => False);
      end case;
   end Code_Timing;

   ---------------------------------------------------------------------------
   --  VESA DMT 1.0 r13: totals, porches, sync and polarity. Pixel clocks in
   --  10 kHz.
   ---------------------------------------------------------------------------
   function DMT_Timing (Item : DMT_Mode) return Monitor_EDID.Timing is
      function Make (W, H, HFront, HSync, HBlank, VFront, VSync, VBlank : Positive;
                     Clock : Pixel_Clock_10kHz; H_Positive, V_Positive : Boolean)
        return Monitor_EDID.Timing
      is
        ((Width => W, Height => H, Horizontal_Blank => HBlank,
          Vertical_Blank => VBlank, Clock => Clock,
          Width_MM => 0, Height_MM => 0,
          Horizontal_Front_Porch => HFront, Horizontal_Sync_Width => HSync,
          Vertical_Front_Porch => VFront, Vertical_Sync_Width => VSync,
          Sync => (Digital_Separate, H_Positive, V_Positive)))
        with Pre => W <= Extent'Last and then H <= Extent'Last and then
                    HBlank <= Blanking'Last and then VBlank <= Blanking'Last and then
                    HFront <= 1023 and then HSync <= 1023 and then
                    VFront <= 63 and then VSync <= 63 and then
                    HFront + HSync <= HBlank and then VFront + VSync <= VBlank;
   begin
      case Item is
         when DMT_640x480_60 => return Make (640, 480, 16, 96, 160, 10, 2, 45, 2_518, False, False);
         when DMT_640x480_72 => return Make (640, 480, 24, 40, 192, 9, 3, 40, 3_150, False, False);
         when DMT_640x480_75 => return Make (640, 480, 16, 64, 200, 1, 3, 20, 3_150, False, False);
         when DMT_720x400_70 => return Make (720, 400, 18, 108, 180, 12, 2, 49, 2_832, False, True);
         when DMT_800x600_56 => return Make (800, 600, 24, 72, 224, 1, 2, 25, 3_600, True, True);
         when DMT_800x600_60 => return Make (800, 600, 40, 128, 256, 1, 4, 28, 4_000, True, True);
         when DMT_800x600_72 => return Make (800, 600, 56, 120, 240, 37, 6, 66, 5_000, True, True);
         when DMT_800x600_75 => return Make (800, 600, 16, 80, 256, 1, 3, 25, 4_950, True, True);
         when DMT_1024x768_60 => return Make (1024, 768, 24, 136, 320, 3, 6, 38, 6_500, False, False);
         when DMT_1024x768_70 => return Make (1024, 768, 24, 136, 304, 3, 6, 38, 7_500, False, False);
         when DMT_1024x768_75 => return Make (1024, 768, 16, 96, 288, 1, 3, 32, 7_875, True, True);
         when DMT_1152x864_75 => return Make (1152, 864, 64, 128, 448, 1, 3, 36, 10_800, True, True);
         when DMT_1280x720_60 => return Make (1280, 720, 110, 40, 370, 5, 5, 30, 7_425, True, True);
         when DMT_1280x800_60 => return Make (1280, 800, 72, 128, 400, 3, 6, 31, 8_350, False, True);
         when DMT_1280x960_60 => return Make (1280, 960, 96, 112, 520, 1, 3, 40, 10_800, True, True);
         when DMT_1280x1024_60 => return Make (1280, 1024, 48, 112, 408, 1, 3, 42, 10_800, True, True);
         when DMT_1280x1024_75 => return Make (1280, 1024, 16, 144, 408, 1, 3, 42, 13_500, True, True);
         when DMT_1366x768_60 => return Make (1366, 768, 70, 143, 426, 3, 3, 30, 8_550, True, True);
         when DMT_1440x900_60 => return Make (1440, 900, 80, 152, 464, 3, 6, 34, 10_650, False, True);
         when DMT_1600x900_60 => return Make (1600, 900, 24, 80, 200, 1, 3, 100, 10_800, True, True);
         when DMT_1600x1200_60 => return Make (1600, 1200, 64, 192, 560, 1, 3, 50, 16_200, True, True);
         when DMT_1680x1050_60 => return Make (1680, 1050, 104, 176, 560, 3, 6, 39, 14_625, False, True);
         when DMT_1920x1080_60 => return Make (1920, 1080, 88, 44, 280, 4, 5, 45, 14_850, True, True);
         when DMT_1920x1200_60 => return Make (1920, 1200, 48, 32, 160, 3, 6, 35, 15_400, True, False);
         when DMT_2048x1152_60 => return Make (2048, 1152, 26, 80, 202, 1, 3, 48, 16_200, True, True);
      end case;
   end DMT_Timing;

   function DMT_Refresh (Item : DMT_Mode) return Positive is
     (case Item is
        when DMT_640x480_60 | DMT_800x600_60 | DMT_1024x768_60 | DMT_1280x720_60 | DMT_1280x800_60 |
             DMT_1280x960_60 | DMT_1280x1024_60 | DMT_1366x768_60 | DMT_1440x900_60 | DMT_1600x900_60 |
             DMT_1600x1200_60 | DMT_1680x1050_60 | DMT_1920x1080_60 | DMT_1920x1200_60 |
             DMT_2048x1152_60 => 60,
        when DMT_640x480_72 | DMT_800x600_72 => 72,
        when DMT_640x480_75 | DMT_800x600_75 | DMT_1024x768_75 | DMT_1152x864_75 | DMT_1280x1024_75 => 75,
        when DMT_720x400_70 | DMT_1024x768_70 => 70,
        when DMT_800x600_56 => 56);

   --  The established-timing bits (E-EDID 1.4 table 3.18) of the DMT modes
   --  CuBit knows: byte 35 bits 7 .. 0, byte 36 bits 7 .. 0.
   Established_First : constant := 35;
   type Established_Bit is record
      Byte : Natural range Established_First .. Established_First + 1;
      Bit : Natural range 0 .. 7;
      Mode : DMT_Mode;
   end record;
   Established_Bits : constant array (1 .. 12) of Established_Bit :=
     [(35, 7, DMT_720x400_70), (35, 5, DMT_640x480_60), (35, 3, DMT_640x480_72),
      (35, 2, DMT_640x480_75), (35, 1, DMT_800x600_56), (35, 0, DMT_800x600_60),
      (36, 7, DMT_800x600_72), (36, 6, DMT_800x600_75), (36, 3, DMT_1024x768_60),
      (36, 2, DMT_1024x768_70), (36, 1, DMT_1024x768_75), (36, 0, DMT_1280x1024_75)];

   --  Standard timings (E-EDID 1.4 3.9): eight two-byte slots; 01 01 is an
   --  unused slot. Width (first byte + 31) * 8; aspect in the second byte's
   --  top two bits (16:10, 4:3, 5:4, 16:9); refresh its low six bits + 60.
   Standard_First : constant := 38;
   Standard_Slots : constant := 8;
   Unused_Slot : constant := 1;
   Standard_Width_Offset : constant := 31;
   Standard_Width_Unit : constant := 8;
   Standard_Refresh_Base : constant := 60;
   Aspect_Shift : constant := 64;
   Refresh_Field : constant := 64;
   type Aspect is (Aspect_16_10, Aspect_4_3, Aspect_5_4, Aspect_16_9);
   function Height_For (Width : Natural; Ratio : Aspect) return Natural is
     (case Ratio is
        when Aspect_16_10 => Width * 10 / 16,
        when Aspect_4_3 => Width * 3 / 4,
        when Aspect_5_4 => Width * 4 / 5,
        when Aspect_16_9 => Width * 9 / 16)
     with Pre => Width <= 2_288;

   ---------------------------------------------------------------------------
   function Same_Mode (A, B : Monitor_EDID.Timing) return Boolean is
     (A.Width = B.Width and then A.Height = B.Height and then A.Clock = B.Clock and then
      A.Horizontal_Blank = B.Horizontal_Blank and then A.Vertical_Blank = B.Vertical_Blank);

   procedure Add (List : in out Mode_List; Item : Mode)
     with Post => List.Count >= List.Count'Old and then List.Count - List.Count'Old <= 1
   is
   begin
      for I in 1 .. List.Count loop
         if Same_Mode (List.Items (I).Timing, Item.Timing) then
            return;
         end if;
      end loop;
      if List.Count < List.Capacity then
         List.Count := List.Count + 1;
         List.Items (List.Count) := Item;
      end if;
   end Add;

   function Valid_CTA (Data : Block) return Boolean is
     (Data (0) = CTA_Tag and then Valid_Checksum (Data) and then
      Natural (Data (CTA_DTD_Offset_Byte)) in CTA_Data_First .. Block_Bytes - 1);

   --  Calls Visit for each data block of a valid CTA extension: its tag and
   --  payload (the bytes after its header), all inside the data collection.
   generic
      with procedure Visit (Tag : Natural; First, Length : Natural; Data : Block);
   procedure Each_Data_Block (Data : Block)
     with Pre => Valid_CTA (Data);
   procedure Each_Data_Block (Data : Block) is
      Limit : constant Natural := Natural (Data (CTA_DTD_Offset_Byte));
      At_Byte : Natural := CTA_Data_First;
   begin
      while At_Byte < Limit loop
         pragma Loop_Invariant (At_Byte in CTA_Data_First .. Limit - 1);
         pragma Loop_Variant (Increases => At_Byte);
         declare
            Header : constant Unsigned_8 := Data (At_Byte);
            Tag : constant Natural := Natural (Header / Data_Block_Header_Length_Bits);
            Length : constant Natural := Natural (Header mod Data_Block_Header_Length_Bits);
         begin
            exit when Length > Limit - At_Byte - 1;
            Visit (Tag, At_Byte + 1, Length, Data);
            At_Byte := At_Byte + 1 + Length;
         end;
      end loop;
   end Each_Data_Block;

   procedure Collect (Base : Block; Extensions : Block_Array; List : in out Mode_List) is

      procedure Add_Video_Codes (Tag : Natural; First, Length : Natural; Data : Block)
        with Pre => Length <= Block_Bytes and then First <= Block_Bytes - Length;
      procedure Add_Video_Codes (Tag : Natural; First, Length : Natural; Data : Block) is
      begin
         if Tag /= Video_Data_Block then
            return;
         end if;
         for I in First .. First + Length - 1 loop
            declare
               Byte : constant Natural := Natural (Data (I));
               Native : constant Boolean := Byte in SVD_Native_First .. SVD_Native_Last;
               Code : constant Video_Identification :=
                 (if Native then Byte - SVD_Native_Offset else Byte);
               Known : constant Known_Code := Code_Timing (Code);
            begin
               if Known.Known then
                  Add (List, (Known.Timing, Video_Code, Code, False, Native));
               end if;
            end;
         end loop;
      end Add_Video_Codes;
      procedure Each_Video is new Each_Data_Block (Add_Video_Codes);
   begin
      List.Count := 0;
      for D in 0 .. Base_Descriptors - 1 loop
         declare
            Item : constant Detailed_Decoding :=
              Detailed (Base, Base_Descriptor_First + D * Descriptor_Bytes);
         begin
            if Item.Valid then
               Add (List, (Item.Timing, Base_Detailed, 0, D = 0, False));
            end if;
         end;
      end loop;
      for E in Extensions'Range loop
         if Valid_CTA (Extensions (E)) then
            declare
               At_Byte : Natural := Natural (Extensions (E) (CTA_DTD_Offset_Byte));
            begin
               while At_Byte <= Block_Bytes - 1 - Descriptor_Bytes loop
                  pragma Loop_Variant (Increases => At_Byte);
                  declare
                     Item : constant Detailed_Decoding := Detailed (Extensions (E), At_Byte);
                  begin
                     --  A zero clock ends the detailed timings (padding follows).
                     exit when Extensions (E) (At_Byte) = 0 and then Extensions (E) (At_Byte + 1) = 0;
                     if Item.Valid then
                        Add (List, (Item.Timing, Extension_Detailed, 0, False, False));
                     end if;
                  end;
                  At_Byte := At_Byte + Descriptor_Bytes;
               end loop;
            end;
         end if;
      end loop;
      for E in Extensions'Range loop
         if Valid_CTA (Extensions (E)) then
            Each_Video (Extensions (E));
         end if;
      end loop;
      for Item of Established_Bits loop
         if (Base (Item.Byte) and Shift_Left (Unsigned_8'(1), Item.Bit)) /= 0 then
            Add (List, (DMT_Timing (Item.Mode), Established, 0, False, False));
         end if;
      end loop;
      for Slot in 0 .. Standard_Slots - 1 loop
         declare
            First : constant Unsigned_8 := Base (Standard_First + 2 * Slot);
            Second : constant Unsigned_8 := Base (Standard_First + 2 * Slot + 1);
            Width : constant Natural := (Natural (First) + Standard_Width_Offset) * Standard_Width_Unit;
            Height : constant Natural := Height_For (Width, Aspect'Val (Natural (Second) / Aspect_Shift));
            Refresh : constant Natural := Natural (Second) mod Refresh_Field + Standard_Refresh_Base;
         begin
            if not (First = Unused_Slot and then Second = Unused_Slot) and then First /= 0 then
               for Known in DMT_Mode loop
                  if DMT_Timing (Known).Width = Width and then DMT_Timing (Known).Height = Height and then
                    DMT_Refresh (Known) = Refresh
                  then
                     Add (List, (DMT_Timing (Known), Standard, 0, False, False));
                  end if;
               end loop;
            end if;
         end;
      end loop;
   end Collect;

   function Modes (Base : Block; Extensions : Block_Array) return Mode_List is
   begin
      return List : Mode_List (Capacity (Extensions'Length)) do
         Collect (Base, Extensions, List);
      end return;
   end Modes;

   function Limits (Base : Block; Extensions : Block_Array) return Sink_Limits is
      Result : Sink_Limits;
      function OUI_Is (Data : Block; First : Natural; OUI : OUI_Bytes) return Boolean is
        (Data (First) = OUI (0) and then Data (First + 1) = OUI (1) and then Data (First + 2) = OUI (2))
        with Pre => First <= Block_Bytes - 3;
      procedure Raise_TMDS (Units : Unsigned_8) is
         Rate : constant Clock_kHz := Natural (Units) * TMDS_Unit_kHz;
      begin
         if Rate > Result.Max_TMDS then
            Result.Max_TMDS := Rate;
         end if;
      end Raise_TMDS;
      procedure Vendor (Tag : Natural; First, Length : Natural; Data : Block)
        with Pre => Length <= Block_Bytes and then First <= Block_Bytes - Length;
      procedure Vendor (Tag : Natural; First, Length : Natural; Data : Block) is
      begin
         if Tag /= Vendor_Data_Block or else Length < HDMI_LLC_OUI'Length then
            return;
         end if;
         if OUI_Is (Data, First, HDMI_LLC_OUI) then
            Result.HDMI := True;
            if Length > HDMI_LLC_Max_TMDS then
               Raise_TMDS (Data (First + HDMI_LLC_Max_TMDS));
            end if;
         elsif OUI_Is (Data, First, HDMI_Forum_OUI) and then Length > HDMI_Forum_Flags then
            Raise_TMDS (Data (First + HDMI_Forum_Max_TMDS));
            Result.SCDC := (Data (First + HDMI_Forum_Flags) and SCDC_Present) /= 0;
         end if;
      end Vendor;
      procedure Each_Vendor is new Each_Data_Block (Vendor);
   begin
      for D in 0 .. Base_Descriptors - 1 loop
         declare
            At_Byte : constant Descriptor_Offset := Base_Descriptor_First + D * Descriptor_Bytes;
         begin
            if Display_Descriptor (Base, At_Byte, Range_Limits_Tag) then
               Result.Max_Pixel_Clock :=
                 Natural (Base (At_Byte + Range_Max_Clock_Byte)) * Range_Clock_Unit_kHz;
            end if;
         end;
      end loop;
      for E in Extensions'Range loop
         if Valid_CTA (Extensions (E)) then
            Each_Vendor (Extensions (E));
         end if;
      end loop;
      return Result;
   end Limits;
end CuBit.Monitor_EDID.Catalog;
