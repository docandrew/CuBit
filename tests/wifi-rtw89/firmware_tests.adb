--  Firmware container parsing (RTW89_Firmware) against the pinned
--  linux-firmware rtw8852b_fw-1.bin (headers only) and synthetic images.
--  Expected values: docs/wifi.md and rtw89 fw.c semantics.
pragma Ada_2022;
with Ada.Environment_Variables;
with Ada.Streams.Stream_IO; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with RTW89_Firmware; use RTW89_Firmware;
with Test_Support; use Test_Support;

procedure Firmware_Tests is
   type File_Access is access Byte_Array;

   function Load (Path : String) return File_Access is
      F : Stream_IO.File_Type;
   begin
      Stream_IO.Open (F, Stream_IO.In_File, Path);
      declare
         Size : constant Natural := Natural (Stream_IO.Size (F));
         Raw  : Stream_Element_Array (1 .. Stream_Element_Offset (Size));
         Last : Stream_Element_Offset;
         Data : constant File_Access := new Byte_Array (0 .. Size - 1);
      begin
         Stream_IO.Read (F, Raw, Last);
         Stream_IO.Close (F);
         for I in Raw'First .. Last loop
            Data (Natural (I) - 1) := Unsigned_8 (Raw (I));
         end loop;
         return Data;
      end;
   end Load;

   procedure Real_File (Path : String) is
      File : constant File_Access := Load (Path);
      A, B, C, Z, N : Image;
      P : Plan;
   begin
      Check (File'Length = 1_245_944, "real: size of rtw8852b_fw-1.bin");
      A := Select_Image (File.all, Normal_CE, 1);
      B := Select_Image (File.all, Normal_CE, 2);
      C := Select_Image (File.all, Normal_CE, 3);
      Z := Select_Image (File.all, Normal_CE, 0);
      N := Select_Image (File.all, Normal, 2);
      Check (A.Found and then A.Offset = 16#60# and then A.Size = 16#4FBD8#, "real: cut B image");
      Check (B.Found and then B.Offset = 16#4FC38# and then B.Size = 16#4FB88#, "real: cut C image");
      Check (C = B, "real: cut D takes the cut C image");
      Check (not Z.Found, "real: no image for cut A");
      Check (not N.Found, "real: no plain Normal image");

      P := Plan_Image (File.all, A);
      Check (P.Status = Planned, "real: cut B plan");
      Check (P.Header = 16#60# and then P.Header_Bytes = 80, "real: header H2C is 80 bytes");
      Check (P.Sections = 3, "real: three sections");
      Check (P.Items (1).Data = 16#60# + 16#90# and then P.Items (1).Length = 16#4BED0#
             and then P.Items (1).Address = 16#1897_0000# and then P.Items (1).Kind = 2,
             "real: section 1");
      Check (P.Items (2).Data = 16#60# + 16#4BF60# and then P.Items (2).Length = 16#3478#
             and then P.Items (2).Address = 16#18E1_2400#, "real: section 2");
      Check (P.Items (3).Data = 16#60# + 16#4F3D8# and then P.Items (3).Length = 16#800#
             and then P.Items (3).Kind = Security_Section, "real: security section");
      Check (Packets (P.Items (1).Length) = 154 and then Packets (P.Items (2).Length) = 7
             and then Packets (P.Items (3).Length) = 2, "real: packet counts");

      P := Plan_Image (File.all, B);
      Check (P.Status = Planned and then P.Sections = 3
             and then P.Items (3).Address = 16#18E1_1920#, "real: cut C plan");
   end Real_File;

   --  A synthetic v0 image: one section of Size bytes after a 48-byte header.
   procedure Synthetic is
      Image_Bytes : constant := Header_Fixed_Bytes + Section_Header_Bytes + 64;
      F : Byte_Array (0 .. MFW_Header_Bytes + MFW_Entry_Bytes + Image_Bytes - 1) :=
        [others => 0];
      Base : constant := MFW_Header_Bytes + MFW_Entry_Bytes;
      Where : Image;
      P : Plan;
   begin
      F (0) := MFW_Signature;
      F (1) := 1;
      F (16) := 1;                     --  cut
      F (17) := Normal_CE_Code;
      F (20) := Base;                  --  offset
      F (24) := Image_Bytes;           --  size
      F (Base + 25) := 1;              --  one section
      F (Base + 32 + 4) := 64;         --  section size 64
      F (Base + 32 + 7) := 2;          --  type 2
      Where := Select_Image (F, Normal_CE, 1);
      Check (Where.Found and then Where.Offset = Base, "synthetic: selected");
      P := Plan_Image (F, Where);
      Check (P.Status = Planned and then P.Sections = 1
             and then P.Items (1).Data = Base + 48 and then P.Items (1).Length = 64,
             "synthetic: planned");

      F (Base + 32 + 4) := 65;
      Check (Plan_Image (F, Where).Status = Section_Overruns_Image, "synthetic: overrun refused");
      F (Base + 32 + 4) := 63;
      Check (Plan_Image (F, Where).Status = Size_Mismatch, "synthetic: short sections refused");
      F (Base + 32 + 4) := 56;
      F (Base + 32 + 7) := 16#12#;     --  checksum bit: 56 + 8
      Check (Plan_Image (F, Where).Status = Planned
             and then Plan_Image (F, Where).Items (1).Length = 64, "synthetic: checksum adds 8");
      F (Base + 32 + 4) := 64;
      F (Base + 32 + 7) := Security_Section;
      F (Base + 32 + 8) := 16#FF#;
      Check (Plan_Image (F, Where).Status = Formatted_Signatures, "synthetic: MSS pool refused");
      F (Base + 32 + 8) := 1;          --  one 512-byte signature: past the end
      Check (Plan_Image (F, Where).Status = Section_Overruns_Image, "synthetic: signatures counted");
      F (Base + 32 + 8) := 0;
      F (Base + 25) := 0;
      Check (Plan_Image (F, Where).Status = Bad_Section_Count, "synthetic: zero sections refused");
      F (Base + 25) := 11;
      Check (Plan_Image (F, Where).Status = Bad_Section_Count, "synthetic: 11 sections refused");
      F (Base + 25) := 1;
      F (Base + 15) := 1;
      Check (Plan_Image (F, Where).Status = Unsupported_Version, "synthetic: v1 refused");
      F (Base + 15) := 0;

      F (18) := 1;                     --  mass-production image: skipped
      Check (not Select_Image (F, Normal_CE, 1).Found, "synthetic: mp image skipped");
      F (18) := 0;
      F (24) := 0;
      F (25) := 1;                     --  size past the file
      Check (not Select_Image (F, Normal_CE, 1).Found, "synthetic: oversized entry skipped");
      F (1) := 200;
      Check (not Select_Image (F, Normal_CE, 1).Found, "synthetic: entry table past file");
   end Synthetic;
begin
   Synthetic;
   if Ada.Environment_Variables.Exists ("RTW89_FIRMWARE") then
      Real_File (Ada.Environment_Variables.Value ("RTW89_FIRMWARE"));
   else
      Put_Line ("FAIL real firmware file: RTW89_FIRMWARE not set");
      Check (False, "real: RTW89_FIRMWARE names the vendored blob");
   end if;
end Firmware_Tests;
