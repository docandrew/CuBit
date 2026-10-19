--  EDID catalog: identity, name, size, every mode, HDMI sink limits, and
--  hostile input (every single-bit corruption of every fixture block).
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Monitor_EDID.Catalog;
with EDID_Fixtures; use EDID_Fixtures;
procedure EDID_Tests is
   package C renames CuBit.Monitor_EDID.Catalog;
   package E renames CuBit.Monitor_EDID;
   use type C.Mode_Source;

   function Find (List : C.Mode_List; W, H : Positive; Clock : E.Pixel_Clock_10kHz) return Natural is
   begin
      for I in 1 .. List.Count loop
         if List.Items (I).Timing.Width = W and then List.Items (I).Timing.Height = H and then
           List.Items (I).Timing.Clock = Clock
         then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   procedure Check_UHD is
      Who : constant C.Identity := C.Identify (UHD_HDMI_Base);
      Size : constant C.Physical_Size := C.Size_Of (UHD_HDMI_Base);
      List : constant C.Mode_List := C.Modes (UHD_HDMI_Base, UHD_HDMI_Extensions);
      Sink : constant C.Sink_Limits := C.Limits (UHD_HDMI_Base, UHD_HDMI_Extensions);
      Preferred : constant Natural := Find (List, 3840, 2160, 59_400);
   begin
      pragma Assert (C.Valid_Checksum (UHD_HDMI_Base));
      pragma Assert (C.Extensions_Declared (UHD_HDMI_Base) = 1);
      pragma Assert (Who.Vendor = "CUB" and then Who.Product = 16#2701# and then
                     Who.Serial = 16#00C0FFEE# and then Who.Year = 2023);
      pragma Assert (Who.Name.Text (1 .. Who.Name.Length) = "CuBit UHD27");
      --  The preferred DTD's millimetres win over the base block's centimetres.
      pragma Assert (Size.Width_MM = 597 and then Size.Height_MM = 336);
      pragma Assert (List.Capacity = C.Capacity (1));
      pragma Assert (Preferred = 1 and then List.Items (1).Preferred and then
                     List.Items (1).Source = C.Base_Detailed);
      pragma Assert (E.Refresh_Millihertz (List.Items (1).Timing) = 60_000);
      --  4K30 DTD, 1080p60 and 1440p60 extension DTDs.
      pragma Assert (Find (List, 3840, 2160, 29_700) /= 0);
      pragma Assert (List.Items (Find (List, 2560, 1440, 24_150)).Source = C.Extension_Detailed);
      pragma Assert (List.Items (Find (List, 1920, 1080, 14_850)).Source = C.Extension_Detailed);
      --  Short codes: VIC 97 and 95 duplicate the DTDs (dropped), VIC 4, 31,
      --  3 and 1 are new; VIC 16 duplicates the 1080p60 DTD.
      pragma Assert (List.Items (Find (List, 1280, 720, 7_425)).Code = 4);
      pragma Assert (List.Items (Find (List, 1920, 1080, 14_850)).Code = 0);
      pragma Assert (Find (List, 720, 480, 2_700) /= 0 and then Find (List, 640, 480, 2_518) /= 0);
      for I in 1 .. List.Count loop
         for J in I + 1 .. List.Count loop
            pragma Assert (not (List.Items (I).Timing.Width = List.Items (J).Timing.Width and then
                                List.Items (I).Timing.Height = List.Items (J).Timing.Height and then
                                List.Items (I).Timing.Clock = List.Items (J).Timing.Clock and then
                                List.Items (I).Timing.Horizontal_Blank =
                                  List.Items (J).Timing.Horizontal_Blank));
         end loop;
      end loop;
      pragma Assert (List.Count = 8);
      pragma Assert (Sink.HDMI and then Sink.SCDC and then Sink.Max_TMDS = 600_000 and then
                     Sink.Max_Pixel_Clock = 600_000);
   end Check_UHD;

   procedure Check_Others is
      Old : constant C.Sink_Limits := C.Limits (UHD_HDMI14_Base, UHD_HDMI14_Extensions);
      Panel : constant C.Mode_List := C.Modes (FHD_EDP_Base, FHD_EDP_Extensions);
      Panel_Who : constant C.Identity := C.Identify (FHD_EDP_Base);
      DVI : constant C.Sink_Limits := C.Limits (DVI_1080_Base, DVI_1080_Extensions);
      DVI_Who : constant C.Identity := C.Identify (DVI_1080_Base);
   begin
      pragma Assert (Old.HDMI and then not Old.SCDC and then Old.Max_TMDS = 340_000);
      pragma Assert (Panel.Count = 1 and then Panel.Items (1).Preferred);
      pragma Assert (Panel_Who.Name.Length = 0);
      pragma Assert (C.Size_Of (FHD_EDP_Base).Width_MM = 309);
      pragma Assert (not DVI.HDMI and then DVI.Max_TMDS = 0);
      pragma Assert (DVI_Who.Name.Text (1 .. DVI_Who.Name.Length) = "Old");
   end Check_Others;

   --  Every single-bit corruption of every block: never an exception, and a
   --  corrupted extension contributes nothing (its checksum fails).
   procedure Check_Corruption is
      Base : C.Block;
      Extensions : C.Block_Array := UHD_HDMI_Extensions;
      Clean : constant C.Mode_List := C.Modes (UHD_HDMI_Base, C.Block_Array'(1 .. 0 => <>));
      Count : Natural := 0;
   begin
      for Byte in C.Block'Range loop
         for Bit in 0 .. 7 loop
            Base := UHD_HDMI_Base;
            Base (Byte) := Base (Byte) xor Shift_Left (Unsigned_8 (1), Bit);
            declare
               List : constant C.Mode_List := C.Modes (Base, UHD_HDMI_Extensions);
               Who : constant C.Identity := C.Identify (Base);
               Sink : constant C.Sink_Limits := C.Limits (Base, UHD_HDMI_Extensions);
               Size : constant C.Physical_Size := C.Size_Of (Base);
            begin
               pragma Assert (List.Count <= List.Capacity and then Who.Name.Length <= C.Name_Length);
               pragma Assert (Sink.Max_TMDS = 600_000 and then Size.Width_MM <= E.Dimension_MM'Last);
            end;
            Extensions := UHD_HDMI_Extensions;
            Extensions (1) (Byte) := Extensions (1) (Byte) xor Shift_Left (Unsigned_8 (1), Bit);
            declare
               List : constant C.Mode_List := C.Modes (UHD_HDMI_Base, Extensions);
               Sink : constant C.Sink_Limits := C.Limits (UHD_HDMI_Base, Extensions);
            begin
               pragma Assert (List.Count = Clean.Count and then not Sink.HDMI);
            end;
            Count := Count + 2;
         end loop;
      end loop;
      Put_Line ("PASS: EDID catalog," & Count'Image & " single-bit corruptions");
   end Check_Corruption;

   --  An extension whose data blocks claim more bytes than the collection
   --  holds is cut off at the bad block, never read past it.
   procedure Check_Overrun is
      Extensions : C.Block_Array := UHD_HDMI_Extensions;
      Sum : Unsigned_8 := 0;
   begin
      Extensions (1) (4) := 16#5F#;  --  video block, 31 bytes (past the DTD offset)
      Extensions (1) (127) := 0;
      for B of Extensions (1) loop Sum := Sum + B; end loop;
      Extensions (1) (127) := 0 - Sum;
      declare
         List : constant C.Mode_List := C.Modes (UHD_HDMI_Base, Extensions);
      begin
         pragma Assert (List.Count = 4);  --  two base and two extension DTDs
      end;
   end Check_Overrun;
begin
   Check_UHD;
   Check_Others;
   Check_Corruption;
   Check_Overrun;
   for Code in C.Video_Identification loop
      declare
         K : constant C.Known_Code := C.Code_Timing (Code);
      begin
         if K.Known then
            pragma Assert (K.Timing.Horizontal_Front_Porch + K.Timing.Horizontal_Sync_Width <=
                           K.Timing.Horizontal_Blank);
         end if;
      end;
   end loop;
   Put_Line ("PASS: EDID identity, size, modes and HDMI sink limits");
end EDID_Tests;
