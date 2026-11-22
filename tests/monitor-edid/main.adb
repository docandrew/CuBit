with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Monitor_EDID;
procedure Main is
   package E renames CuBit.Monitor_EDID;
   use type E.Decode_Status;
   use type E.Sync_Kind;
   Data : E.Base_Block := [others => 0];
   R : E.Result;
   procedure Checksum is
      Sum : Unsigned_8 := 0;
   begin
      Data (127) := 0;
      for Byte of Data loop Sum := Sum + Byte; end loop;
      Data (127) := 0 - Sum;
   end Checksum;
begin
   pragma Assert (E.Decode (Data).Status = E.Bad_Header);
   Data (1 .. 6) := [others => 255];
   Data (18) := 1; Data (19) := 4;
   --  1280x720, 74.25 MHz, totals 1650x750 => exactly 60 Hz.
   Data (54) := 16#01#; Data (55) := 16#1D#;
   Data (56) := 0; Data (57) := 16#72#; Data (58) := 16#51#;
   Data (59) := 16#D0#; Data (60) := 30; Data (61) := 16#20#;
   Data (62) := 110; Data (63) := 40; Data (64) := 16#55#;
   Data (66) := 16#58#; Data (67) := 16#36#; Data (68) := 16#21#;
   Data (71) := 16#1E#;
   Checksum;
   R := E.Decode (Data);
   pragma Assert (R.Status = E.Accepted);
   pragma Assert (R.Preferred.Width = 1280 and R.Preferred.Height = 720);
   pragma Assert (R.Preferred.Width_MM = 600 and R.Preferred.Height_MM = 310);
   pragma Assert (E.Refresh_Millihertz (R.Preferred) = 60_000);
   pragma Assert (R.Preferred.Horizontal_Front_Porch = 110 and
     R.Preferred.Horizontal_Sync_Width = 40 and
     R.Preferred.Vertical_Front_Porch = 5 and
     R.Preferred.Vertical_Sync_Width = 5);
   declare
      Saved : constant E.Base_Block := Data;
   begin
      for Flags in Unsigned_8 loop
         Data (71) := Flags; Checksum;
         R := E.Decode (Data);
         if (Flags and 16#E0#) /= 0 then
            pragma Assert (R.Status = E.Unsupported_Timing);
         else
            pragma Assert (R.Status = E.Accepted);
            case Natural (Shift_Right (Flags, 3) and 3) is
               when 0 | 1 =>
                  pragma Assert (R.Preferred.Sync.Kind =
                    (if (Flags and 8) = 0 then E.Analog_Composite
                     else E.Bipolar_Analog_Composite));
                  pragma Assert (R.Preferred.Sync.Analog_Serrations = ((Flags and 4) /= 0));
                  pragma Assert (R.Preferred.Sync.Sync_On_All_Channels = ((Flags and 2) /= 0));
               when 2 =>
                  pragma Assert (R.Preferred.Sync.Kind = E.Digital_Composite);
                  pragma Assert (R.Preferred.Sync.Digital_Serrations = ((Flags and 4) /= 0));
                  pragma Assert (R.Preferred.Sync.Composite_Positive = ((Flags and 2) /= 0));
               when others =>
                  pragma Assert (R.Preferred.Sync.Kind = E.Digital_Separate);
                  pragma Assert (R.Preferred.Sync.Horizontal_Positive = ((Flags and 2) /= 0));
                  pragma Assert (R.Preferred.Sync.Vertical_Positive = ((Flags and 4) /= 0));
            end case;
         end if;
      end loop;
      Data := Saved;
      Data (57) := 255; Data (58) := 16#5F#;
      Data (60) := 255; Data (61) := 16#2F#;
      Data (62 .. 65) := [others => 255]; Checksum;
      R := E.Decode (Data);
      pragma Assert (R.Status = E.Accepted);
      pragma Assert (R.Preferred.Horizontal_Front_Porch = 1023 and
        R.Preferred.Horizontal_Sync_Width = 1023 and
        R.Preferred.Vertical_Front_Porch = 63 and R.Preferred.Vertical_Sync_Width = 63);
      Data := Saved;
   end;
   --  Every single-bit corruption of the base block must be rejected.
   for I in Data'Range loop
      for Bit in 0 .. 7 loop
         Data (I) := Data (I) xor Shift_Left (Unsigned_8 (1), Bit);
         pragma Assert (E.Decode (Data).Status /= E.Accepted);
         Data (I) := Data (I) xor Shift_Left (Unsigned_8 (1), Bit);
      end loop;
   end loop;
   Data (71) := 16#9E#; Checksum;
   pragma Assert (E.Decode (Data).Status = E.Unsupported_Timing);
   Data (71) := 16#1E#;
   Data (63) := 255; Checksum; -- sync still fits horizontal blanking
   pragma Assert (E.Decode (Data).Status = E.Accepted); -- 110 + 255 <= 370
   Data (62) := 255; Checksum;
   pragma Assert (E.Decode (Data).Status = E.Invalid_Timing);
   Data (62) := 110; Data (63) := 40;
   Data (19) := 3; Checksum;
   pragma Assert (E.Decode (Data).Status = E.No_Preferred_Timing);
   Data (24) := 2; Checksum;
   pragma Assert (E.Decode (Data).Status = E.Accepted);
   Data (18) := 2; Checksum;
   pragma Assert (E.Decode (Data).Status = E.Unsupported_Version);
   --  Exhaustive geometry: padded buffers cover every legal DTD extent and
   --  never expose adjacent-buffer bytes through a rounded grant mapping.
   for W in E.Extent loop
      for H in E.Extent loop
         pragma Assert (E.Buffer_Bytes (W, H) mod 4096 = 0);
         pragma Assert (E.Buffer_Bytes (W, H) >= W * H * 4);
         pragma Assert (E.Buffer_Bytes (W, H) - W * H * 4 < 4096);
      end loop;
   end loop;
   Put_Line ("PASS EDID: preferred timing, corruption, unsupported modes, buffer extents");
end Main;
