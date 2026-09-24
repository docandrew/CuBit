with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Monitor_EDID;
procedure Main is
   package E renames CuBit.Monitor_EDID;
   use type E.Decode_Status;
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
