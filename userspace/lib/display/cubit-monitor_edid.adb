pragma Ada_2022;
package body CuBit.Monitor_EDID with SPARK_Mode is
   use Interfaces;

   function Decode (Data : Base_Block) return Result is
      Header : constant array (Natural range 0 .. 7) of Unsigned_8 :=
        [0, 255, 255, 255, 255, 255, 255, 0];
      Sum : Unsigned_8 := 0;
      subtype Field_12 is Natural range 0 .. 4095;
      function Low_High (Low, High : Unsigned_8) return Field_12 is
        (Natural (Low) + Natural (High / 16) * 256);
      function Low_Low (Low, High : Unsigned_8) return Field_12 is
        (Natural (Low) + Natural (High mod 16) * 256);
      Width : constant Field_12 := Low_High (Data (56), Data (58));
      Height : constant Field_12 := Low_High (Data (59), Data (61));
      HB : constant Field_12 := Low_Low (Data (57), Data (58));
      VB : constant Field_12 := Low_Low (Data (60), Data (61));
      Clock : constant Natural range 0 .. 65_535 :=
        Natural (Data (54)) + Natural (Data (55)) * 256;
      HX : constant Natural := Natural (Data (62)) +
        Natural (Shift_Right (Data (65), 6)) * 256;
      HW : constant Natural := Natural (Data (63)) +
        Natural (Shift_Right (Data (65), 4) and 3) * 256;
      VX : constant Natural := Natural (Shift_Right (Data (64), 4)) +
        Natural (Shift_Right (Data (65), 2) and 3) * 16;
      VW : constant Natural := Natural (Data (64) and 15) +
        Natural (Data (65) and 3) * 16;
   begin
      for I in Header'Range loop
         if Data (I) /= Header (I) then return (Status => Bad_Header); end if;
      end loop;
      for Byte of Data loop Sum := Sum + Byte; end loop;
      if Sum /= 0 then return (Status => Bad_Checksum); end if;
      if Data (18) /= 1 or else Data (19) not in 3 .. 4 then
         return (Status => Unsupported_Version);
      end if;
      if Clock = 0 or else (Data (19) = 3 and then (Data (24) and 2) = 0) then
         return (Status => No_Preferred_Timing);
      end if;
      --  Interlaced, stereo and border modes need additional semantics. Never
      --  silently reinterpret one as a plain progressive buffer.
      if (Data (71) and 16#E1#) /= 0 or else
        Data (69) /= 0 or else Data (70) /= 0
      then
         return (Status => Unsupported_Timing);
      end if;
      if Width = 0 or else Height = 0 or else HB = 0 or else VB = 0 or else
        HW = 0 or else VW = 0 or else HX + HW > HB or else VX + VW > VB
      then
         return (Status => Invalid_Timing);
      end if;
      return (Accepted, (Width, Height, HB, VB, Clock,
        Low_High (Data (66), Data (68)), Low_Low (Data (67), Data (68))));
   end Decode;

   function Refresh_Millihertz (Mode : Timing) return Unsigned_64 is
     (Unsigned_64 (Mode.Clock) * 10_000_000 /
       (Unsigned_64 (Mode.Width + Mode.Horizontal_Blank) *
        Unsigned_64 (Mode.Height + Mode.Vertical_Blank)));

   function Buffer_Bytes (Width, Height : Extent) return Positive is
     (((Width * Height * 4 + 4095) / 4096) * 4096);
end CuBit.Monitor_EDID;
