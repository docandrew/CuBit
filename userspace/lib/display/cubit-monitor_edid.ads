pragma Ada_2022;
with Interfaces;

--  Untrusted monitor metadata, not device authority or proof that a hardware
--  link can drive a mode. Only the base block's preferred progressive DTD is
--  decoded here; extensions and other timing encodings are deliberately absent.
package CuBit.Monitor_EDID with SPARK_Mode, Pure is
   type Base_Block is array (Natural range 0 .. 127) of Interfaces.Unsigned_8;
   subtype Extent is Positive range 1 .. 4095;
   subtype Dimension_MM is Natural range 0 .. 4095;
   subtype Pixel_Clock_10kHz is Positive range 1 .. 65_535;
   subtype Blanking is Positive range 1 .. 4095;
   type Sync_Kind is
     (Analog_Composite, Bipolar_Analog_Composite, Digital_Composite, Digital_Separate);
   -- Decoded semantics, not a memory-mapped register or wire overlay.
   -- VESA E-EDID A2 table 3.22: bits 2/1 have different meanings by kind.
   type Sync_Info (Kind : Sync_Kind := Digital_Separate) is record
      case Kind is
         when Digital_Separate =>
            Horizontal_Positive, Vertical_Positive : Boolean := False;
         when Digital_Composite =>
            Composite_Positive, Digital_Serrations : Boolean := False;
         when Analog_Composite | Bipolar_Analog_Composite =>
            Analog_Serrations, Sync_On_All_Channels : Boolean := False;
      end case;
   end record;
   type Timing is record
      Width, Height : Extent := 1;
      Horizontal_Blank, Vertical_Blank : Blanking := 1;
      Clock : Pixel_Clock_10kHz := 1;
      Width_MM, Height_MM : Dimension_MM := 0; -- zero means unspecified
      Horizontal_Front_Porch : Natural range 0 .. 1023 := 0;
      Horizontal_Sync_Width : Positive range 1 .. 1023 := 1;
      Vertical_Front_Porch : Natural range 0 .. 63 := 0;
      Vertical_Sync_Width : Positive range 1 .. 63 := 1;
      Sync : Sync_Info;
   end record;
   type Decode_Status is
     (Accepted, Bad_Header, Bad_Checksum, Unsupported_Version,
      No_Preferred_Timing, Unsupported_Timing, Invalid_Timing);
   type Result (Status : Decode_Status := Bad_Header) is record
      case Status is
         when Accepted => Preferred : Timing;
         when others => null;
      end case;
   end record;
   function Decode (Data : Base_Block) return Result
     with Post => (if Decode'Result.Status = Accepted then
       Decode'Result.Preferred.Horizontal_Front_Porch +
         Decode'Result.Preferred.Horizontal_Sync_Width <=
           Decode'Result.Preferred.Horizontal_Blank and then
       Decode'Result.Preferred.Vertical_Front_Porch +
         Decode'Result.Preferred.Vertical_Sync_Width <=
           Decode'Result.Preferred.Vertical_Blank);
   --  Advertised nominal rate, not measured refresh or a presentation deadline.
   function Refresh_Millihertz (Mode : Timing) return Interfaces.Unsigned_64;
   --  Page-rounded storage per BGRA buffer. Each mapping starts on its own
   --  page, including modes whose visible byte length is not page-aligned.
   function Buffer_Bytes (Width, Height : Extent) return Positive
     with Post => Buffer_Bytes'Result mod 4096 = 0 and then
       Buffer_Bytes'Result >= Width * Height * 4;
end CuBit.Monitor_EDID;
