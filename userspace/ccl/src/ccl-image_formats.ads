with Interfaces;

--  Streaming decoders for the image files CCL loads: QOI (the "Quite OK
--  Image" format, https://qoiformat.org) and binary PPM (P6). Bytes arrive
--  one at a time, so a file of any length decodes in bounded memory; the
--  decoder never reads past what it was given and rejects anything that is
--  not exactly one well-formed image. Pixels go to the instance's Emit,
--  as 16#RRGGBB# (QOI alpha composited over black).
generic
   Maximum_Side : Positive;
   Maximum_Pixels : Positive;
   --  Called once, with the image's size, before any pixel.
   with procedure Start (Width, Height : Positive);
   with procedure Emit (X, Y : Natural; Pixel : Interfaces.Unsigned_32);
package CCL.Image_Formats with SPARK_Mode is
   use Interfaces;
   type Format is (Unknown_Format, QOI_Format, PPM_Format);
   type Decoder is private;
   procedure Reset (Item : out Decoder);
   procedure Feed (Item : in out Decoder; Byte : Unsigned_8);
   --  After the last byte: whether exactly one complete image was read.
   function Complete (Item : Decoder) return Boolean;
   function Failed (Item : Decoder) return Boolean;
   function Kind (Item : Decoder) return Format;
private
   type Phase is
     (Magic, QOI_Header, QOI_Chunks, QOI_Operand, QOI_End, PPM_Header, PPM_Pixels,
      Done, Broken);
   type RGBA is record
      R, G, B : Unsigned_8 := 0;
      A : Unsigned_8 := 255;
   end record;
   type Color_Index is array (0 .. 63) of RGBA;
   subtype Operand_Count is Natural range 0 .. 4;
   type Operand_Bytes is array (1 .. 4) of Unsigned_8;
   --  PPM header fields: width, height, maxval.
   subtype PPM_Field is Natural range 1 .. 3;
   QOI_END_BYTES : constant := 8;
   type Decoder is record
      Stage : Phase := Magic;
      Image_Format : Format := Unknown_Format;
      Seen : Natural := 0;               --  bytes of the current header part
      Header : Operand_Bytes := [others => 0];
      Width, Height : Natural := 0;
      Position : Natural := 0;           --  pixels written
      Previous : RGBA;
      Index : Color_Index := [others => (0, 0, 0, 0)];
      Operation : Unsigned_8 := 0;
      Needed : Operand_Count := 0;
      Operands : Operand_Bytes := [others => 0];
      Have : Operand_Count := 0;
      --  PPM: the field being read, its value, whether inside a comment.
      Field : PPM_Field := 1;
      Value : Natural := 0;
      In_Number, In_Comment : Boolean := False;
      Channel : Natural range 0 .. 2 := 0;
      Pending : RGBA;
   end record;
end CCL.Image_Formats;
