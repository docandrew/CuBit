------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A streaming decoder for the Quite OK Image format (QOI 1.0,
--  https://qoiformat.org/qoi-specification.pdf) into the desktop's 32-bit
--  pixels (16#AARRGGBB#, BGRA bytes in memory).
--
--  @description
--  Total and bounded: every byte sequence is either decoded or rejected,
--  with no overflow and no write outside the caller's buffer (proved,
--  docs/assets.md). Input arrives in chunks of any size, including single
--  bytes, so a reader can decode straight out of its transfer window.
--  Strict: the header's width and height must fit the caller's pixel limit,
--  a run may not pass the last pixel, and exactly the eight-byte end marker
--  must follow the last pixel. A stream that ends early is Truncated, never
--  a partial image.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package CuBit.QOI with SPARK_Mode, Pure is

   --  Wire format (specification section "header" and "end marker").
   Header_Bytes : constant := 14;
   Marker_Bytes : constant := 8;
   Magic_Q : constant := 16#71#;   --  "qoif"
   Magic_O : constant := 16#6F#;
   Magic_I : constant := 16#69#;
   Magic_F : constant := 16#66#;
   Channels_RGB      : constant := 3;
   Channels_RGBA     : constant := 4;
   Colorspace_SRGB   : constant := 0;
   Colorspace_Linear : constant := 1;
   Marker_Last       : constant := 16#01#;   --  seven 16#00# bytes, then this

   --  Chunk tags (two high bits) and the two eight-bit tags.
   Tag_Mask  : constant := 16#C0#;
   Op_Index  : constant := 16#00#;
   Op_Diff   : constant := 16#40#;
   Op_Luma   : constant := 16#80#;
   Op_Run    : constant := 16#C0#;
   Op_RGB    : constant := 16#FE#;
   Op_RGBA   : constant := 16#FF#;
   Payload_Mask : constant := 16#3F#;
   Index_Slots  : constant := 64;
   Run_Bias     : constant := 1;    --  a run's six bits store length - 1
   Diff_Bias    : constant := 2;
   Luma_Green_Bias : constant := 32;
   Luma_Delta_Bias : constant := 8;
   Opaque       : constant := 16#FF#;

   --  One side of an image (the format allows 32 bits; CuBit images stay
   --  within the GPU's largest texture side). Images are at most
   --  Maximum_Pixels; a caller's own limit is usually far smaller.
   Maximum_Side   : constant := 16_384;
   Maximum_Pixels : constant := 64 * 1024 * 1024;
   subtype Side is Positive range 1 .. Maximum_Side;
   subtype Pixel_Count is Natural range 0 .. Maximum_Pixels;
   subtype Pixel_Limit is Pixel_Count range 1 .. Maximum_Pixels;
   subtype Pixel_Index is Natural range 0 .. Maximum_Pixels - 1;

   subtype Pixel is Unsigned_32;   --  16#AARRGGBB#
   type Pixel_Buffer is array (Pixel_Index range <>) of Pixel;
   type Byte_Array is array (Positive range <>) of Unsigned_8;

   type Phase is (Reading_Header, Reading_Pixels, Reading_Marker, Complete, Failed);
   type Failure is
     (No_Failure,
      Bad_Magic,          --  not "qoif"
      Bad_Size,           --  a zero or over-Maximum_Side width or height
      Over_Limit,         --  width * height exceeds the caller's limit
      Bad_Channels,       --  neither 3 nor 4
      Bad_Colorspace,     --  neither 0 nor 1
      Run_Past_End,       --  a run longer than the pixels left
      Bad_Marker,         --  the bytes after the last pixel are not the marker
      Trailing_Data,      --  bytes after the marker
      Truncated);         --  the stream ended before the marker (Finish)

   type Decoder is private;

   function Limit (D : Decoder) return Pixel_Limit;
   function Current (D : Decoder) return Phase;
   function Error (D : Decoder) return Failure;
   --  The header's size, once it is read (Current past Reading_Header).
   function Width (D : Decoder) return Natural;
   function Height (D : Decoder) return Natural;
   --  Pixels the image declares (0 until the header is read), and pixels
   --  written to the output so far (always the prefix 0 .. Written - 1).
   function Total (D : Decoder) return Pixel_Count;
   function Written (D : Decoder) return Pixel_Count;
   function Valid (D : Decoder) return Boolean;

   --  A fresh decoder for an image of at most Capacity pixels.
   procedure Start (D : out Decoder; Capacity : Pixel_Limit)
     with Post => Valid (D) and then Current (D) = Reading_Header and then
                  Limit (D) = Capacity and then Written (D) = 0;

   --  Consume every byte of Input, writing decoded pixels to Output from
   --  index Written (D) on. Once Failed, further input changes nothing.
   procedure Feed (D : in out Decoder; Input : Byte_Array; Output : in out Pixel_Buffer)
     with Pre  => Valid (D) and then Output'First = 0 and then
                  Output'Length >= Limit (D),
          Post => Valid (D) and then Limit (D) = Limit (D'Old) and then
                  Written (D) >= Written (D'Old) and then
                  Written (D) <= Total (D) and then Total (D) <= Limit (D) and then
                  (if Current (D'Old) = Failed then Current (D) = Failed) and then
                  (if Current (D) = Complete then Written (D) = Total (D)) and then
                  (if Current (D) = Failed then Error (D) /= No_Failure);

   --  The stream has ended: a decoder short of the marker becomes Failed
   --  (Truncated). Complete means exactly Total (D) pixels were written.
   procedure Finish (D : in out Decoder)
     with Pre  => Valid (D),
          Post => Valid (D) and then Limit (D) = Limit (D'Old) and then
                  Written (D) = Written (D'Old) and then
                  Current (D) in Complete | Failed and then
                  (if Current (D) = Complete then
                     Written (D) = Total (D) and then Total (D) > 0
                   else Error (D) /= No_Failure);

private
   type Color is record
      R, G, B, A : Unsigned_8 := 0;
   end record;
   subtype Slot is Natural range 0 .. Index_Slots - 1;
   type Color_Index is array (Slot) of Color;
   subtype Header_Fill is Natural range 0 .. Header_Bytes;
   subtype Header_Data is Byte_Array (1 .. Header_Bytes);
   subtype Marker_Fill is Natural range 0 .. Marker_Bytes;
   --  The longest chunk is Op_RGBA: its tag and four bytes.
   Longest_Chunk : constant := 4;
   subtype Chunk_Fill is Natural range 0 .. Longest_Chunk;
   subtype Chunk_Data is Byte_Array (1 .. Longest_Chunk);

   Start_Color : constant Color := (R => 0, G => 0, B => 0, A => Opaque);

   type Decoder is record
      Capacity  : Pixel_Limit := 1;
      State     : Phase := Reading_Header;
      Problem   : Failure := No_Failure;
      Header    : Header_Data := [others => 0];
      Have      : Header_Fill := 0;
      Columns   : Natural := 0;
      Rows      : Natural := 0;
      Pixels    : Pixel_Count := 0;
      Done      : Pixel_Count := 0;
      Previous  : Color := Start_Color;
      Seen      : Color_Index := [others => (others => 0)];
      --  A multi-byte chunk in progress: its tag, the bytes still owed
      --  and those received.
      Tag       : Unsigned_8 := 0;
      Owed      : Chunk_Fill := 0;
      Got       : Chunk_Fill := 0;
      Chunk     : Chunk_Data := [others => 0];
      Ending    : Marker_Fill := 0;
   end record;

   function Limit (D : Decoder) return Pixel_Limit is (D.Capacity);
   function Current (D : Decoder) return Phase is (D.State);
   function Error (D : Decoder) return Failure is (D.Problem);
   function Width (D : Decoder) return Natural is (D.Columns);
   function Height (D : Decoder) return Natural is (D.Rows);
   function Total (D : Decoder) return Pixel_Count is (D.Pixels);
   function Written (D : Decoder) return Pixel_Count is (D.Done);

   --  A multi-byte chunk in progress still owes at least one byte.
   function Chunk_Valid (D : Decoder) return Boolean is
     (if D.Owed > 0 then D.Got < D.Owed else D.Got = 0);

   function Valid (D : Decoder) return Boolean is
     (D.Done <= D.Pixels and then D.Pixels <= D.Capacity and then
      Chunk_Valid (D) and then
      (D.State = Failed) = (D.Problem /= No_Failure) and then
      (case D.State is
         when Reading_Header =>
           D.Have < Header_Bytes and then D.Pixels = 0 and then D.Done = 0 and then
           D.Owed = 0 and then D.Ending = 0,
         when Reading_Pixels => D.Pixels > 0 and then D.Done < D.Pixels and then D.Ending = 0,
         when Reading_Marker =>
           D.Pixels > 0 and then D.Done = D.Pixels and then D.Owed = 0 and then
           D.Ending < Marker_Bytes,
         when Complete =>
           D.Pixels > 0 and then D.Done = D.Pixels and then D.Owed = 0,
         when Failed => True));
end CuBit.QOI;
