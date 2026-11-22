------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A PNG encoder for screenshots (REMOTE-001): 8-bit RGB, no interlace, one
--  zlib stream of fixed-Huffman deflate blocks with LZ77 matches. Desktop
--  screens are mostly flat colour and repeated rows, so this small encoder
--  already shrinks them many times over; it is not a general compressor.
--
--  @description
--  Encode reads the source frame through Source (BGRA 8888 bytes, rows of
--  Width * 4, the display-capture layout) and hands the file's bytes, in
--  order, to Emit in pieces of at most Chunk_Bytes. Match candidates per
--  position: the previous occurrence of the next three bytes (a hash of
--  them), the pixel to the left and the pixel above; the longest wins.
--  Memory is fixed: the hash table and one IDAT chunk buffer.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Remote_Wire;

package Remote_PNG is
   subtype Byte_Array is Remote_Wire.Byte_Array;

   --  The largest frame side encoded (a PNG allows more; a screen does not).
   Maximum_Side : constant := 16_384;
   subtype Side is Positive range 1 .. Maximum_Side;
   --  The most bytes Emit is given at once (one IDAT chunk's payload).
   Chunk_Bytes : constant := 32_768;

   generic
      with function Source (Index : Natural) return Unsigned_8;
      with procedure Emit (Data : Byte_Array);
   procedure Encode (Width, Height : Side);

   --  CRC-32 (PNG chunks) and Adler-32 (zlib), exposed for tests.
   function CRC32 (Data : Byte_Array; Seed : Unsigned_32 := 0) return Unsigned_32;
end Remote_PNG;
