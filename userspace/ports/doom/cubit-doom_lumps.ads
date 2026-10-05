------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  DOOM sound effect lumps (DMX format), read from the WAD, which is
--  untrusted input (docs/c-removal.md).
--
--  Layout: a 16-bit format tag (3), a 16-bit sample rate and a 32-bit
--  sample count, all little-endian, then unsigned 8-bit PCM. DMX skips 16
--  padding samples at each end when the sound is long enough.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Doom_Lumps with SPARK_Mode, Pure is

   Header_Bytes    : constant := 8;
   Format_Tag      : constant := 3;
   Padding_Samples : constant := 16;
   --  Sounds with fewer samples are refused.
   Minimum_Samples : constant := 8;

   type Header is array (0 .. Header_Bytes - 1) of Unsigned_8;

   --  W_LumpLength's result; negative lengths are refused before this.
   subtype Lump_Length is Unsigned_32 range 0 .. 2 ** 31 - 1;
   subtype Sample_Rate is Unsigned_32 range 1 .. 2 ** 16 - 1;

   type Sound (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Rate  : Sample_Rate;
            --  Byte offset of the first sample and the sample count, both
            --  within the lump.
            First : Lump_Length;
            Count : Lump_Length;
         when False => null;
      end case;
   end record;

   function Parse (Head : Header; Length : Lump_Length) return Sound
     with Pre  => Length >= Header_Bytes,
          Post => (if Parse'Result.Valid then
                     Parse'Result.First >= Header_Bytes and then
                     Parse'Result.Count >= 1 and then
                     Parse'Result.Count <= Length - Parse'Result.First);

end CuBit.Doom_Lumps;
