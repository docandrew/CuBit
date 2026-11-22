------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Random words from the CPU (RDRAND), until the entropy service exists.
--  The one implementation: libc's getrandom and the installer's GUIDs and
--  filesystem UUIDs use it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Hardware_Random is

   --  RDRAND attempts per word before giving up (Intel recommends ten).
   Attempts : constant := 16;

   --  One 64-bit word. OK False: the CPU returned no value Attempts times.
   procedure Word (Value : out Unsigned_64; OK : out Boolean);

   type Byte_Array is array (Positive range <>) of Unsigned_8;

   --  Fills Data. OK False: a word failed; Data is then all zero.
   procedure Fill (Data : out Byte_Array; OK : out Boolean);

end CuBit.Hardware_Random;
