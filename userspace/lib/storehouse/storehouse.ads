with Interfaces;

--  The portable core of CCL build and the Storehouse (docs/ccl-build.md):
--  content identities, their canonical encoding and the path rules. It does
--  no input or output, so the same units run hosted on Linux and on CuBit.
package Storehouse with SPARK_Mode, Pure is

   subtype Byte is Interfaces.Unsigned_8;
   type Byte_Array is array (Positive range <>) of Byte;

   --  A SHA-256 identity: of a file's bytes, of a tree's canonical listing,
   --  or of a step's canonical encoding.
   Digest_Bytes : constant := 32;
   subtype Digest_Index is Positive range 1 .. Digest_Bytes;
   type Digest is array (Digest_Index) of Byte;

   --  What a Storehouse entry is. A file's identity is the SHA-256 of its
   --  bytes whatever its kind; the kind selects the entry's namespace, so a
   --  file and an executable with the same bytes are two entries.
   type Entry_Kind is (Regular_File, Executable_File, Tree);

end Storehouse;
