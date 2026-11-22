pragma Ada_2022;
with Interfaces; use Interfaces;

--  A program's manifest, as bytes to compare (CuBit.Service_Replacement's
--  Content_Admits): every SHT_PROGBITS section of the ELF image whose name
--  starts with ".cubit." (identity, requests, startup table, scopes, launch
--  table, description, parameters, resources: everything ccl-manifest
--  writes), in section order, each as its name, a zero byte, its size (8
--  bytes, little-endian) and its contents. Two programs with equal
--  fingerprints declare the same manifest, byte for byte. No I/O.
package Manifest_Fingerprints with SPARK_Mode is
   type Bytes is array (Positive range <>) of Unsigned_8;

   --  Named ceiling: a manifest's sections, together (CAPACITY beyond).
   Maximum_Fingerprint_Bytes : constant := 65_536;
   subtype Fingerprint_Length is Natural range 0 .. Maximum_Fingerprint_Bytes;
   Section_Prefix : constant String := ".cubit.";

   type Reading is
     (Read,              --  the fingerprint is Into (1 .. Length)
      Not_ELF64,         --  too short, or not a 64-bit section header table
      Headers_Outside,   --  the section headers or names leave the image
      Section_Outside,   --  a .cubit. section leaves the image
      Too_Large);        --  the sections exceed Maximum_Fingerprint_Bytes

   procedure Fingerprint
     (Image : Bytes; Into : out Bytes; Length : out Fingerprint_Length; Result : out Reading)
   with Pre  => Image'First = 1 and then Image'Last < Natural'Last / 2
                and then Into'First = 1 and then Into'Length = Maximum_Fingerprint_Bytes,
        Post => (if Result = Read then Length <= Into'Length else Length = 0);

   function Same (Left, Right : Bytes) return Boolean is
     (Left'Length = Right'Length
      and then (for all I in 0 .. Left'Length - 1 => Left (Left'First + I) = Right (Right'First + I)));
end Manifest_Fingerprints;
