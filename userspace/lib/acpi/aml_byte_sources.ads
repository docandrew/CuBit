pragma Ada_2022;
with Firmware_Tables;
-- Owned immutable-reader storage. This carries RAM bytes, never hardware authority.
package AML_Byte_Sources with SPARK_Mode, Pure is
   pragma Elaborate_Body;
   type Storage (Length : Natural) is tagged record
      Bytes : Firmware_Tables.Bytes (1 .. Length) := [others => 0];
   end record;
   function Source_Length (Data : aliased Storage) return Natural;
   function Source_Byte (Data : aliased Storage; Index : Natural)
      return Firmware_Tables.Byte with Pre'Class => Index < Data.Length;
   function Fits_Window (Data : Storage; First, Count : Natural) return Boolean is
     (First <= Data.Length and then Count <= Data.Length - First);
   function Corresponds
     (Data : Storage; First : Natural; Code : Firmware_Tables.Bytes) return Boolean is
     (Fits_Window (Data, First, Code'Length) and then
       (for all J in Code'Range => Natural (Code (J)) =
          Natural (Data.Bytes (First + 1 + (J - Code'First)))));
end AML_Byte_Sources;
