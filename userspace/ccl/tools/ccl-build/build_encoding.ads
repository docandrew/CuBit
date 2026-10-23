with Ada.Containers.Vectors;
with Interfaces;
with Storehouse;

--  Building and reading canonical encodings (docs/ccl-build.md, "Canonical
--  encoding") from the proved heads of Storehouse.Canonical.
package Build_Encoding is
   use Storehouse;

   type Writer is tagged private;
   procedure Add_Unsigned (W : in out Writer; Value : Interfaces.Unsigned_64);
   procedure Add_Text (W : in out Writer; Text : String);
   procedure Add_Bytes (W : in out Writer; Data : Byte_Array);
   procedure Add_Digest (W : in out Writer; Value : Digest);
   procedure Add_Boolean (W : in out Writer; Value : Boolean);
   --  The head of an array of Count items; the items follow.
   procedure Add_Array (W : in out Writer; Count : Natural);
   function Bytes (W : Writer) return Byte_Array;
   function Identity (W : Writer) return Digest;

   --  Reading refuses anything but the canonical form of what is expected.
   Decode_Error : exception;
   type Reader (Length : Natural) is record
      Data : Byte_Array (1 .. Length);
      Position : Positive := 1;
   end record;
   function Read_Unsigned (R : in out Reader) return Interfaces.Unsigned_64;
   function Read_Text (R : in out Reader) return String;
   function Read_Digest (R : in out Reader) return Digest;
   function Read_Array (R : in out Reader) return Natural;
   function At_End (R : Reader) return Boolean is (R.Position = R.Length + 1);

private
   use type Storehouse.Byte;
   package Byte_Vectors is new Ada.Containers.Vectors (Positive, Byte);
   type Writer is tagged record
      Data : Byte_Vectors.Vector;
   end record;
end Build_Encoding;
