--  Bounded ECMA-119 metadata decoder. No packed overlays on untrusted bytes.
with Interfaces; use Interfaces;
package ISO_Records with SPARK_Mode => On is
   Block_Bytes : constant := 2048;
   type Sector is array (Natural range 0 .. Block_Bytes - 1) of Unsigned_8;
   type File_Record is record
      Extent : Unsigned_32 := 0;
      Bytes : Unsigned_32 := 0;
      Directory : Boolean := False;
      Name_Length : Natural range 0 .. 207 := 0;
      Name : String (1 .. 207) := [others => ' '];
   end record;
   function Header_Valid (Data : Sector) return Boolean;
   procedure Decode_Record
     (Data : Sector; Offset : Natural; Volume_Blocks : Unsigned_64;
      Item : out File_Record; Consumed : out Natural; Valid : out Boolean);
   procedure Decode_Volume
     (Data : Sector; Media_Blocks : Unsigned_64;
      Volume_Blocks : out Unsigned_64; Root : out File_Record;
      Valid : out Boolean);
   function Matches (Item : File_Record; Name : String) return Boolean;
end ISO_Records;
