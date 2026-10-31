with Interfaces;
with Firmware_Snapshot_ABI;
-- Hosted model of the native source ABI. This grants no actual authority.
package CuBit.Firmware_Snapshot is
   type Authority is limited private;
   function Test_Authority return Authority;
   type Table_ID is new Interfaces.Unsigned_32;
   type Byte_Count is new Interfaces.Unsigned_64;
   Chunk_Max : constant := Firmware_Snapshot_ABI.Chunk_Max;
   type Outcome is
     (Ok, Not_Ready, Unknown_Table, Beyond_End, Empty_Before_End, Copy_Failed,
      Not_Authorized, Malformed);
   type Catalog_Info is record
      Version, Table_Count : Interfaces.Unsigned_32 := 0;
      Ready : Boolean := False;
      Largest_Table, Total_Bytes : Byte_Count := 0;
   end record;
   Signature_Length : constant := Firmware_Snapshot_ABI.Signature_Length;
   subtype Signature_Index is Positive range 1 .. Signature_Length;
   type Signature_Bytes is array (Signature_Index) of Interfaces.Unsigned_8;
   type Table_Description is record
      Signature : Signature_Bytes := [others => 0];
      Revision : Interfaces.Unsigned_8 := 0;
      Extent : Byte_Count := 0;
   end record;
   type Byte_Array is array (Positive range <>) of Interfaces.Unsigned_8;
   procedure Info (A : Authority; Result : out Catalog_Info; Status : out Outcome);
   procedure Describe
     (A : Authority; ID : Table_ID; Result : out Table_Description; Status : out Outcome);
   procedure Copy_Part
     (A : Authority; ID : Table_ID; Offset : Byte_Count; Into : out Byte_Array;
      Copied : out Byte_Count; Status : out Outcome);
private
   Expected_Tag : constant Interfaces.Unsigned_64 := 16#A100_0000_0000_0042#;
   type Authority is limited record Tag : Interfaces.Unsigned_64 := 0; end record;
end CuBit.Firmware_Snapshot;
