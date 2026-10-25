pragma Ada_2022;
with Interfaces; use Interfaces;
with Firmware_Tables;
with Firmware_Tables.Catalog;
with Firmware_Tables.Snapshots;
with Firmware_Tables.Snapshots.Transfers;
with Firmware_Snapshot_ABI;

--  A snapshot built in the test and a simulated caller memory, for the
--  hosted instance of Firmware_Snapshot_Service.
package Test_Snapshot is
   package F renames Firmware_Tables;
   package S renames F.Snapshots;
   package ABI renames Firmware_Snapshot_ABI;

   --  Above the boot prototype's 64 KiB default per-table limit: the
   --  snapshot is sized from its inventory, the export has no table limit.
   Large_Length : constant Positive := 65_573;
   Large : constant F.Catalog.Descriptor := (16#1234#, Large_Length, "DSDT", 2);
   Small : constant F.Catalog.Descriptor := (16#2345#, 116, "FACP", 6);
   Tiny  : constant F.Catalog.Descriptor := (16#3456#, F.Table_Header_Size, "SSDT", 1);
   Table_Total : constant := 3;
   Byte_Total : constant Positive := Large_Length + 116 + F.Table_Header_Size;

   function Raw (D : F.Catalog.Descriptor) return F.Bytes;

   State : S.State (Table_Total, Byte_Total, Large_Length);
   --  Building: two of the three tables appended, nothing observable.
   procedure Begin_Partial;
   --  The third table, then sealed (Ready).
   procedure Finish;

   function Ready return Boolean;
   function Count return Natural;
   function Describe (Index : Natural) return S.Metadata_Result;
   procedure Copy_Part
     (Index, Offset : Natural; Destination : out F.Bytes;
      Result : out S.Transfers.Transfer_Result);

   --  The caller's memory: User_Base .. User_Base + User_Bytes - 1 is
   --  writable; Write_Limit bytes may be written before the next write
   --  refuses (a fault after a prefix).
   User_Base  : constant Unsigned_64 := 16#10_0000#;
   User_Bytes : constant := 8_192;
   User : F.Bytes (1 .. User_Bytes) := [others => 0];
   Write_Limit : Natural := Natural'Last;
   Writes : Natural := 0;
   procedure Write_User (Destination : Unsigned_64; Source : F.Bytes; Success : out Boolean);

   Last_Reply : ABI.Reply := ABI.Empty_Reply;
   Replies : Natural := 0;
   Reply_Refused : Boolean := False;
   procedure Write_Reply (Item : ABI.Reply; Success : out Boolean);
end Test_Snapshot;
