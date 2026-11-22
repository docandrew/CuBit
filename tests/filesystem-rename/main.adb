with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Directory_Blocks; use Directory_Blocks;
with Directory_Commit;

procedure Main is
   Original, Data, Medium : Block_Data := [others => 0];
   Status : Prepare_Result;
   Calls : Natural := 0;
   type Fault_Mode is (Healthy, First_Unchanged, First_Partial, First_All, Both);
   Fault : Fault_Mode := Healthy;

   procedure Write_Block
     (Data : Block_Data; Size : Block_Length; Success : out Boolean)
   is
   begin
      Calls := Calls + 1;
      Success := Fault = Healthy or else (Calls = 2 and then Fault /= Both);
      if Calls = 1 and then Fault = First_Unchanged then
         null; -- Failure before the device changed any bytes.
      elsif Success or else (Calls = 1 and then Fault = First_All) then
         Medium (1 .. Size) := Data (1 .. Size);
      elsif Fault in First_Partial | Both then
         Medium (1 .. Size / 2) := Data (1 .. Size / 2);
      end if;
   end Write_Block;
   package Committer is new Directory_Commit (Write_Block);
   Result : Committer.Commit_Result;
   use type Committer.Commit_Result;

   procedure Put_Record
     (Offset, Span : Natural; Name : String; Inode : Unsigned_8 := 17)
   is
   begin
      Original (Offset + 1) := Inode;
      Original (Offset + 5) := Unsigned_8 (Span mod 256);
      Original (Offset + 6) := Unsigned_8 (Span / 256);
      Original (Offset + 7) := Name'Length;
      Original (Offset + 8) := 1;
      for Index in 1 .. Name'Length loop
         Original (Offset + 8 + Index) :=
           Character'Pos (Name (Name'First + (Index - 1)));
      end loop;
   end Put_Record;

   procedure Reject (Name : String; Expected : Prepare_Result) is
   begin
      Data := Original;
      Prepare_Rename (Data, 64, 100, "old", Name, Status);
      pragma Assert (Status = Expected and then Data = Original);
   end Reject;
begin
   Put_Record (0, 16, "old");
   Put_Record (16, 48, "sibling", 18);
   Reject ("old", Unchanged);
   Reject ("sibling", Destination_Exists);
   Reject ("..", Invalid_Name);
   Reject ("a/b", Invalid_Name);
   Reject ("", Invalid_Name);
   Reject ("a" & ASCII.NUL, Invalid_Name);
   Reject ([1 .. 255 => 'x'], Insufficient_Space);
   Data := Original;
   Prepare_Rename (Data, 64, 100, "old", "a-longer-name", Status);
   pragma Assert (Status = Prepared and then Data /= Original);
   --  Inode/type preserved; sibling follows the new aligned record at byte 24.
   pragma Assert (Data (1) = 17 and then Data (8) = 1 and then Data (7) = 13);
   pragma Assert (Data (25) = 18 and then Data (31) = 7);
   for Mode in Fault_Mode loop
      Fault := Mode;
      Calls := 0;
      Medium := Original;
      Committer.Commit (Original, Data, 64, Result);
      if Mode = Healthy then
         pragma Assert (Result = Committer.Committed and then Calls = 1);
         pragma Assert (Medium = Data);
      elsif Mode = Both then
         pragma Assert (Result = Committer.Recovery_Required and then Calls = 2);
      else
         pragma Assert (Result = Committer.Original_Restored and then Calls = 2);
         pragma Assert (Medium = Original);
      end if;
   end loop;
   Data := Original;
   Prepare_Rename (Data, 64, 100, "missing", "new", Status);
   pragma Assert (Status = Source_Not_Found and then Data = Original);
   --  Corrupt the source header and require byte-for-byte rejection.
   for Span in 0 .. 7 loop
      Original (5) := Unsigned_8 (Span);
      Reject ("new", Malformed_Block);
   end loop;
   Original (5) := 65;
   Reject ("new", Malformed_Block);
   Original (5) := 16;
   Original (7) := 9;
   Reject ("new", Malformed_Block);
   Original (7) := 3;
   Original (1) := 101;
   Reject ("new", Malformed_Block);
   Original (1) := 17;
   --  A malformed later record must not permit an early source rename.
   Original (21) := 0;
   Reject ("new", Malformed_Block);

   --  Real ext2 block sizes, maximum names, free records, and non-1-based
   --  caller strings. The unused buffer tail must remain untouched.
   for Scale in 0 .. 2 loop
      declare
         Size : constant Block_Length := 1024 * 2 ** Scale;
         Long_Name : constant String (11 .. 265) := [others => 'x'];
      begin
         Original := [others => 16#A5#];
         Original (1 .. Size) := [others => 0];
         Put_Record (0, 12, "", 0);
         Put_Record (12, 16, "old");
         Put_Record (28, Size - 28, "sibling", 18);
         Data := Original;
         Prepare_Rename (Data, Size, 100, "old", Long_Name, Status);
         pragma Assert (Status = Prepared);
         pragma Assert (Data (1) = 17 and Data (7) = 255 and Data (8) = 1);
         pragma Assert (Data (265) = 18 and Data (271) = 7);
         pragma Assert (Data (Size + 1 .. Maximum_Bytes) =
                          Original (Size + 1 .. Maximum_Bytes));
         Prepare_Rename (Data, Size, 100, Long_Name, "old", Status);
         pragma Assert (Status = Prepared);
         pragma Assert (Data (1) = 17 and Data (7) = 3);
         pragma Assert (Data (13) = 18 and Data (19) = 7);
         --  The sibling is still locatable by the same name after compaction.
         Prepare_Rename (Data, Size, 100, "sibling", "old", Status);
         pragma Assert (Status = Destination_Exists);
      end;
   end loop;
   Original := [others => 0];
   Put_Record (0, 16, "old");
   Put_Record (16, 48, "old", 18);
   Reject ("new", Malformed_Block);
   Put_Line ("directory rename: preparation and injected commit failures PASS");
end Main;
