with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_VM_Image;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Record_Growth;
procedure Insertion_Metadata_Growth_Tests is
   package VM is new Intel_GPU_VM_Image (64, 4, 2);
   Receipt : VM.Insertion_Receipt;
   type Byte_Array is array (1 .. 262144) of Unsigned_8;
   Memory : Byte_Array := [others => 16#A5#] with Alignment => 4096;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Memory'Address));
   Reserved, Committed, Published : Unsigned_64 := 0;
   Reserves, Commits, Initializes, Publications : Natural := 0;
   Fail_Commit : Boolean := False;
   function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
   begin
      pragma Assert (Bytes = Memory'Length and Reserves = 0);
      Reserves := Reserves + 1; Reserved := Bytes; return Base;
   end Reserve;
   function Commit (Address, Offset, Bytes : Unsigned_64) return Boolean is
   begin
      pragma Assert (Address = Base and Offset = Committed and Bytes <= 65536);
      pragma Assert (Offset + Bytes <= Reserved);
      Commits := Commits + 1;
      if Fail_Commit then return False; end if;
      Committed := Offset + Bytes; return True;
   end Commit;
   function Initialize (Address, Bytes : Unsigned_64) return Boolean is
   begin
      pragma Assert (Address = Base + Published and Published + Bytes <= Committed);
      Initializes := Initializes + 1; Published := Published + Bytes; return True;
   end Initialize;
   package Arena is new Intel_GPU_Metadata_Arena (Reserve, Commit, Initialize);
   function Capacity return Positive is (VM.Insertion_Capacity (Receipt));
   procedure Publish (Address, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      pragma Assert (Address = Base and Bytes = Published);
      Publications := Publications + 1;
      VM.Extend_Insertion_Metadata (Receipt, Address, Bytes, Accepted);
   end Publish;
   package Growth is new Intel_GPU_Record_Growth (Arena, Capacity, Publish);
   use type Growth.Phase, Growth.Failure;
   Controller : Growth.Controller;
   OK : Boolean;
   Steps, Before, Calls : Natural;
   procedure Advance is
   begin
      Before := Capacity;
      Calls := Reserves + Commits + Initializes + Publications;
      Growth.Step (Controller);
      -- One arena phase per service turn; commit+initialize share one bounded span.
      pragma Assert (Reserves + Commits + Initializes + Publications - Calls <= 2);
      pragma Assert (Capacity >= Before);
   end Advance;
begin
   pragma Assert (Capacity = 2);
   Growth.Configure (Controller, 262144, 32768, OK); pragma Assert (OK);
   Growth.Request (Controller, 2, OK); pragma Assert (OK);
   Advance; pragma Assert (Reserves = 0 and Commits = 0 and Capacity = 2);
   Growth.Request (Controller, 3, OK); pragma Assert (OK);
   Growth.Request (Controller, 9000, OK); pragma Assert (not OK);
   Steps := 0;
   while Growth.Snapshot (Controller).State /= Growth.Idle loop
      Advance; Steps := Steps + 1; pragma Assert (Steps <= 4);
   end loop;
   pragma Assert (Steps = 4 and Capacity = 8194 and Publications = 1);
   pragma Assert (Reserves = 1 and Committed = 65536);
   for I in 65537 .. Memory'Last loop pragma Assert (Memory (I) = 16#A5#); end loop;
   -- A later request extends the same reservation without touching its old prefix.
   Memory (1) := 16#5A#;
   Growth.Request (Controller, 9000, OK); pragma Assert (OK);
   Steps := 0;
   while Growth.Snapshot (Controller).State /= Growth.Idle loop
      Advance; Steps := Steps + 1; pragma Assert (Steps <= 3);
   end loop;
   pragma Assert (Steps = 3 and Capacity = 16386 and Publications = 2 and Reserves = 1);
   pragma Assert (Memory (1) = 16#5A# and Committed = 131072);
   Growth.Request (Controller, 32769, OK); pragma Assert (not OK);
   pragma Assert (Growth.Snapshot (Controller).State = Growth.Idle);
   Fail_Commit := True;
   Growth.Request (Controller, 20000, OK); pragma Assert (OK);
   Advance; Advance;
   pragma Assert (Growth.Snapshot (Controller).State = Growth.Failed);
   pragma Assert (Growth.Snapshot (Controller).Error = Growth.Commit_Failed);
   pragma Assert (Capacity = 16386 and Memory (1) = 16#5A# and Publications = 2);
   Calls := Commits;
   Growth.Request (Controller, 20000, OK); pragma Assert (not OK);
   Growth.Step (Controller); pragma Assert (Commits = Calls);
   for I in 131073 .. Memory'Last loop pragma Assert (Memory (I) = 16#A5#); end loop;
   Ada.Text_IO.Put_Line ("Insertion metadata growth PASS: lazy reserve, bounded commit, stable prefix, quota and retained failure");
end Insertion_Metadata_Growth_Tests;
