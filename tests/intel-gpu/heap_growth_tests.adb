with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Extent_Allocator;
with Intel_GPU_Extent_Directory;
with Intel_GPU_Physical_Extents;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Record_Growth;
procedure Heap_Growth_Tests is
   package D renames Intel_GPU_Extent_Directory;
   package E renames Intel_GPU_Physical_Extents;
   CPU : constant Unsigned_64 := 16#7000_0000_0000#;
   DMA : constant Unsigned_64 := 2 ** 40;
   Ready : Boolean := True;
   Calls, Reserves, Commits, Clears : Natural := 0;
   function Owner_Ready return Boolean is (Ready);
   function Allocate (Address : Unsigned_64) return Unsigned_64 is
   begin
      pragma Assert (Address = CPU + Unsigned_64 (Calls) * E.Block_Bytes);
      Calls := Calls + 1;
      return DMA + Unsigned_64 (Calls - 1) * 2 * E.Block_Bytes;
   end Allocate;
   package A is new Intel_GPU_Extent_Allocator (Owner_Ready, Allocate);
   Pool : A.Pool;
   type RAM is array (Natural range 0 .. 65535) of Unsigned_8 with Alignment => 4096;
   Metadata : RAM := [others => 16#A5#];
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Metadata'Address));
   function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
   begin
      pragma Assert (Bytes = 65536);
      Reserves := Reserves + 1;
      return Base;
   end Reserve;
   function Commit (Address, Offset, Bytes : Unsigned_64) return Boolean is
   begin
      pragma Assert (Address = Base and Offset = 0 and Bytes = 65536);
      Commits := Commits + 1;
      return True;
   end Commit;
   function Clear (Address, Bytes : Unsigned_64) return Boolean is
   begin
      pragma Assert (Address = Base and Bytes = 65536);
      Metadata := [others => 0];
      Clears := Clears + 1;
      return True;
   end Clear;
   function Capacity return Positive is (A.Extent_Capacity (Pool));
   procedure Publish (Address, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      A.Extend_Extents (Pool, Address, Bytes, Accepted);
   end Publish;
   package Storage is new Intel_GPU_Metadata_Arena (Reserve, Commit, Clear);
   package Growth is new Intel_GPU_Record_Growth (Storage, Capacity, Publish);
   use type Growth.Phase;
   Controller : Growth.Controller;
   Prefix, Latest : D.Borrowed_View;
   OK : Boolean;
begin
   pragma Assert (A.DMA_Ceiling (Pool) = 2 ** 32);
   for Invalid in 0 .. 3 loop
      A.Configure_Heap (Pool,
        (case Invalid is when 0 => 0, when 1 => E.Block_Bytes + 1,
         when 2 => Unsigned_64'Last, when others => 24 * 1024 ** 3),
        (if Invalid = 3 then E.Block_Bytes else 2 ** 48), OK);
      pragma Assert (not OK and Calls = 0);
      pragma Assert (A.DMA_Ceiling (Pool) = 2 ** 32);
   end loop;
   A.Configure_Heap (Pool, 24 * 1024 ** 3, 2 ** 48, OK);
   pragma Assert (OK and Calls = 0 and not A.Memory_Budget (Pool).Known);
   pragma Assert (A.DMA_Ceiling (Pool) = 2 ** 48);
   A.Configure_Heap (Pool, E.Capacity, 2 ** 32, OK); pragma Assert (not OK);
   pragma Assert (A.DMA_Ceiling (Pool) = 2 ** 48);
   A.Acquire (Pool, CPU, Prefix, OK, 16 * E.Block_Bytes);
   pragma Assert (OK and Calls = 16 and D.Byte_Count (Prefix) = 16 * E.Block_Bytes);
   A.Acquire (Pool, CPU, Latest, OK, 17 * E.Block_Bytes);
   pragma Assert (not OK and Calls = 16 and D.Valid (Prefix));
   pragma Assert (A.Memory_Budget (Pool).Known);
   Growth.Configure (Controller, 65536, 12288, OK); pragma Assert (OK);
   Growth.Request (Controller, 600, OK); pragma Assert (OK);
   for Turn in 1 .. 4 loop
      Growth.Step (Controller);
      pragma Assert (Calls = 16 and D.Valid (Prefix));
      if Turn < 4 then pragma Assert (Capacity = 16); end if;
   end loop;
   pragma Assert (Growth.Snapshot (Controller).State = Growth.Idle);
   pragma Assert (Reserves = 1 and Commits = 1 and Clears = 1 and Capacity = 4112);
   A.Acquire (Pool, CPU, Latest, OK, 600 * E.Block_Bytes);
   pragma Assert (OK and Calls = 600 and D.Byte_Count (Latest) = 600 * E.Block_Bytes);
   pragma Assert (A.Memory_Budget (Pool).Capacity = 24 * 1024 ** 3);
   pragma Assert (A.Memory_Budget (Pool).Committed = 600 * E.Block_Bytes);
   pragma Assert (D.Same_Owner (Prefix, Latest));
   pragma Assert (not D.Resolve (Prefix, 16 * E.Block_Bytes, 1).Valid);
   for Index in 0 .. 599 loop
      pragma Assert (D.Resolve (Latest, Unsigned_64 (Index) * E.Block_Bytes, 4096).Address =
        DMA + Unsigned_64 (Index) * 2 * E.Block_Bytes);
   end loop;
   -- Metadata quota exhaustion cannot cause extra physical callbacks or
   -- invalidate already published backing; there is no larger reservation.
   Growth.Request (Controller, 9000, OK); pragma Assert (not OK);
   Growth.Step (Controller);
   pragma Assert (Growth.Snapshot (Controller).State = Growth.Idle);
   pragma Assert (Calls = 600 and Reserves = 1 and D.Valid (Latest));
   Growth.Request (Controller, 600, OK); pragma Assert (OK);
   pragma Assert (Growth.Snapshot (Controller).State = Growth.Idle);
   Ready := False;
   A.Extend_Extents (Pool, Base, 65536, OK); pragma Assert (not OK);
   A.Acquire (Pool, CPU, Latest, OK, E.Block_Bytes);
   pragma Assert (not OK and not D.Valid (Prefix) and Calls = 600);
   Ready := True;
   A.Acquire (Pool, CPU, Latest, OK, E.Block_Bytes);
   pragma Assert (not OK and Calls = 600);
   Ada.Text_IO.Put_Line ("Heap growth PASS: supervisor + bounded metadata controller, 600 synthetic extents, independent quotas, no eager backing, immutable prefixes, revocation");
end Heap_Growth_Tests;
