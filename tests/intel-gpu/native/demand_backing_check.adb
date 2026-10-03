with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_Extent_Allocator;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Metadata_Platform;
with Intel_GPU_Record_Growth;

-- Disposable-VM privileged supervisor, installed INSTEAD OF devmgr for this
-- test only. Real kernel allocations and CPU mappings; no GPU and no allocation
-- IPC exchange. Never grant an ordinary application process authority to run it.
procedure Demand_Backing_Check is
   package V renames Intel_GPU_Buffer_Reply;
   PID : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   Calls : Natural := 0;
   Ready : Boolean := True;
   function Owner return Boolean is (Ready);
   function Allocate (CPU : Unsigned_64) return Unsigned_64 is
   begin
      Calls := Calls + 1;
      return syscall (SYSCALL_ALLOC_DMA, PID, 9, CPU, 3, 2 ** 32);
   end Allocate;
   package A is new Intel_GPU_Extent_Allocator (Owner, Allocate);
   Pool : A.Pool;
   function Capacity return Positive is (A.Record_Capacity (Pool));
   procedure Publish (Base, Bytes : Unsigned_64; OK : out Boolean) is
   begin A.Extend_Records (Pool, Base, Bytes, OK); end Publish;
   package G is new Intel_GPU_Record_Growth
     (Intel_GPU_Metadata_Platform.Storage, Capacity, Publish);
   use type G.Phase;
   Growth : G.Controller;
   Views : array (1 .. 17) of V.Extent_View;
   OK, Pending : Boolean;
   Ignore : Unsigned_64;

   procedure Check (Condition : Boolean; Detail : String) is
   begin
      if not Condition then
         debugPrint ("TEST: FAIL native demand backing: " & Detail & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT);
         loop null; end loop;
      end if;
   end Check;
   function Sentinel (Index, Page : Natural) return Unsigned_64 is
     (16#D00D_0000_0000_0000# + Unsigned_64 (Index) * 2 ** 32 + Unsigned_64 (Page));
   procedure Check_Pixels (Write_First : Boolean) is
   begin
      for Index in Views'Range loop
         for Page in 0 .. Natural (V.Byte_Count (Views (Index)) / 4096) - 1 loop
            declare
               Word : Unsigned_64 with Import, Volatile,
                 Address => To_Address (Integer_Address
                   (V.CPU_Address (Views (Index)) + Unsigned_64 (Page) * 4096));
            begin
               if Write_First then Word := Sentinel (Index, Page);
               else Check (Word = Sentinel (Index, Page), "CPU alias/content"); end if;
            end;
         end loop;
      end loop;
   end Check_Pixels;
begin
   debugPrint ("native demand backing: privileged disposable fixture (NO GPU/IPC)" & ASCII.LF);
   Check (PID /= 0 and PID /= Unsigned_64'Last, "PID");
   G.Configure (Growth, 65536, 1000, OK);
   Check (OK, "metadata configure");
   for Index in Views'Range loop
      G.Request (Growth, Index, OK);
      Check (OK, "metadata request");
      for Turn in 1 .. 8 loop
         G.Step (Growth);
         exit when G.Snapshot (Growth).State in G.Idle | G.Failed;
      end loop;
      Check (G.Snapshot (Growth).State = G.Idle and Capacity >= Index,
        "metadata growth");
      for Turn in 1 .. 8 loop
         declare Before : constant Natural := Calls; begin
            A.Step_Buffer (Pool, 7, Index, (if Index = 1 then 4096 else 1), 1,
              Views (Index), OK, Pending);
            Check (Calls <= Before + 1, "unbounded physical step");
            if Pending then
               Check (not OK and not V.Valid (Views (Index)), "early publication");
            end if;
         end;
         exit when not Pending;
      end loop;
      Check (OK and not Pending and V.Valid (Views (Index)), "allocation");
      Check (Calls = (if Index = 1 then 8 else 9), "demand high-water");
   end loop;
   Check (A.Memory_Budget (Pool).Committed = 18 * 1024 * 1024 and
     A.Memory_Budget (Pool).Retained = 16 * 1024 * 1024 + 16 * 4096,
     "budget separation");
   -- Separate complete write/read passes expose accidental physical aliases.
   Check_Pixels (True);
   Check_Pixels (False);
   debugPrint ("native demand backing: 4112 page sentinels and metadata extension PASS" & ASCII.LF);
   declare
      Old_CPU : constant Unsigned_64 := V.CPU_Address (Views (9));
      Old_DMA : constant Unsigned_64 := V.Page_Address (Views (9), 0);
   begin
      -- Nothing in this fixture publishes a GPU PTE or a CPU grant. Thus no
      -- external references exist; retirement is not simulated GPU completion.
      A.Retire_Buffer (Pool, 7, 9, 1, True, OK);
      Check (OK, "retirement");
      A.Step_Buffer (Pool, 7, 9, 1, 2, Views (9), OK, Pending);
      Check (OK and not Pending and Calls = 9 and
        V.CPU_Address (Views (9)) = Old_CPU and
        V.Page_Address (Views (9), 0) = Old_DMA, "slice reuse");
      A.Retire_Buffer (Pool, 7, 9, 1, True, OK);
      Check (not OK, "stale retirement");
   end;
   Check_Pixels (False);
   Ready := False;
   A.Step_Buffer (Pool, 7, 18, 1, 1, Views (1), OK, Pending);
   Check (not OK and not Pending and Calls = 9, "owner loss");
   debugPrint ("TEST: PASS native demand backing 18MiB 17 objects (NO GPU/IPC)" & ASCII.LF);
   -- Retained mappings intentionally survive until the disposable VM ends.
   loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
end Demand_Backing_Check;
