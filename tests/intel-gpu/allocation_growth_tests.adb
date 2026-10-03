with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Extent_Allocator;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Metadata_Initialize;
with Intel_GPU_Record_Growth;
with Intel_GPU_Allocation_Growth;
procedure Allocation_Growth_Tests is
   package V renames Intel_GPU_Buffer_Reply;
begin
   for Fault in 0 .. 7 loop
      declare
         type RAM is array (Natural range 0 .. 8191) of Unsigned_64;
         Memory : RAM := [others => 0] with Alignment => 4096;
         Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Memory'Address));
         Ready, Saved : Boolean := True;
         Saves, Replies, Acquires, Commits, Blocks : Natural := 0;
         Last_Success : Boolean := False;
         Expected_Index : Positive := 1;
         function Owner return Boolean is (Ready);
         function Allocate (CPU : Unsigned_64) return Unsigned_64 is
            pragma Unreferenced (CPU);
         begin
            Blocks := Blocks + 1;
            return 2 ** 32 - Unsigned_64 (2 * Blocks) * 2 ** 21;
         end Allocate;
         package A is new Intel_GPU_Extent_Allocator (Owner, Allocate);
         Pool : A.Pool;
         function Capacity return Positive is (A.Record_Capacity (Pool));
         procedure Publish (Address, Bytes : Unsigned_64; OK : out Boolean) is
         begin A.Extend_Records (Pool, Address, Bytes, OK); end Publish;
         function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
         begin pragma Assert (Bytes = 65536); return Base; end Reserve;
         function Commit (Address, Offset, Bytes : Unsigned_64) return Boolean is
         begin
            Commits := Commits + 1;
            pragma Assert (Address = Base and Offset = 0 and Bytes = 65536);
            return Fault /= 2;
         end Commit;
         package M is new Intel_GPU_Metadata_Arena
           (Reserve, Commit, Intel_GPU_Metadata_Initialize.Clear);
         package G is new Intel_GPU_Record_Growth (M, Capacity, Publish);
         function Save return Boolean is
         begin
            Saves := Saves + 1;
            pragma Assert (not Saved);
            Saved := Fault /= 1;
            return Saved;
         end Save;
         procedure Acquire
           (Index : Positive; Pages : V.Layout.Page_Count;
            Generation : Unsigned_32; Buffer : out V.Extent_View; Success, Pending : out Boolean) is
         begin
            Acquires := Acquires + 1;
            pragma Assert (Saved and Ready);
            A.Step_Buffer (Pool, 7, Index, Pages, Generation, Buffer, Success, Pending);
         end Acquire;
         procedure Respond
           (Index : Positive; Generation : Unsigned_32;
            Buffer : V.Extent_View; Success : Boolean) is
         begin
            pragma Assert (Saved and Index = Expected_Index and Generation = 1);
            pragma Assert (not Success or else V.Valid (Buffer));
            Saved := False;
            Replies := Replies + 1;
            Last_Success := Success;
            -- Fault5 models a lost reply: consume the saved cap anyway.
         end Respond;
         package D is new Intel_GPU_Allocation_Growth (G, Owner, Save, Acquire, Respond);
         Dispatcher : D.Dispatcher;
         OK : Boolean;
         Old_Count : Natural;
         Before_Blocks : Natural;
         Quota : constant Positive := (if Fault = 0 then 10000 else 2000);
      begin
         Saved := False;
         pragma Assert (D.Growth_Allowance (Dispatcher, 16) = 0);
         D.Configure (Dispatcher, 65536, Quota, OK); pragma Assert (OK);
         pragma Assert (D.Growth_Allowance (Dispatcher, 16) = Quota - 16);
         pragma Assert (D.Record_Budget (Dispatcher, 16, 0) = Quota - 16);
         pragma Assert (D.Record_Budget (Dispatcher, 16, 16) = Quota);
         pragma Assert (D.Record_Budget (Dispatcher, 16, 17) = 0);
         pragma Assert (D.Record_Budget (Dispatcher, Quota + 16, Quota) = Quota - 16);
         pragma Assert (D.Record_Budget (Dispatcher, Quota + 16, Quota + 16) = Quota);
         pragma Assert (D.Growth_Allowance (Dispatcher, Quota) = 0);
         Ready := False;
         pragma Assert (D.Growth_Allowance (Dispatcher, 16) = 0);
         Ready := True;
         D.Begin_Request (Dispatcher, Quota + 1, 1, 1, OK); pragma Assert (not OK and Saves = 0);
         D.Begin_Request (Dispatcher, 1, 1, 0, OK); pragma Assert (not OK and Saves = 0);
         Expected_Index := 17;
         D.Begin_Request (Dispatcher, 17, (if Fault >= 6 then 4096 else 1), 1, OK);
         if Fault = 1 then
            pragma Assert (not OK and not D.Pending (Dispatcher));
         else
            pragma Assert (OK and D.Pending (Dispatcher) and Replies = 0);
            D.Begin_Request (Dispatcher, 18, 2, 2, OK);
            pragma Assert (not OK and Saves = 1);
            for Turn in 1 .. 12 loop
               if (Fault = 3 and Turn = 1) or (Fault = 4 and Turn = 5) or
                 (Fault = 7 and Turn = 8) then Ready := False; end if;
               Before_Blocks := Blocks;
               D.Step (Dispatcher);
               pragma Assert (Blocks <= Before_Blocks + 1);
               if D.Pending (Dispatcher) then
                  pragma Assert (Saved and Replies = 0);
                  D.Begin_Request (Dispatcher, 18, 1, 1, OK);
                  pragma Assert (not OK and Saves = 1);
               end if;
               exit when not D.Pending (Dispatcher);
            end loop;
            pragma Assert (not D.Pending (Dispatcher) and Replies = 1 and not Saved);
            pragma Assert (Last_Success = (Fault in 0 | 5 | 6));
            pragma Assert (Acquires = (if Fault in 0 | 5 then 1
              elsif Fault = 6 then 8 elsif Fault = 7 then 3 else 0));
         end if;
         Old_Count := Acquires + Replies + Commits;
         for Turn in 1 .. 12 loop D.Step (Dispatcher); end loop;
         pragma Assert (Acquires + Replies + Commits = Old_Count);
         if Fault in 3 | 4 | 7 then
            Ready := True;
            D.Begin_Request (Dispatcher, 18, 1, 1, OK);
            pragma Assert (not OK and Saves = 1);
            pragma Assert (D.Growth_Allowance (Dispatcher, 16) = 0);
         end if;
         if Fault /= 1 then
            pragma Assert (D.Growth_Allowance (Dispatcher, Capacity) = 0);
         end if;
         if Fault = 0 then
            for Index in 18 .. 100 loop
               Expected_Index := Index;
               D.Begin_Request (Dispatcher, Index, 1, 1, OK); pragma Assert (OK);
               D.Step (Dispatcher);
               pragma Assert (not D.Pending (Dispatcher) and Last_Success);
            end loop;
            pragma Assert (Commits = 1 and Acquires = 84 and Replies = 84);
            pragma Assert (A.Memory_Budget (Pool).Retained = 84 * 4096);
            -- Metadata byte quota is full, but smaller requests can still use
            -- initialized records. The rejected request owns exactly one reply.
            pragma Assert (Capacity < Quota);
            Expected_Index := Capacity + 1;
            Before_Blocks := Blocks;
            D.Begin_Request (Dispatcher, Expected_Index, 1, 1, OK);
            pragma Assert (OK and D.Pending (Dispatcher) and Saved);
            D.Step (Dispatcher);
            pragma Assert (not D.Pending (Dispatcher) and not Saved and not Last_Success);
            pragma Assert (Replies = 85 and Saves = 85 and Acquires = 84);
            pragma Assert (Blocks = Before_Blocks and Commits = 1);
            for Turn in 1 .. 12 loop D.Step (Dispatcher); end loop;
            pragma Assert (Replies = 85 and Acquires = 84);
            pragma Assert (D.Record_Budget (Dispatcher, Capacity, Capacity - 84) = Capacity - 84);
            Expected_Index := 101;
            D.Begin_Request (Dispatcher, Expected_Index, 1, 1, OK);
            pragma Assert (OK);
            D.Step (Dispatcher);
            pragma Assert (not D.Pending (Dispatcher) and not Saved and Last_Success);
            pragma Assert (Replies = 86 and Saves = 86 and Acquires = 85);
            pragma Assert (Blocks = Before_Blocks and Commits = 1);
            pragma Assert (A.Memory_Budget (Pool).Retained = 85 * 4096);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Allocation growth PASS: real extent allocator past bootstrap, saved request, busy/quota/generation checks, owner loss, commit failure, no replay");
end Allocation_Growth_Tests;
