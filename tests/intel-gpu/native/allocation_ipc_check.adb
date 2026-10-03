with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Memory;
with Intel_GPU_Extent_Allocator;
with Intel_GPU_Physical_Extents;
with Intel_GPU_Extent_Directory;
with Intel_GPU_Metadata_Platform;
with Intel_GPU_Record_Growth;
with Intel_GPU_Allocation_Growth;
with Intel_GPU_Extent_Growth;

-- Real kernel async IPC in one privileged disposable process. This tests
-- saved reply capabilities and production client/allocator state machines,
-- NOT cross-process isolation, devmgr admission policy, or a GPU.
procedure Allocation_IPC_Check is
   package L renames Intel_GPU_Buffer_Backing;
   package V renames Intel_GPU_Buffer_Reply;
   package E renames Intel_GPU_Physical_Extents;
   PID : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   Calls, Saves, Responses, Extents, Pings : Natural := 0;
   Ping_Sent : Boolean := False;
   Ignore : Unsigned_64;
   function Owner return Boolean is (True);
   procedure Check (Condition : Boolean; Detail : String) is
   begin
      if not Condition then
         debugPrint ("TEST: FAIL native allocation IPC: " & Detail & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT);
         loop null; end loop;
      end if;
   end Check;
   function Allocate (CPU : Unsigned_64) return Unsigned_64 is
   begin
      Calls := Calls + 1;
      return syscall (SYSCALL_ALLOC_DMA, PID, 9, CPU, 3, 2 ** 32);
   end Allocate;
   package A is new Intel_GPU_Extent_Allocator (Owner, Allocate);
   Pool : A.Pool;
   package Extent_Growth is new Intel_GPU_Extent_Growth
     (A, Pool, Intel_GPU_Metadata_Platform.Storage, Owner);
   function Capacity return Positive is (A.Record_Capacity (Pool));
   procedure Publish (Base, Bytes : Unsigned_64; OK : out Boolean) is
   begin A.Extend_Records (Pool, Base, Bytes, OK); end Publish;
   package G is new Intel_GPU_Record_Growth
     (Intel_GPU_Metadata_Platform.Storage, Capacity, Publish);
   function Save return Boolean is
   begin Saves := Saves + 1; return saveReplyCap (58) = 1; end Save;
   procedure Acquire
     (Index : Positive; Pages : L.Page_Count; Generation : Unsigned_32;
      Buffer : out V.Extent_View; Success, Pending : out Boolean) is
   begin Extent_Growth.Step (PID, Index, Pages, Generation, Buffer, Success, Pending); end Acquire;
   procedure Respond
     (Index : Positive; Generation : Unsigned_32;
      Buffer : V.Extent_View; Success : Boolean) is
      Msg : Message := NULL_MESSAGE;
   begin
      Check (Success, "supervisor allocation");
      Msg.tag := (16#F004#, 4, 0, 0);
      Msg.words := [V.CPU_Address (Buffer), V.Byte_Count (Buffer), PID,
        L.Allocation_Key (Index, Generation)];
      Check (replyCap (58, Msg) = 1, "saved reply delivery");
      Check (replyCap (58, Msg) /= 1, "saved reply consumed twice");
      Responses := Responses + 1;
   end Respond;
   package D is new Intel_GPU_Allocation_Growth (G, Owner, Save, Acquire, Respond);
   Dispatcher : D.Dispatcher;
   package Client is new Intel_GPU_Buffer_Memory (Owner, Intel_GPU_Metadata_Platform.Storage);
   Driver : Client.Pool;
   use type Client.Allocation_Stage;
   Driver_Metadata_Observed : Boolean := False;
   function Client_Capacity return Positive is (Client.Record_Capacity (Driver));
   procedure Client_Publish (Base, Bytes : Unsigned_64; OK : out Boolean) is
   begin Client.Extend_Records (Driver, Base, Bytes, OK); end Client_Publish;
   package CG is new Intel_GPU_Record_Growth
     (Intel_GPU_Metadata_Platform.Storage, Client_Capacity, Client_Publish);
   use type CG.Phase;
   Client_Growth : CG.Controller;
   Views : array (1 .. 17) of V.Backing;
   OK, Found, Consumed : Boolean;
   From : ProcessID;
   Msg, Answer : Message;
   Receipt : aliased CompletionEntry;
   Deadline : Unsigned_64;
begin
   debugPrint ("native allocation IPC: real loopback transport (NO GPU/ISOLATION)" & ASCII.LF);
   Check (syscall (SYSCALL_POLICY_MINT_CAPABILITY, PID, 1, PID,
     16#4947#, 3, 15) /= Unsigned_64'Last, "self endpoint");
   D.Configure (Dispatcher, 65536, 1000, OK); Check (OK, "supervisor configure");
   CG.Configure (Client_Growth, 65536, 1000, OK); Check (OK, "driver configure");
   A.Configure_Heap (Pool, 64 * 1024 * 1024, 2 ** 32, OK); Check (OK, "supervisor heap policy");
   Client.Configure_Heap (Driver, 64 * 1024 * 1024, 2 ** 32, 65536, OK);
   Check (OK, "driver heap policy");
   for Index in Views'Range loop
      CG.Request (Client_Growth, Index, OK); Check (OK, "driver growth request");
      for Turn in 1 .. 8 loop
         CG.Step (Client_Growth);
         exit when CG.Snapshot (Client_Growth).State in CG.Idle | CG.Failed;
      end loop;
      Check (CG.Snapshot (Client_Growth).State = CG.Idle, "driver growth");
      Client.Start (Driver, Index, (if Index = 1 then 1024 else 512), OK);
      Check (OK, "driver start");
      Deadline := syscall (SYSCALL_GETTIME) + 30_000;
      while Client.Pending (Driver) loop
         declare Before : constant Natural := Calls; begin
            D.Step (Dispatcher);
            Check (Calls <= Before + 1, "unbounded backing step");
         end;
         Poll_Service_Request (From, Msg, Found);
         if Found then
            Check (Unsigned_64 (From) = PID and Msg.authorityTag = 16#4947#,
              "kernel endpoint attribution");
            if L.Valid_Allocation_Request (Msg.tag.label, Msg.tag.length,
              Msg.tag.flags, Msg.tag.reserved, Msg.words (0), Msg.words (1),
              Msg.words (2), Msg.words (3)) then
               D.Begin_Request (Dispatcher, Positive (Msg.words (0)),
                 L.Page_Count (Msg.words (1)), Unsigned_32 (Msg.words (2)), OK);
               Check (OK, "save allocation request");
               if not Ping_Sent then
                  Answer := NULL_MESSAGE;
                  Answer.tag := (16#7777#, 0, 0, 0);
                  Check (capSubmit (15, Answer, 16#7777#), "interleaved submit");
                  Ping_Sent := True;
               end if;
            elsif Msg.tag = (16#7777#, 0, 0, 0) then
               Check (D.Pending (Dispatcher), "ping must interleave pending allocation");
               Check (reply (From, Msg) = 1, "interleaved reply");
            elsif Msg.tag = (L.Extent_Request_Label, 2, 0, 0) then
               Check (L.Extent_Request_Authorized
                 (Msg.tag.label, Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
                  L.Budget_Words (Msg.words), Unsigned_64 (From), Msg.authorityTag, PID,
                  Intel_GPU_Extent_Directory.Byte_Count (A.Snapshot (Pool)), True),
                 "shared extent admission");
               declare
                  Offset : constant Unsigned_64 := Msg.words (0) * E.Block_Bytes;
                  Part : constant E.Span := Intel_GPU_Extent_Directory.Resolve
                    (A.Snapshot (Pool), Offset, E.Block_Bytes);
               begin
                  Check (Msg.words (1) = PID and Part.Valid, "extent request");
                  Answer := NULL_MESSAGE;
                  Answer.tag := (16#F003#, 4, 0, 0);
                  Answer.words := [Msg.words (0), Part.Address, L.CPU_Base + Offset, PID];
                  Check (reply (From, Answer) = 1, "extent reply");
                  Extents := Extents + 1;
               end;
            else Check (False, "unexpected request"); end if;
         end if;
         if Poll_Completion (Receipt'Address) /= 0 then
            if Receipt.token = 16#7777# then
               Check (Receipt.status = COMPLETION_OK and Receipt.msg.tag.label = 16#7777#,
                 "interleaved completion");
               Pings := Pings + 1;
            else
               Client.Complete (Driver, Receipt, Consumed);
               Check (Consumed, "allocation completion token");
            end if;
         end if;
         Driver_Metadata_Observed := Driver_Metadata_Observed or else
           Client.Last_Stage (Driver) = Client.Awaiting_Extent_Metadata;
         Client.Tick (Driver);
         Check (syscall (SYSCALL_GETTIME) < Deadline, "deadline");
      end loop;
      Views (Index) := Client.Result (Driver);
      Check (V.Valid (Views (Index)), "client validated backing");
      for Previous in 1 .. Index loop
         declare
            Word : Unsigned_64 with Import, Volatile,
              Address => To_Address (Integer_Address (Views (Previous).CPU_Address));
         begin
            Check (Word = (if Previous = Index then 0 else Unsigned_64 (Previous)),
              "clear damaged earlier allocation");
            Word := Unsigned_64 (Previous);
         end;
      end loop;
   end loop;
   Check (Calls = 18 and Extents = 18 and Saves = 17 and Responses = 17 and Pings = 1,
     "transport counters");
   Check (not D.Pending (Dispatcher), "pending saved request");
   Check (Driver_Metadata_Observed and A.Extent_Capacity (Pool) > 16, "both directories grew");
   Check (Poll_Completion (Receipt'Address) = 0, "duplicate completion");
   debugPrint ("TEST: PASS native allocation IPC 17 saved replies 18 extents both directories grew 1 interleaved request (NO GPU/ISOLATION)" & ASCII.LF);
   loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
end Allocation_IPC_Check;
