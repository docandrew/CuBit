with Interfaces; use Interfaces;
with System; use type System.Address;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Capability_Grants;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with Intel_GPU_Extent_Allocator;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Sharing;
with Intel_GPU_Buffer_Views;
with Intel_GPU_Metadata_Platform;
with Intel_GPU_Record_Growth;

-- Real kernel mappings and metadata reservation/commit, privileged loopback.
-- Forced growth tests stable storage, not >16 simultaneous owner grants,
-- automatic admission growth, GPU execution or cross-process isolation.
procedure Mapping_Growth_Check is
   package G renames CuBit.Memory_Grants;
   package V renames Intel_GPU_Buffer_Views;
   package R renames Intel_GPU_Buffer_Reply;
   use type V.View_State;
   PID : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   Identity, Ignore : Unsigned_64 := 0;
   function Owner return Boolean is (True);
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Sender = PID and Stamp = 16#4947# then Identity else 0);
   procedure Recipient_Of
     (Sender, Stamp : Unsigned_64; Slot : out CapabilitySlot;
      Target : out Unsigned_64) is
   begin
      Slot := 15;
      Target := Session_Of (Sender, Stamp);
   end Recipient_Of;
   function Allocate (CPU : Unsigned_64) return Unsigned_64 is
     (syscall (SYSCALL_ALLOC_DMA, PID, 9, CPU, 3, 2 ** 32));
   package A is new Intel_GPU_Extent_Allocator (Owner, Allocate);
   package B is new Intel_GPU_Buffer_Requests (Session_Of, Owner);
   package S is new B.Sharing (Recipient_Of);
   use type S.Retirement_State;
   Pool : A.Pool;
   Buffer : R.Extent_View;
   Object : B.Service;
   Table : S.Mapping_Table;
   function Capacity return Positive is (S.Record_Capacity (Table));
   procedure Publish (Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin S.Extend_Storage (Table, Base, Bytes, Accepted); end Publish;
   package Growth is new Intel_GPU_Record_Growth
     (Intel_GPU_Metadata_Platform.Storage, Capacity, Publish);
   use type Growth.Phase;
   Controller : Growth.Controller;
   Response : B.Words;
   Ticket : B.Ticket;
   First, Expanded, Current : S.Mapping_ID;
   First_Ref, Expanded_Ref : G.Grant_Reference;
   Wire, ID : Unsigned_64;
   Address : System.Address;
   OK : Boolean;
   State : V.View_State;
   procedure Check (Condition : Boolean; Detail : String) is
   begin
      if not Condition then
         debugPrint ("TEST: FAIL native mapping growth: " & Detail & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT);
         loop null; end loop;
      end if;
   end Check;
   procedure Grow (Target : Positive) is
   begin
      Growth.Request (Controller, Target, OK); Check (OK, "growth request");
      for Turn in 1 .. 100 loop
         Growth.Step (Controller);
         exit when Growth.Snapshot (Controller).State in Growth.Idle | Growth.Failed;
      end loop;
      Check (Growth.Snapshot (Controller).State = Growth.Idle and
        Capacity >= Target, "committed typed metadata");
   end Grow;
   procedure Acquire (Ref : G.Grant_Reference) is
   begin
      G.Acquire_Via_Capability (15, Ref, 0, 4096, G.Read_Access, Address, OK);
      Check (OK and Address /= System.Null_Address, "reader acquisition");
      declare Word : Unsigned_64 with Import, Volatile, Address => Address; begin
         Check (Word = 16#CAFE_0065#, "retained content");
      end;
   end Acquire;
begin
   debugPrint ("native mapping growth: forced metadata, real self-grants (NO GPU/ISOLATION)" & ASCII.LF);
   Check (syscall (SYSCALL_POLICY_MINT_CAPABILITY, PID, 1, PID,
     16#4947#, 3, 15) /= Unsigned_64'Last, "self endpoint");
   Identity := CuBit.Capability_Grants.Incarnation (CuBit.Capability_Grants.Capture (15));
   Check (Identity /= 0, "endpoint identity");
   A.Acquire_Buffer (Pool, PID, 1, 1, 1, Buffer, OK); Check (OK, "backing");
   declare Word : Unsigned_64 with Import, Volatile,
     Address => To_Address (Integer_Address (R.CPU_Address (Buffer))); begin
      Word := 16#CAFE_0065#;
   end;
   B.Handle (Object, PID, 16#4947#, B.Label, 4, 0, 0,
     [1, B.Create, 4096, 0], Response, Ticket);
   Check (Ticket /= 0, "allocation ticket");
   B.Complete (Object, Ticket, R.From_View (Buffer), Response, OK);
   Check (OK and Response (0) = B.OK, "allocation completion");
   ID := Response (2);
   S.Map (Object, Table, PID, 16#4947#, ID, 0, 4096, False, First, Wire);
   Check (First = 1, "inline mapping");
   First_Ref := CuBit.Grant_References.Decode (Wire);
   Acquire (First_Ref);
   for Index in 2 .. S.Initial_Capacity loop
      S.Map (Object, Table, PID, 16#4947#, ID, 0, 4096, False, Current, Wire);
      Check (Current = S.Mapping_ID (Index), "consume inline record");
      S.Retire (Object, Table, PID, 16#4947#, Current, OK, State);
      Check (OK and State = V.Retired, "temporary grant retirement");
   end loop;
   Growth.Configure (Controller, 65536, 512, OK); Check (OK, "configure growth");
   Grow (128);
   Acquire (First_Ref);
   G.Return_Acquisition (First_Ref, OK); Check (OK, "return extra inline reader");
   S.Map (Object, Table, PID, 16#4947#, ID, 0, 4096, False, Expanded, Wire);
   Check (Expanded = 65, "expanded mapping");
   Expanded_Ref := CuBit.Grant_References.Decode (Wire);
   Acquire (Expanded_Ref);
   Grow (256);
   Acquire (Expanded_Ref);
   G.Return_Acquisition (Expanded_Ref, OK); Check (OK, "return extra expanded reader");
   B.Handle (Object, PID, 16#4947#, B.Label, 4, 0, 0,
     [1, B.Close, ID, 0], Response, Ticket);
   Check (Response (0) = B.OK and Ticket = 0, "close name");
   S.Retire_Session (Object, Table, Identity);
   S.Poll (Object, Table);
   Check (S.Observe_Retirement (Table, Identity) = S.Outstanding,
     "readers retain both storage tiers");
   G.Return_Acquisition (First_Ref, OK); Check (OK, "return inline reader");
   S.Poll (Object, Table);
   Check (S.Observe_Retirement (Table, Identity) = S.Outstanding,
     "expanded reader still retains");
   G.Return_Acquisition (Expanded_Ref, OK); Check (OK, "return expanded reader");
   S.Poll (Object, Table);
   Check (S.Observe_Retirement (Table, Identity) = S.Clear, "confirmed drain");
   G.Acquire_Via_Capability (15, Expanded_Ref, 0, 4096, G.Read_Access, Address, OK);
   Check (not OK and Address = System.Null_Address, "stale grant denied");
   S.Map (Object, Table, PID, 16#4947#, ID, 0, 4096, False, Current, Wire);
   Check (Current = 0 and Wire = 0, "closed name denied");
   debugPrint ("TEST: PASS native mapping growth 64-128-256 record65 retained readers drained (NO GPU/ISOLATION)" & ASCII.LF);
   loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
end Mapping_Growth_Check;
