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
   Active : Boolean := True;
   function Owner return Boolean is (True);
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Active and Sender = PID and Stamp = 16#4947# then Identity else 0);
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
   Buffer_Ticket : B.Ticket;
   Buffer_Retirement : B.Session_Retirement;
   Map_Retirement : S.Mapping_Retirement;
   First, Expanded, Current : S.Mapping_ID;
   First_Ref, Expanded_Ref : G.Grant_Reference;
   Wire, ID : Unsigned_64;
   Address : System.Address;
   OK, Complete : Boolean;
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
   Buffer_Ticket := Ticket;
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
   Acquire (Expanded_Ref);
   G.Return_Acquisition (Expanded_Ref, OK); Check (OK, "return extra expanded reader");
   S.Retire (Object, Table, PID, 16#4947#, Expanded, OK, State);
   Check (OK and State = V.Retiring, "expanded reader retirement pending");
   S.Poll (Object, Table); -- Cursor is not at the start when metadata grows.
   Grow (256);
   S.Retire (Object, Table, PID, 16#4947#, Expanded, OK, State);
   Check (OK and State = V.Retiring, "pending expanded grant survived growth");
   debugPrint ("TEST: PASS native mapping growth preserves pending retirement" & ASCII.LF);
   declare
      Scratch : R.Extent_View;
      Previous_Ticket, Replacement_Ticket : B.Ticket := 0;
      Previous_Name, Replacement_Name : Unsigned_64 := 0;
      Replacement_Map : S.Mapping_ID;
      Ref : G.Grant_Reference;
   begin
      A.Acquire_Buffer (Pool, PID, 2, 1, 1, Scratch, OK);
      Check (OK, "neighbor backing slice");
      declare Word : Unsigned_64 with Import, Volatile,
        Address => To_Address (Integer_Address (R.CPU_Address (Scratch))); begin
         Word := 16#CAFE_0065#;
      end;
      for Cycle in 1 .. 128 loop
         B.Handle (Object, PID, 16#4947#, B.Label, 4, 0, 0,
           [1, B.Create, 4096, 0], Response, Replacement_Ticket);
         Check (Replacement_Ticket /= 0, "replacement ticket");
         if Previous_Ticket /= 0 then
            Check (Replacement_Ticket = Previous_Ticket + B.Ticket_Stride,
              "same slot with fresh generation");
         end if;
         B.Complete (Object, Replacement_Ticket, R.From_View (Scratch), Response, OK);
         Check (OK and Response (0) = B.OK, "replacement completion");
         Replacement_Name := Response (2);
         Check (Replacement_Name > Previous_Name, "fresh monotonic name");
         if Previous_Name /= 0 then
            S.Map (Object, Table, PID, 16#4947#, Previous_Name,
              0, 4096, False, Current, Wire);
            Check (Current = 0 and Wire = 0, "replaced name cannot map");
            B.Handle (Object, PID, 16#4947#, B.Label, 4, 0, 0,
              [1, B.Close, Previous_Name, 0], Response, Ticket);
            Check (Response (0) = B.Denied, "replaced name cannot close");
         end if;
         S.Map (Object, Table, PID, 16#4947#, Replacement_Name,
           0, 4096, False, Replacement_Map, Wire);
         Check (Replacement_Map /= 0, "replacement mapping");
         Ref := CuBit.Grant_References.Decode (Wire);
         Acquire (Ref);
         B.Handle (Object, PID, 16#4947#, B.Label, 4, 0, 0,
           [1, B.Close, Replacement_Name, 0], Response, Ticket);
         Check (Response (0) = B.OK, "close replacement name");
         Check (not B.Can_Retire (Object, Identity, Replacement_Ticket),
           "held reader prevents replacement retirement");
         S.Retire (Object, Table, PID, 16#4947#, Replacement_Map, OK, State);
         Check (OK and State = V.Retiring, "replacement reader retained");
         G.Return_Acquisition (Ref, OK); Check (OK, "return replacement reader");
         S.Retire (Object, Table, PID, 16#4947#, Replacement_Map, OK, State);
         Check (OK and State = V.Retired, "replacement CPU drain");
         -- This fixture never submits GPU work. Trusted acknowledgement here
         -- certifies only this CPU-only test; it is not GPU completion evidence.
         B.Acknowledge_Retirement (Object, Identity, Replacement_Ticket, True, OK);
         Check (OK, "acknowledged CPU-only replacement");
         G.Acquire_Via_Capability (15, Ref, 0, 4096, G.Read_Access, Address, OK);
         Check (not OK and Address = System.Null_Address, "old grant stays denied");
         Acquire (First_Ref);
         G.Return_Acquisition (First_Ref, OK); Check (OK, "neighbor extra reader");
         Check (S.Observe_Retirement (Table, Identity) = S.Outstanding,
           "neighbor retirement still outstanding");
         Previous_Ticket := Replacement_Ticket;
         Previous_Name := Replacement_Name;
      end loop;
   end;
   debugPrint ("TEST: PASS native 128 handle replacements preserve pinned neighbor" & ASCII.LF);
   -- Trusted admission closes before either snapshot. No API can append a
   -- mapping for this session beyond the captured prefix after this point.
   Active := False;
   B.Begin_Retire_Session (Object, Identity, Buffer_Retirement, OK);
   Check (OK, "begin buffer sweep");
   S.Begin_Retire_Session (Object, Table, Identity, Map_Retirement, OK);
   Check (OK, "begin map sweep");
   B.Retire_Session_Step (Object, Buffer_Retirement, Complete);
   Check (not Complete, "name phase is separate from allocation records");
   B.Retire_Session_Step (Object, Buffer_Retirement, Complete);
   Check (Complete, "buffer metadata sweep completed");
   Check (not B.Can_Retire (Object, Identity, Buffer_Ticket),
     "closed name still pinned before grant sweep");
   -- Captured used prefix is193 even though allocated capacity is256.
   -- Thirteen bounded calls initiate retirement; none means reader release.
   for Turn in 1 .. 13 loop
      S.Retire_Session_Step (Object, Table, Map_Retirement, Complete);
      Check (Complete = (Turn = 13), "map sweep twelve chunks of16 then1");
      Check (S.Observe_Retirement (Table, Identity) = S.Outstanding,
        "step completion cannot dismiss held readers");
      Check (not B.Can_Retire (Object, Identity, Buffer_Ticket),
        "real grant pins survive metadata sweep");
      Ignore := syscall (SYSCALL_SLEEP, 1);
   end loop;
   S.Retire_Session_Step (Object, Table, Map_Retirement, Complete);
   Check (Complete, "completed map step idempotent");
   debugPrint ("TEST: PASS native stepped session sweep retains real grant readers" & ASCII.LF);
   S.Poll (Object, Table);
   Check (S.Observe_Retirement (Table, Identity) = S.Outstanding,
     "readers retain both storage tiers");
   G.Return_Acquisition (First_Ref, OK); Check (OK, "return inline reader");
   S.Poll (Object, Table);
   Check (S.Observe_Retirement (Table, Identity) = S.Outstanding,
     "expanded reader still retains");
   G.Return_Acquisition (Expanded_Ref, OK); Check (OK, "return expanded reader");
   -- Poll is deliberately bounded. Walk at most one full table rotation;
   -- these are incremental metadata visits, not retries of grant revocation.
   for Turn in 1 .. (Capacity + S.Poll_Budget - 1) / S.Poll_Budget loop
      S.Poll (Object, Table);
      exit when S.Observe_Retirement (Table, Identity) = S.Clear;
   end loop;
   Check (S.Observe_Retirement (Table, Identity) = S.Clear, "confirmed drain");
   Check (B.Can_Retire (Object, Identity, Buffer_Ticket),
     "buffer name and CPU pins drained, no GPU or backing release implied");
   debugPrint ("TEST: PASS native bounded mapping poll retained readers drained" & ASCII.LF);
   G.Acquire_Via_Capability (15, Expanded_Ref, 0, 4096, G.Read_Access, Address, OK);
   Check (not OK and Address = System.Null_Address, "stale grant denied");
   S.Map (Object, Table, PID, 16#4947#, ID, 0, 4096, False, Current, Wire);
   Check (Current = 0 and Wire = 0, "closed admission denied");
   debugPrint ("TEST: PASS native mapping growth 64-128-256 record65 retained readers drained (NO GPU/ISOLATION)" & ASCII.LF);
   loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
end Mapping_Growth_Check;
