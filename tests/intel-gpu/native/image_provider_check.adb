with Interfaces; use Interfaces;
with System; use type System.Address;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Capability_Grants;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with Intel_GPU_Extent_Allocator;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Sharing;
with Intel_GPU_Buffer_Requests.Images;
with Intel_GPU_Buffer_Views;
with Intel_GPU_Image_Lease;
with Intel_GPU_Image_Layout;
with Intel_GPU_Image_Consumers;
procedure Image_Provider_Check is
   package G renames CuBit.Memory_Grants;
   package L renames Intel_GPU_Image_Lease;
   package C renames Intel_GPU_Image_Consumers;
   Consumers : C.Ledger;
   CPU_Read : C.Obligation;
   PID : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   Identity, Ignore : Unsigned_64 := 0;
   function Owner return Boolean is (True);
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Sender = PID and Stamp = 16#4947# then Identity else 0);
   procedure Recipient (Sender, Stamp : Unsigned_64;
      Slot : out CapabilitySlot; Target : out Unsigned_64) is
   begin Slot := 15; Target := Session_Of (Sender, Stamp); end Recipient;
   function Allocate (CPU : Unsigned_64) return Unsigned_64 is
     (syscall (SYSCALL_ALLOC_DMA, PID, 9, CPU, 3, 2 ** 32));
   package A is new Intel_GPU_Extent_Allocator (Owner, Allocate);
   package B is new Intel_GPU_Buffer_Requests (Session_Of, Owner);
   package S is new B.Sharing (Recipient);
   Table : S.Mapping_Table;
   -- No GPU/display consumers exist in this fixture. These identities and
   -- completion facts MUST NOT be copied into production admission.
   function Authorize (Key : L.Identity; Image : Intel_GPU_Image_Layout.Descriptor)
      return Boolean is (Key.Adapter = 1 and Key.Output_Epoch = 1);
   function Producer_Drained (Session, Allocation : Unsigned_64) return Boolean is
     (not S.Writable_Buffer_Held (Table, Session, Allocation));
   function Consumers_Drained (Key : L.Identity) return Boolean is
     (C.Drained (Consumers, Key));
   package P is new B.Images (Authorize, Producer_Drained, Consumers_Drained);
   Pool : A.Pool;
   Buffer : Intel_GPU_Buffer_Reply.Extent_View;
   Object : B.Service;
   Lease : L.Lease;
   Key : L.Identity;
   Image : constant Intel_GPU_Image_Layout.Descriptor :=
     (Intel_GPU_Image_Layout.BGRA8_UNorm, Intel_GPU_Image_Layout.Linear, 16, 16, 64, 0);
   Reply : B.Words;
   Ticket : B.Ticket;
   Writer, Attempt, Reader : S.Mapping_ID;
   Wire, Other_Wire : Unsigned_64;
   Ref : G.Grant_Reference;
   Address : System.Address;
   State : Intel_GPU_Buffer_Views.View_State;
   OK : Boolean;
   use type L.State, Intel_GPU_Buffer_Views.View_State;
   use type S.Retirement_State;
   procedure Check (Condition : Boolean; Detail : String) is
   begin
      if not Condition then
         debugPrint ("TEST: FAIL native image provider: " & Detail & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT);
         loop null; end loop;
      end if;
   end Check;
begin
   debugPrint ("native image provider: real CPU grants, synthetic GPU/output (NO GPU/ISOLATION)" & ASCII.LF);
   Check (syscall (SYSCALL_POLICY_MINT_CAPABILITY, PID, 1, PID,
     16#4947#, 3, 15) /= Unsigned_64'Last, "self endpoint");
   Identity := CuBit.Capability_Grants.Incarnation (CuBit.Capability_Grants.Capture (15));
   Check (Identity /= 0, "identity");
   A.Acquire_Buffer (Pool, PID, 1, 1, 1, Buffer, OK); Check (OK, "backing");
   B.Handle (Object, PID, 16#4947#, B.Label, 4, 0, 0,
     [1, B.Create, 4096, 0], Reply, Ticket);
   Check (Ticket /= 0, "ticket");
   B.Complete (Object, Ticket, Intel_GPU_Buffer_Reply.From_View (Buffer), Reply, OK);
   Check (OK, "allocation");
   Key := (1, Identity, Reply (2), 1, 1, 1, 0, Identity);
   S.Map (Object, Table, PID, 16#4947#, Key.Allocation, 0, 4096, True, Writer, Wire);
   Check (Writer /= 0 and Wire /= 0, "writer export");
   Ref := CuBit.Grant_References.Decode (Wire);
   G.Acquire_Via_Capability (15, Ref, 0, 4096, G.Write_Access, Address, OK);
   Check (OK and Address /= System.Null_Address, "writer acquire");
   P.Prepare (Object, PID, 16#4947#, Key, Image, Lease, OK);
   Check (not OK and L.Current (Lease) = L.Empty, "live writer blocks lease");
   S.Retire (Object, Table, PID, 16#4947#, Writer, OK, State);
   Check (OK and State = Intel_GPU_Buffer_Views.Retiring, "writer pending revoke");
   P.Prepare (Object, PID, 16#4947#, Key, Image, Lease, OK);
   Check (not OK, "held acquisition blocks lease after revoke");
   G.Return_Acquisition (Ref, OK); Check (OK, "writer acquisition returned");
   S.Poll (Object, Table);
   P.Prepare (Object, PID, 16#4947#, Key, Image, Lease, OK);
   Check (OK and P.Backing (Object, Lease, Key).Ready, "drained producer lease");
   S.Map (Object, Table, PID, 16#4947#, Key.Allocation, 0, 4096, True, Attempt, Other_Wire);
   Check (Attempt = 0 and Other_Wire = 0, "lease blocks new writer");
   C.Open (Consumers, Key, OK); Check (OK, "consumer ledger");
   C.Reserve (Consumers, Key, C.CPU, CPU_Read, OK);
   Check (OK, "reserve before reader export");
   S.Map (Object, Table, PID, 16#4947#, Key.Allocation, 0, 4096, False, Reader, Wire);
   Check (Reader /= 0 and Wire /= 0, "consumer read export");
   Ref := CuBit.Grant_References.Decode (Wire);
   G.Acquire_Via_Capability (15, Ref, 0, 4096, G.Read_Access, Address, OK);
   Check (OK, "consumer acquisition");
   C.Stop (Consumers, Key, OK); Check (OK, "close consumer dispatch");
   -- This serialized fixture admits no more readers after Stop. Production
   -- must close every dispatch path before using the table-clear observation.
   S.Retire (Object, Table, PID, 16#4947#, Reader, OK, State);
   Check (OK and State = Intel_GPU_Buffer_Views.Retiring, "consumer pending revoke");
   P.Retire (Object, Lease, Key, OK);
   Check (not OK, "pending consumer holds lease");
   C.Complete (Consumers, Key, CPU_Read,
     S.Observe_Buffer_Retirement (Table, Identity, Key.Allocation) = S.Clear, OK);
   Check (not OK, "pending real reader cannot complete obligation");
   G.Return_Acquisition (Ref, OK); Check (OK, "consumer acquisition return");
   S.Poll (Object, Table);
   C.Complete (Consumers, Key, CPU_Read,
     S.Observe_Buffer_Retirement (Table, Identity, Key.Allocation) = S.Clear, OK);
   Check (OK and C.Drained (Consumers, Key), "confirmed CPU completion");
   debugPrint ("native image consumers: real reader retirement discharges exact obligation PASS" & ASCII.LF);
   P.Retire (Object, Lease, (Key with delta Serial => 2), OK);
   Check (not OK, "wrong retirement identity");
   P.Retire (Object, Lease, Key, OK); Check (OK, "retire exact lease");
   S.Map (Object, Table, PID, 16#4947#, Key.Allocation, 0, 4096, True, Attempt, Other_Wire);
   Check (Attempt /= 0 and Other_Wire /= 0, "writer readmitted after retirement");
   S.Retire (Object, Table, PID, 16#4947#, Attempt, OK, State);
   Check (OK, "final writer retire");
   S.Poll (Object, Table);
   Check (not S.Writable_Buffer_Held (Table, Identity, Key.Allocation), "final drain");
   debugPrint ("TEST: PASS native image provider writer drain lease exclusion and retirement (NO GPU/ISOLATION)" & ASCII.LF);
   loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
end Image_Provider_Check;
