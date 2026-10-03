with Interfaces; use Interfaces;
with System; use type System.Address;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Capability_Grants;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with Intel_GPU_Extent_Allocator;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Handles;
with Intel_GPU_Buffer_Views;
with Intel_GPU_Deferred_Retirement;
-- Privileged disposable bootstrap; real self-grants, not GPU or isolation proof.
procedure View_Retention_Check is
   package B renames Intel_GPU_Buffer_Reply;
   package H renames Intel_GPU_Buffer_Handles;
   package V renames Intel_GPU_Buffer_Views;
   package G renames CuBit.Memory_Grants;
   package D renames Intel_GPU_Deferred_Retirement;
   use type V.View_State;
   PID : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   function Owner return Boolean is (True);
   function Allocate (CPU : Unsigned_64) return Unsigned_64 is
     (syscall (SYSCALL_ALLOC_DMA, PID, 9, CPU, 3, 2 ** 32));
   package A is new Intel_GPU_Extent_Allocator (Owner, Allocate);
   Pool : A.Pool;
   Registry : H.Registry;
   Buffer : B.Extent_View;
   Backing : B.Backing;
   Identity, Ignore : Unsigned_64;
   OK : Boolean;
   procedure Check (Condition : Boolean; Detail : String) is
   begin
      if not Condition then
         debugPrint ("TEST: FAIL native view retention: " & Detail & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT);
         loop null; end loop;
      end if;
   end Check;
begin
   debugPrint ("native view retention: real self-grants (NO GPU/ISOLATION)" & ASCII.LF);
   Check (syscall (SYSCALL_POLICY_MINT_CAPABILITY, PID, 1, PID,
     16#4947#, 3, 15) /= Unsigned_64'Last, "self endpoint");
   Identity := CuBit.Capability_Grants.Incarnation
     (CuBit.Capability_Grants.Capture (15));
   Check (Identity /= 0, "endpoint identity");
   A.Acquire_Buffer (Pool, PID, 1, 1, 1, Buffer, OK);
   Check (OK, "backing");
   Backing := B.From_View (Buffer);
   declare
      Reference : G.Grant_Reference;
      Address, Denied : System.Address;
      Word : Unsigned_64 with Import, Volatile,
        Address => To_Address (Integer_Address (Backing.CPU_Address));
   begin
      Word := 16#A70C_1C00#;
      G.Create_Via_Capability
        (15, To_Address (Integer_Address (Backing.CPU_Address)),
         1, False, Reference, OK);
      Check (OK, "legacy revoke setup");
      G.Acquire_Via_Capability
        (15, Reference, 0, 4096, G.Read_Access, Address, OK);
      Check (OK, "legacy revoke reader");
      Check (syscall (SYSCALL_REVOKE_SHARED_MEMORY_GRANT,
        Unsigned_64'Last) = 0, "legacy revoke wrong owner rejected");
      Check (syscall (SYSCALL_REVOKE_SHARED_MEMORY_GRANT,
        Reference.slot) = 1, "legacy revoke active accepted");
      Check (not G.Retirement_Confirmed (Reference), "legacy revoke reader retains mapping");
      G.Acquire_Via_Capability
        (15, Reference, 0, 4096, G.Read_Access, Denied, OK);
      Check (not OK, "legacy revoke closes admission");
      declare Alias : Unsigned_64 with Import, Volatile, Address => Address; begin
         Check (Alias = Word, "legacy revoke retained content");
      end;
      G.Return_Acquisition (Reference, OK);
      Check (OK and then G.Retirement_Confirmed (Reference), "legacy revoke reader drains");
      Check (syscall (SYSCALL_REVOKE_SHARED_MEMORY_GRANT,
        Reference.slot) = 0, "legacy revoke retired rejected");
   end;
   debugPrint ("native legacy revoke: locked admission and reader drain PASS" & ASCII.LF);
   for Cycle in 1 .. 3 loop
      declare
         First, Second : V.View;
         Export_Pin, Independent_Pin : H.Retained_Reference;
         Name : H.Handle;
         Reference, Child, Rejected : G.Grant_Reference;
         Address, Child_Address, Denied : System.Address;
         Word : Unsigned_64 with Import, Volatile,
           Address => To_Address (Integer_Address (Backing.CPU_Address));
         Queue : D.Queue;
         Attempts, Finished : Natural := 0;
         function Attempt (Index : D.Slot; Item : D.Candidate) return D.Outcome is
         begin
            Check (Index = 1 and Item.Ticket = 1 and Item.Session = Identity and
              Item.Sender = PID and Item.Stamp = 16#4947# and Item.Handle = Unsigned_64 (Name),
              "queued identity");
            Attempts := Attempts + 1;
            V.Poll_Retirement (First, Registry);
            if V.State (First) = V.Retired then
               Finished := Finished + 1;
               return D.Submitted;
            end if;
            Check (V.State (First) = V.Retiring, "queued retirement state");
            return D.Waiting;
         end Attempt;
         procedure Poll is new D.Poll (Attempt);
      begin
         Word := 16#CAFE_0000# + Unsigned_64 (Cycle);
         H.Register (Registry, Identity, Backing, Name);
         Check (Name /= 0, "register");
         V.Share (First, Registry, Identity, Name, 15, Identity, 0, 4096,
           Writable => False, Presentation => True);
         V.Share (Second, Registry, Identity, Name, 15, Identity, 0, 4096,
           Writable => False);
         Check (V.State (First) = V.Shared and V.State (Second) = V.Shared,
           "two kernel grants");
         Reference := CuBit.Grant_References.Decode (V.Wire_Reference (First));
         G.Acquire_Via_Capability (15, Reference, 0, 4096, G.Write_Access, Denied, OK);
         Check (not OK and Denied = System.Null_Address, "write escalation rejected");
         G.Acquire_Via_Capability (15, Reference, 0, 4096, G.Read_Access, Address, OK);
         Check (OK, "read acquisition");
         declare Alias : Unsigned_64 with Import, Volatile, Address => Address; begin
            Check (Alias = Word, "shared content");
         end;
         G.Derive_Via_Capability (15, Reference, 0, 1, True, Rejected, OK);
         Check (not OK, "forwarding cannot add write access");
         G.Derive_Via_Capability (15, Reference, 0, 1, False, Child, OK);
         Check (OK, "terminal child");
         G.Acquire_Via_Capability (15, Child, 0, 4096, G.Read_Access, Child_Address, OK);
         Check (OK, "child acquisition");
         G.Derive_Via_Capability (15, Child, 0, 1, False, Rejected, OK);
         Check (not OK, "child cannot forward again");
         H.Retain_Backing (Registry, Identity, Name, Export_Pin, OK);
         Check (OK, "independent lifetime admission");
         H.Close (Registry, Identity, Name, OK); Check (OK, "close name");
         H.Retain_Referenced_Backing (Registry, Export_Pin, Independent_Pin, OK);
         Check (OK, "split retained lifetime after name closure");
         H.Return_Reference (Registry, Export_Pin, True, OK);
         Check (OK, "original internal user retires");
         Check (not H.Can_Release_Backing (Registry, Identity, Name), "two pins");
         V.Retire (First, Registry);
         Check (V.State (First) = V.Retiring, "active acquisition retains grant");
         Check (not H.Can_Release_Backing (Registry, Identity, Name), "pending reader pin");
         D.Remember (Queue, 1, (1, Identity, PID, 16#4947#, Unsigned_64 (Name)));
         for Turn in 1 .. 8 loop Poll (Queue); end loop;
         Check (Attempts = 8 and Finished = 0, "pending root fair polling");
         G.Return_Acquisition (Reference, OK); Check (OK, "return reader");
         Poll (Queue);
         Check (V.State (First) = V.Retiring, "forwarded reader still pins root");
         Check (not H.Can_Release_Backing (Registry, Identity, Name), "forwarded pin");
         G.Acquire_Via_Capability (15, Child, 0, 4096, G.Read_Access, Denied, OK);
         Check (not OK, "root revocation closes child admission");
         declare Alias : Unsigned_64 with Import, Volatile, Address => Child_Address; begin
            Check (Alias = Word, "held child content survives parent return");
         end;
         G.Return_Acquisition (Child, OK); Check (OK, "return forwarded reader");
         Poll (Queue);
         Check (Attempts = 10 and Finished = 1, "one terminal retirement");
         for Turn in 1 .. 32 loop Poll (Queue); end loop;
         Check (Attempts = 10 and Finished = 1, "no terminal replay");
         Check (V.State (First) = V.Retired, "kernel confirmed first retirement");
         Check (not H.Can_Release_Backing (Registry, Identity, Name), "second pin survives");
         V.Retire (Second, Registry);
         Check (V.State (Second) = V.Retired, "second retirement");
         H.Release_Retired_Backing (Registry, Identity, Name, True, OK);
         Check (not OK and then H.Referenced_Backing (Registry, Independent_Pin).Ready,
           "independent pin outlives both kernel grants");
         H.Return_Reference (Registry, Independent_Pin, True, OK);
         Check (OK, "independent internal user retires");
         H.Release_Retired_Backing (Registry, Identity, Name, True, OK);
         Check (OK, "release after both grants");
         G.Acquire_Via_Capability (15, Reference, 0, 4096, G.Read_Access, Denied, OK);
         Check (not OK, "stale grant rejected");
         Check (Word = 16#CAFE_0000# + Unsigned_64 (Cycle), "owner backing retained");
      end;
   end loop;
   -- Eight simultaneously held roots force multiple lazy forwarding blocks
   -- with the current seven-scopes/page native layout. Ordinary root/child
   -- identities cross the former sixteen-slot ceiling; later rounds reuse identities
   -- only after both mapping and forwarding retirement are confirmed.
   for Round in 1 .. 32 loop
      declare
         Roots, Children : array (1 .. 8) of G.Grant_Reference;
         Addresses : array (1 .. 8) of System.Address;
         Address, Denied : System.Address;
         Extra : G.Grant_Reference;
         Word : Unsigned_64 with Import, Volatile,
           Address => To_Address (Integer_Address (Backing.CPU_Address));
      begin
         Word := 16#B10C_0000# + Unsigned_64 (Round);
         for Index in Roots'Range loop
            G.Create_Forwardable_Via_Capability
              (15, To_Address (Integer_Address (Backing.CPU_Address)),
               1, False, Roots (Index), OK);
            Check (OK, "block root creation");
            G.Acquire_Via_Capability
              (15, Roots (Index), 0, 4096, G.Read_Access, Address, OK);
            Check (OK, "block root acquisition");
            G.Derive_Via_Capability
              (15, Roots (Index), 0, 1, False, Children (Index), OK);
            Check (OK, "block child creation");
            G.Acquire_Via_Capability
              (15, Children (Index), 0, 4096, G.Read_Access, Addresses (Index), OK);
            Check (OK, "block child acquisition");
            -- New block initialization must preserve all earlier scopes.
            for Previous in Roots'First .. Index loop
               G.Acquire_Via_Capability
                 (15, Children (Previous), 0, 4096, G.Read_Access, Address, OK);
               Check (OK, "neighbour scope preserved");
               G.Return_Acquisition (Children (Previous), OK);
               Check (OK, "extra neighbour reader returned");
            end loop;
         end loop;
         G.Create_Forwardable_Via_Capability
           (15, To_Address (Integer_Address (Backing.CPU_Address)),
            1, False, Extra, OK);
         Check (OK, "seventeenth grant admitted without disturbing scopes");
         G.Revoke (Extra, OK);
         Check (OK and then G.Retirement_Confirmed (Extra), "extra grant retired");
         for Index in Roots'Range loop
            G.Revoke (Roots (Index), OK); Check (OK, "block root revoke");
            G.Return_Acquisition (Roots (Index), OK);
            Check (OK, "block root reader return");
            Check (not G.Retirement_Confirmed (Roots (Index)), "child retains root");
            G.Acquire_Via_Capability
              (15, Children (Index), 0, 4096, G.Read_Access, Denied, OK);
            Check (not OK, "closed block child denies new reader");
            declare Alias : Unsigned_64 with Import, Volatile,
              Address => Addresses (Index); begin
               Check (Alias = Word, "held block child content");
            end;
         end loop;
         for Index in reverse Roots'Range loop
            G.Return_Acquisition (Children (Index), OK);
            Check (OK, "block child reader return");
            Check (G.Retirement_Confirmed (Children (Index)) and
              G.Retirement_Confirmed (Roots (Index)), "block confirmed retirement");
            G.Acquire_Via_Capability
              (15, Children (Index), 0, 4096, G.Read_Access, Denied, OK);
            Check (not OK, "retired block child stale");
         end loop;
      end;
   end loop;
   debugPrint ("native forwarding blocks: 32 rounds eight retained roots and children PASS" & ASCII.LF);
   debugPrint ("TEST: PASS native view retention 3 cycles two pins terminal child queued retirement (NO GPU/ISOLATION)" & ASCII.LF);
   loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
end View_Retention_Check;
