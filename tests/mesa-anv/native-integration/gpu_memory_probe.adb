with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with Native_GPU_Memory;
package body GPU_Memory_Probe is
   use CuBit.Messages;
   package G renames CuBit.Memory_Grants;
   package R renames CuBit.Grant_References;
   Reference : R.Reference;
   Created : Boolean := False;
   Label : constant Unsigned_32 := 16#0A30#;
   Magic : constant Unsigned_64 := 16#C0B1_1234_9876_5678#;

   procedure Server (Sender : ProcessID; Request : Message) is
      Answer : Message := NULL_MESSAGE;
      Address, Ignore : Unsigned_64;
      OK : Boolean;
   begin
      Answer.tag := (Label, 4, 0, 0);
      if Request.words (0) = 0 and not Created then
         Address := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, 4096);
         if Address /= 0 then
            declare
               Value : Unsigned_64 with Import, Volatile,
                 Address => To_Address (Integer_Address (Address));
            begin
               Value := Magic;
            end;
            -- Test-only synchronous reply-authorized recipient. Production
            -- GPU views instead require an authenticated session endpoint.
            G.Create_For_Process (Sender, To_Address (Integer_Address (Address)),
                                  1, False, Reference, OK);
            Created := OK;
            if OK then Answer.words (0) := R.Encode (Reference); end if;
         end if;
      elsif Created and Request.words (0) = 1 then
         G.Revoke (Reference, OK);
         Answer.words (0) := (if OK then 1 else 0);
         Answer.words (1) := (if G.Retirement_Confirmed (Reference) then 1 else 0);
      elsif Created and Request.words (0) = 2 then
         Answer.words (0) := (if G.Retirement_Confirmed (Reference) then 1 else 0);
      end if;
      Ignore := reply (Sender, Answer);
   end Server;

   procedure Client (Slot, Empty : CapabilitySlot) is
      Msg : Message := NULL_MESSAGE;
      Tag : MessageTag;
      Wire : Unsigned_64;
      Address, Address2 : aliased Unsigned_64 := 0;
      Status : Unsigned_32;
      Passed : Boolean := True;
      procedure Check (Condition : Boolean) is
      begin
         if not Condition then
            Passed := False;
            debugPrint ("TEST: FAIL GPU-MEMORY-IPC" & ASCII.LF);
         end if;
      end Check;
      procedure Call (Operation : Unsigned_64) is
      begin
         Msg := NULL_MESSAGE;
         Msg.tag := (Label, 4, 0, 0);
         Msg.words (0) := Operation;
         Tag := capCall (Slot, Msg);
         Check (Tag = (Label, 4, 0, 0) and Msg.tag = Tag);
      end Call;
   begin
      Call (0);
      Wire := Msg.words (0);
      if not R.Valid_Wire (Wire) then Check (False); return; end if;
      Status := Native_GPU_Memory.Acquire (Empty, Wire, 0, 4096, 0, Address'Access);
      Check (Status /= 0 and Address = 0);
      Status := Native_GPU_Memory.Acquire (Slot, Wire, 0, 4096, 1, Address'Access);
      Check (Status /= 0 and Address = 0);
      Status := Native_GPU_Memory.Acquire (Slot, Wire, 0, 4096, 0, Address'Access);
      Check (Status = 0 and Address /= 0);
      if Status /= 0 or Address = 0 then return; end if;
      declare
         Value : Unsigned_64 with Import, Volatile,
           Address => To_Address (Integer_Address (Address));
      begin
         Check (Value = Magic);
      end;
      Status := Native_GPU_Memory.Acquire (Slot, Wire, 0, 4096, 0, Address2'Access);
      Check (Status = 0 and Address2 = Address);
      if Status /= 0 then return; end if;
      Call (1);
      Check (Msg.words (0) = 1 and Msg.words (1) = 0);
      Status := Native_GPU_Memory.Acquire (Slot, Wire, 0, 4096, 0, Address2'Access);
      Check (Status /= 0 and Address2 = 0);
      Status := Native_GPU_Memory.Return_Borrow (Wire);
      Check (Status = 0);
      Call (2);
      Check (Msg.words (0) = 0);
      Status := Native_GPU_Memory.Return_Borrow (Wire);
      Check (Status = 0);
      Call (2);
      Check (Msg.words (0) = 1);
      Status := Native_GPU_Memory.Return_Borrow (Wire);
      Check (Status /= 0);
      Status := Native_GPU_Memory.Acquire (Slot, Wire, 0, 4096, 0, Address2'Access);
      Check (Status /= 0 and Address2 = 0);
      if Passed then
         debugPrint ("GPU-MEMORY-IPC: PASS native grants, read-only denial, deferred retirement, stale rejection" & ASCII.LF);
      end if;
   end Client;
end GPU_Memory_Probe;
