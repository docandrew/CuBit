with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with Native_GPU_Buffers;
with Native_GPU_Memory;
package body GPU_Buffer_Probe is
   use CuBit.Messages;
   package G renames CuBit.Memory_Grants;
   package R renames CuBit.Grant_References;
   Page : Unsigned_64 := 0;
   Opened, Granted : Boolean := False;
   Mapping_Sequence : Unsigned_64 := 0;
   Reference : R.Reference;
   Magic : constant Unsigned_64 := 16#C0B1_3344_5566_7788#;
   procedure Server (Sender : ProcessID; Request : Message) is
      Answer : Message := NULL_MESSAGE;
      Ignore : Unsigned_64;
      OK : Boolean;
   begin
      Answer.tag := (Request.tag.label, 4, 0, 0);
      Answer.words := [2, 1, 0, 0];
      if Request.tag.length /= 4 or Request.tag.flags /= 0 or Request.tag.reserved /= 0 then
         Ignore := reply (Sender, Answer);
         return;
      end if;
      if Request.tag.label = 16#0A22# and Request.words (0) = 1 then
         if Request.words (1) = 0 and Request.words (2) = 4096 and
           Request.words (3) = 0 and Page = 0 then
            Page := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, 4096);
            if Page /= 0 then
               declare
                  Value : Unsigned_64 with Import, Volatile,
                    Address => To_Address (Integer_Address (Page));
               begin Value := Magic; end;
               Opened := True;
               Answer.words := [0, 1, 1, 4096];
            end if;
         elsif Request.words = [1, 1, 1, 0] and Opened then
            Opened := False;
            Answer.words := [0, 1, 0, 0];
         end if;
      elsif Request.tag.label = 16#0A23# then
         if Request.words = [1, 1, 0, 4096] and Opened and not Granted then
            -- Synthetic service ONLY: synchronous reply authority permits
            -- an ordinary RAM grant. This does NOT test render admission or
            -- the production session-bound recipient endpoint association.
            G.Create_For_Process (Sender, To_Address (Integer_Address (Page)),
                                  1, False, Reference, OK);
            Granted := OK;
            if OK then
               Mapping_Sequence := Mapping_Sequence + 1;
               Answer.words := [0, 1, Mapping_Sequence, R.Encode (Reference)];
            end if;
         elsif Request.words = [1 + 2 * 2 ** 32, Mapping_Sequence, 0, 0]
           and Mapping_Sequence /= 0 then
            if R.Valid_Wire (R.Encode (Reference)) and G.Retirement_Confirmed (Reference) then
               Answer.words := [0, 1, 0, 0];
            else
               G.Revoke (Reference, OK);
               if OK then
                  Answer.words := [(if G.Retirement_Confirmed (Reference) then 0 else 4), 1, 0, 0];
               else Answer.words := [3, 1, 0, 0]; end if;
            end if;
            if Answer.words (0) = 0 then Granted := False; end if;
         end if;
      end if;
      Ignore := reply (Sender, Answer);
   end Server;
   procedure Client (Slot : CapabilitySlot) is
      ID, Mapping : aliased Unsigned_32 := 0;
      Wire, Address : aliased Unsigned_64 := 0;
      Previous_Wire : Unsigned_64 := 0;
      Status : Unsigned_32;
      Passed : Boolean := True;
      procedure Check (Condition : Boolean) is
      begin
         if not Condition then
            Passed := False;
            debugPrint ("TEST: FAIL GPU-BUFFER-IPC" & ASCII.LF);
         end if;
      end Check;
   begin
      Status := Native_GPU_Buffers.Create (Unsigned_64 (Slot), 4096, ID'Access);
      Check (Status = 0 and ID = 1);
      if Status /= 0 then return; end if;
      for Cycle in 1 .. 128 loop
      Status := Native_GPU_Buffers.Map
        (Unsigned_64 (Slot), ID, 0, 4096, 0, Mapping'Access, Wire'Access);
      Check (Status = 0 and Mapping = Unsigned_32 (Cycle) and R.Valid_Wire (Wire));
      if Status /= 0 then return; end if;
      if Previous_Wire /= 0 then
         Check (Wire /= Previous_Wire);
         Status := Native_GPU_Memory.Acquire
           (Unsigned_64 (Slot), Previous_Wire, 0, 4096, 0, Address'Access);
         Check (Status /= 0 and Address = 0);
         Status := Native_GPU_Buffers.Retire_Map (Unsigned_64 (Slot), Mapping - 1);
         Check (Status /= 0);
      end if;
      Status := Native_GPU_Memory.Acquire
        (Unsigned_64 (Slot), Wire, 0, 4096, 1, Address'Access);
      Check (Status /= 0 and Address = 0);
      Status := Native_GPU_Memory.Acquire
        (Unsigned_64 (Slot), Wire, 0, 4096, 0, Address'Access);
      Check (Status = 0 and Address /= 0);
      if Status /= 0 then return; end if;
      declare
         Value : Unsigned_64 with Import, Volatile,
           Address => To_Address (Integer_Address (Address));
      begin Check (Value = Magic); end;
      Status := Native_GPU_Buffers.Retire_Map (Unsigned_64 (Slot), Mapping);
      Check (Status = 4);
      Status := Native_GPU_Memory.Return_Borrow (Wire);
      Check (Status = 0);
      Status := Native_GPU_Buffers.Retire_Map (Unsigned_64 (Slot), Mapping);
      Check (Status = 0);
      Status := Native_GPU_Memory.Acquire
        (Unsigned_64 (Slot), Wire, 0, 4096, 0, Address'Access);
      Check (Status /= 0 and Address = 0);
      Previous_Wire := Wire;
      end loop;
      Status := Native_GPU_Buffers.Close (Unsigned_64 (Slot), ID);
      Check (Status = 0);
      if Passed then
         debugPrint ("GPU-BUFFER-IPC: PASS 128 native grant cycles, stale generations rejected, ordinary RAM" & ASCII.LF);
      end if;
   end Client;
end GPU_Buffer_Probe;
