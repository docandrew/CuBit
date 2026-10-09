with Interfaces; use Interfaces;
with System; use type System.Address;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with Cpio;

-- Disposable privileged bootstrap and its unprivileged child, same ELF.
-- No GPU is involved. The host runner bounds hangs and owns the guest.
procedure Lifetime is
   package G renames CuBit.Memory_Grants;
   package R renames CuBit.Grant_References;
   Child : constant Unsigned_64 := 42;
   PID : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   Ignore : Unsigned_64;
   type Bytes is array (0 .. 4095) of Unsigned_8;
   Buffer : Bytes := (others => 16#A3#) with Alignment => 4096, Volatile;
   Msg : Message;
   From : Process_ID;
   OK : Boolean;
   Ref, Fresh : G.Grant_Reference;
   Mapped, Denied, ELF : System.Address;
   ELF_Size : Unsigned_64;
   Archive : Cpio.Archive;

   procedure Check (Value : Boolean; Detail : String) is
   begin
      if not Value then
         debugPrint ("TEST: FAIL grant lifetime: " & Detail & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT);
         loop null; end loop;
      end if;
   end Check;

   function Call (Slot : CapabilitySlot; Op : Unsigned_32) return Unsigned_64 is
      M : Message := NULL_MESSAGE;
   begin
      M.tag := (Op, 0, 0, 0);
      M.tag := capCall (Slot, M, Wait_Forever);
      Check (M.tag.label = 16#F000#, "child reply");
      return M.words (0);
   end Call;

   function Spawn return Unsigned_64 is
     (syscall (SYSCALL_SPAWN, Unsigned_64 (To_Integer (ELF)), ELF_Size,
               5, 0, Child, PID));

   procedure Start (Slot : CapabilitySlot) is
   begin
      Check (Spawn = Child, "requested PID admitted");
      Check (syscall (SYSCALL_POLICY_MINT_CAPABILITY, PID, 6, Child,
        0, 4, 14) /= Unsigned_64'Last, "process authority");
      Check (syscall (SYSCALL_POLICY_MINT_CAPABILITY, PID, 1, Child,
        0, 3, Unsigned_64 (Slot)) /= Unsigned_64'Last, "endpoint authority");
      Check (syscall (SYSCALL_RESUME, Child) = 0, "resume child");
   end Start;

   procedure Wait_Retirement is
   begin
      receive (From, Msg);
      -- In this isolated guest only the bootstrap and child exist. The
      -- child never sends this label; kernel emits it after invalidation.
      -- This is a phase oracle here, NOT a generally authenticated event.
      Check (Msg.tag.label = 16#0103# and Msg.tag.length = 1
        and Msg.words (0) = Child, "completed child retirement event");
   end Wait_Retirement;
begin
   if PID = Child then
      loop
         receive (From, Msg);
         case Msg.tag.label is
            when 1 =>
               G.Create_For_Process (From, Buffer'Address, 1, False, Ref, OK);
               Check (OK, "child creates grant");
               Msg.words (0) := R.Encode (Ref);
            when 2 =>
               Buffer := (others => 16#C9#);
               Msg.words (0) := 0;
            when others => Check (False, "child operation");
         end case;
         declare Exiting : constant Boolean := Msg.tag.label = 2; begin
            Msg.tag := (16#F000#, 1, 0, 0);
            Ignore := reply (From, Msg);
            if Exiting then
               Ignore := syscall (SYSCALL_EXIT);
               loop null; end loop;
            end if;
         end;
      end loop;
   end if;
   debugPrint ("grant lifetime: disposable native owner/PID oracle (NO GPU)" & ASCII.LF);
   Cpio.init (Archive, To_Address (16#0000_5000_0000_0000#),
     getInfo (SYSINFO_RAMDISK_SIZE), OK);
   Check (OK, "trusted initrd");
   Cpio.fileView (Archive, Cpio.findFile (Archive, "devmgr.svc"), ELF, ELF_Size, OK);
   Check (OK, "child ELF view");
   Start (15);
   Ref := R.Decode (Call (15, 1));
   G.Acquire_Via_Capability (15, Ref, 0, 4096, G.Read_Access, Mapped, OK);
   Check (OK, "retain owner grant");
   Ignore := Call (15, 2);
   Wait_Retirement;
   Check (Spawn = Unsigned_64'Last, "held grant prevents PID reuse");
   declare View : Bytes with Import, Address => Mapped, Volatile; begin
      for I in View'Range loop
         Check (View (I) = 16#C9#, "contents survive completed owner retirement");
      end loop;
   end;
   G.Acquire_Via_Capability (15, Ref, 0, 4096, G.Read_Access, Denied, OK);
   Check (not OK, "dead endpoint denies acquisition");
   G.Return_Acquisition (Ref, OK);
   Check (OK, "return after endpoint invalidation");
   G.Return_Acquisition (Ref, OK);
   Check (not OK, "duplicate return denied");
   debugPrint ("grant lifetime: retired owner held PID and readable backing PASS" & ASCII.LF);
   Start (16);
   Fresh := R.Decode (Call (16, 1));
   Check (Fresh.slot = Ref.slot and Fresh.generation /= Ref.generation,
     "same PID/slot receives different grant generation");
   G.Acquire_Via_Capability (16, Ref, 0, 4096, G.Read_Access, Denied, OK);
   Check (not OK, "new endpoint rejects old grant");
   G.Acquire_Via_Capability (15, Fresh, 0, 4096, G.Read_Access, Denied, OK);
   Check (not OK, "old endpoint rejects new grant");
   G.Acquire_Via_Capability (16, Fresh, 0, 4096, G.Read_Access, Mapped, OK);
   Check (OK, "new endpoint and grant accepted");
   G.Return_Acquisition (Ref, OK);
   Check (not OK, "old return cannot release replacement acquisition");
   declare View : Bytes with Import, Address => Mapped, Volatile; begin
      for I in View'Range loop
         Check (View (I) = 16#A3#, "replacement has fresh backing");
      end loop;
   end;
   G.Return_Acquisition (Fresh, OK);
   Check (OK, "return replacement grant");
   Ignore := Call (16, 2);
   Wait_Retirement;
   debugPrint ("TEST: PASS grant lifetime retired owner drained exact PID reused stale identities denied (NO GPU)" & ASCII.LF);
   loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
end Lifetime;
