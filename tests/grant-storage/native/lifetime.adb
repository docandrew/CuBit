with Interfaces; use Interfaces;
with System; use type System.Address;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Launch_Configurations;
with Launch_Configuration;
with CuBit.Memory_Grants;
with CuBit.Handles;
with CuBit.Handles.Boot_Roots;
with CuBit.Handles.Control;
with CuBit.Process_IDs;
with CuBit.Startup_Tables;
with CuBit.Grant_References;
with Cpio;

-- Disposable privileged bootstrap and its unprivileged child, same ELF.
-- No GPU is involved. The host runner bounds hangs and owns the guest.
procedure Lifetime is
   --  The boot image's size: the kernel's launch configuration for the
   --  process it starts in devmgr's place.
   function Boot_Image_Bytes return Unsigned_64 is
      Configuration : Launch_Configuration.Device_Manager_Configuration;
      Configured : Boolean;
   begin
      CuBit.Launch_Configurations.Get (Configuration, Configured);
      return (if Configured then Configuration.Boot_Image else 0);
   end Boot_Image_Bytes;
   package G renames CuBit.Memory_Grants;
   package R renames CuBit.Grant_References;
   --  The bootstrap runs in devmgr's place (the kernel's launch
   --  configuration and boot roots); the child it spawns has neither.
   Roots : constant CuBit.Handles.Boot_Roots.Roots := CuBit.Handles.Boot_Roots.Get;
   Is_Child : constant Boolean := not Roots.Valid;
   First_Child, Second_Child : CuBit.Handles.Control.Spawned_Child;
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

   function Call (Slot : CuBit.Handles.Endpoint; Op : Unsigned_32) return Unsigned_64 is
      M : Message := NULL_MESSAGE;
   begin
      M.tag := (label => Op, length => 0, protocol => 0, ipc => <>, reserved => 0);
      M.tag := capCall (Slot, M, Wait_Forever);
      Check (M.tag.label = 16#F000#, "child reply");
      return M.words (0);
   end Call;

   --  SPAWN gives the spawner the child's Control and an endpoint to it.
   procedure Start (Spawned : out CuBit.Handles.Control.Spawned_Child) is
      Result : constant CuBit.Handles.Control.Spawn_Result :=
        CuBit.Handles.Control.Spawn
          (Roots.Process_Root, ELF, ELF_Size, 5, "grant-child",
           CuBit.Process_IDs.No_Process, (Kind => CuBit.Startup_Tables.Standard_Limit));
   begin
      Check (Result.Spawned, "child spawned");
      Spawned := Result.Value;
      Check (CuBit.Handles.Control.Resume (Spawned.Child, Spawned.Control, 0, 0), "resume child");
   end Start;

   procedure Wait_Retirement (Child : CuBit.Process_IDs.Process_ID) is
   begin
      receive (From, Msg);
      -- In this isolated guest only the bootstrap and child exist. The
      -- child never sends this label; kernel emits it after invalidation.
      -- This is a phase oracle here, NOT a generally authenticated event.
      Check (Msg.tag.label = 16#0103# and Msg.tag.length = 1
        and Msg.words (0) = CuBit.Process_IDs.To_Word (Child), "completed child retirement event");
   end Wait_Retirement;
begin
   if Is_Child then
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
            Msg.tag := (label => 16#F000#, length => 1, protocol => 0, ipc => <>, reserved => 0);
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
     Boot_Image_Bytes, OK);
   Check (OK, "trusted initrd");
   Cpio.fileView (Archive, Cpio.findFile (Archive, "devmgr.svc"), ELF, ELF_Size, OK);
   Check (OK, "child ELF view");
   Start (First_Child);
   Ref := R.Decode (Call (First_Child.Mailbox, 1));
   G.Acquire_Via_Capability (First_Child.Mailbox, Ref, 0, 4096, G.Read_Access, Mapped, OK);
   Check (OK, "retain owner grant");
   Ignore := Call (First_Child.Mailbox, 2);
   Wait_Retirement (First_Child.Child);
   declare View : Bytes with Import, Address => Mapped, Volatile; begin
      for I in View'Range loop
         Check (View (I) = 16#C9#, "contents survive completed owner retirement");
      end loop;
   end;
   G.Acquire_Via_Capability (First_Child.Mailbox, Ref, 0, 4096, G.Read_Access, Denied, OK);
   Check (not OK, "dead endpoint denies acquisition");
   G.Return_Acquisition (Ref, OK);
   Check (OK, "return after endpoint invalidation");
   G.Return_Acquisition (Ref, OK);
   Check (not OK, "duplicate return denied");
   debugPrint ("grant lifetime: retired owner held PID and readable backing PASS" & ASCII.LF);
   Start (Second_Child);
   --  The held grant kept the first child's identity from coming back.
   Check (CuBit.Process_IDs."/=" (Second_Child.Child, First_Child.Child),
          "retired owner's identity is not reused");
   Fresh := R.Decode (Call (Second_Child.Mailbox, 1));
   Check (Fresh.slot = Ref.slot and Fresh.generation /= Ref.generation,
     "same grant slot receives a different generation");
   G.Acquire_Via_Capability (Second_Child.Mailbox, Ref, 0, 4096, G.Read_Access, Denied, OK);
   Check (not OK, "new endpoint rejects old grant");
   G.Acquire_Via_Capability (First_Child.Mailbox, Fresh, 0, 4096, G.Read_Access, Denied, OK);
   Check (not OK, "old endpoint rejects new grant");
   G.Acquire_Via_Capability (Second_Child.Mailbox, Fresh, 0, 4096, G.Read_Access, Mapped, OK);
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
   Ignore := Call (Second_Child.Mailbox, 2);
   Wait_Retirement (Second_Child.Child);
   debugPrint ("TEST: PASS grant lifetime retired owner drained stale identities denied (NO GPU)" & ASCII.LF);
   loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
end Lifetime;
