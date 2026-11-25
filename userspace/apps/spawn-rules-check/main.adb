------------------------------------------------------------------------------
--  CuBit SPAWN rules check (CAP-012, docs/authority-model.md section 7.3)
--
--  Runs in procmgr's place (the headless spawn-rules test puts it in the
--  initrd as procmgr.svc), so it holds what devmgr gives procmgr: the
--  wildcard process root (Read, Write, Execute, Grant) and the test console
--  object it prints on. It spawns a minimal, never-run ELF image of its own
--  making and checks the kernel's three SPAWN rules (Spawn_Admission):
--
--  1. a child's Control, even with every right, does not spawn;
--  2. a spawn authority without Grant or Write gets a Control without them,
--     and neither the child's endpoint nor its CSPACE;
--  3. a spawn authority without Grant cannot name another process as the
--     child's parent; one with Grant can.
--
--  Every child stays suspended and is stopped through the root.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;

with CuBit.Handles; use CuBit.Handles;
with CuBit.Handles.Boot_Roots;
with CuBit.Handles.Control;
with CuBit.Handles.Test_Consoles;
with CuBit.Process_IDs;
with CuBit.Startup_Tables;
with CuBit.Waits;

procedure Main is
   package HC renames CuBit.Handles.Control;
   package ST renames CuBit.Startup_Tables;
   use type ABI.Refusal;
   use type CuBit.Process_IDs.Process_ID;

   Roots : constant CuBit.Handles.Boot_Roots.Launcher_Handles :=
     CuBit.Handles.Boot_Roots.Process_Manager;
   Passed : Boolean := True;

   procedure Say (Text : String) is
      Answer : CuBit.Handles.Test_Consoles.Outcome;
      pragma Unreferenced (Answer);
   begin
      Answer := CuBit.Handles.Test_Consoles.Write
        (Roots.Test_Console_Object, "spawn-rules: " & Text & ASCII.LF);
   end Say;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      Say (Name & (if Condition then " PASS" else " FAIL"));
      Passed := Passed and then Condition;
   end Check;

   ---------------------------------------------------------------------------
   --  A minimal ELF64 executable: the headers in the first page, one
   --  read-execute PT_LOAD of the second page at Load_Address, and the
   --  PT_GNU_STACK the loader requires. It is spawned suspended and never
   --  runs, so its code page is zeros.
   ---------------------------------------------------------------------------
   Page_Bytes    : constant := 4096;
   Image_Bytes   : constant := 2 * Page_Bytes;
   Load_Address  : constant Unsigned_64 := 16#40_0000#;
   Stack_Bytes   : constant Unsigned_64 := 16 * Page_Bytes;
   Header_Bytes  : constant := 64;
   Program_Header_Bytes : constant := 56;
   PT_LOAD       : constant Unsigned_32 := 1;
   PT_GNU_STACK  : constant Unsigned_32 := 16#6474_E551#;
   PF_X : constant Unsigned_32 := 1;
   PF_W : constant Unsigned_32 := 2;
   PF_R : constant Unsigned_32 := 4;
   ET_EXEC       : constant Unsigned_16 := 2;
   EM_X86_64     : constant Unsigned_16 := 62;

   type Byte_Array is array (Natural range <>) of Unsigned_8;
   Image : Byte_Array (0 .. Image_Bytes - 1) := [others => 0]
     with Alignment => Page_Bytes;

   procedure Put (At_Offset : Natural; Value : Unsigned_64; Bytes : Positive) is
      V : Unsigned_64 := Value;
   begin
      for I in 0 .. Bytes - 1 loop
         Image (At_Offset + I) := Unsigned_8 (V and 16#FF#);
         V := Shift_Right (V, 8);
      end loop;
   end Put;

   procedure Make_Image is
      First_Header : constant := Header_Bytes;
      Second_Header : constant := Header_Bytes + Program_Header_Bytes;
   begin
      --  e_ident: magic, ELFCLASS64, ELFDATA2LSB, EV_CURRENT, ELFOSABI_SYSV.
      Image (0 .. 7) := [16#7F#, Character'Pos ('E'), Character'Pos ('L'),
                         Character'Pos ('F'), 2, 1, 1, 0];
      Put (16, Unsigned_64 (ET_EXEC), 2);
      Put (18, Unsigned_64 (EM_X86_64), 2);
      Put (20, 1, 4);                        --  e_version
      Put (24, Load_Address, 8);             --  e_entry
      Put (32, Header_Bytes, 8);             --  e_phoff
      Put (52, Header_Bytes, 2);             --  e_ehsize
      Put (54, Program_Header_Bytes, 2);     --  e_phentsize
      Put (56, 2, 2);                        --  e_phnum
      --  PT_LOAD: file page 1 at Load_Address, read and execute.
      Put (First_Header, Unsigned_64 (PT_LOAD), 4);
      Put (First_Header + 4, Unsigned_64 (PF_R or PF_X), 4);
      Put (First_Header + 8, Page_Bytes, 8);       --  p_offset
      Put (First_Header + 16, Load_Address, 8);    --  p_vaddr
      Put (First_Header + 24, Load_Address, 8);    --  p_paddr
      Put (First_Header + 32, Page_Bytes, 8);      --  p_filesz
      Put (First_Header + 40, Page_Bytes, 8);      --  p_memsz
      Put (First_Header + 48, Page_Bytes, 8);      --  p_align
      --  PT_GNU_STACK: read-write, no file data.
      Put (Second_Header, Unsigned_64 (PT_GNU_STACK), 4);
      Put (Second_Header + 4, Unsigned_64 (PF_R or PF_W), 4);
      Put (Second_Header + 40, Stack_Bytes, 8);    --  p_memsz
      Put (Second_Header + 48, 16, 8);             --  p_align
   end Make_Image;

   function Spawn (Authority : CuBit.Handles.Process;
                   Parent : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process)
                   return HC.Spawn_Result is
     (HC.Spawn (Authority, Image'Address, Image_Bytes, "spawn-probe", Parent,
                (Kind => ST.Standard_Limit)));

   --  Stop a child the probe spawned, through the root.
   procedure Stop (Result : HC.Spawn_Result) is
      Ignore : Boolean;
   begin
      if Result.Spawned then
         Ignore := HC.Kill (Result.Value.Child, Roots.Process_Root);
      end if;
   end Stop;

   --  Nothing more to do: stay, so devmgr's wait for procmgr is not
   --  disturbed while the test reads the lines above.
   procedure Idle with No_Return is
      Ignore : Unsigned_64;
   begin
      loop
         Ignore := CuBit.Waits.Sleep_For (1000);
      end loop;
   end Idle;

   Execute_Only : constant Rights := (Read | Execute => True, others => False);
begin
   if not Roots.Valid then
      Idle;
   end if;
   Say ("starting");
   Make_Image;

   declare
      --  Rule 1's control case: the root spawns.
      First : constant HC.Spawn_Result := Spawn (Roots.Process_Root);
   begin
      Check (First.Spawned and then Valid (First.Value.Control)
             and then Valid (First.Value.Mailbox) and then Valid (First.Value.Space),
             "the process root spawns, with control, endpoint and CSPACE");
      if not First.Spawned then
         Say ("done FAIL");
         Idle;
      end if;

      --  Rule 1: through the child's Control (every right) it does not.
      declare
         Through_Child : constant HC.Spawn_Result := Spawn (First.Value.Control);
      begin
         Check (not Through_Child.Spawned and then Through_Child.Why = ABI.Not_Permitted,
                "a child's Control does not spawn (rule 1)");
         Stop (Through_Child);
      end;

      --  Rules 2 and 3 with a spawn authority of Read and Execute only.
      declare
         Narrow : constant CuBit.Handles.Process :=
           HC.Derive (Copy (Roots.Process_Root, Execute_Only));
      begin
         Check (Valid (Narrow), "a Read and Execute copy of the root is derived");
         declare
            Second : constant HC.Spawn_Result := Spawn (Narrow);
         begin
            Check (Second.Spawned, "a spawn authority without Grant spawns");
            if Second.Spawned then
               Check (not Valid (Second.Value.Mailbox) and then not Valid (Second.Value.Space),
                      "without Grant: no endpoint and no CSPACE (rule 2)");
               Check (not HC.Kill (Second.Value.Child, Second.Value.Control),
                      "without Write: the child's Control cannot stop it (rule 2)");
            end if;
            Stop (Second);
         end;
         declare
            Adopted : constant HC.Spawn_Result := Spawn (Narrow, Parent => First.Value.Child);
         begin
            Check (not Adopted.Spawned and then Adopted.Why = ABI.Not_Permitted,
                   "without Grant: another process is not named parent (rule 3)");
            Stop (Adopted);
         end;
      end;
      declare
         Granted : constant HC.Spawn_Result := Spawn (Roots.Process_Root, Parent => First.Value.Child);
      begin
         Check (Granted.Spawned, "with Grant over every process: another parent is named");
         Stop (Granted);
      end;
      Stop (First);
   end;
   Say (if Passed then "done PASS" else "done FAIL");
   Idle;
end Main;
