------------------------------------------------------------------------------
--  CuBit Debug_Observe check (DBG-001, docs/debugging.md; Debug_ABI)
--
--  Runs in procmgr's place (the headless debug-observe test puts it in the
--  initrd as procmgr.svc), so it holds the wildcard process root and the
--  test console object. It spawns a target of its own making: a page of
--  code that sets RBX, RBP and R12-R15 to known values and then sleeps
--  forever in a futex Wait on a word of its own code page. Then, through
--  the kernel's debug sessions:
--  * minting: the target's Control mints each kind; a copy of the root
--    without Grant does not; the caller itself is not a target;
--  * Threads: one thread, waiting on a futex with no deadline;
--  * Registers: the blocked thread's callee-saved registers are exact (the
--    SYSCALL entry saves them), its RIP follows the SYSCALL, the frame is a
--    system-call frame for call 149; its XSAVE image has the reset MXCSR;
--  * views: the target's code page maps read-only into this session's slice
--    and shows the marker; a second map there is Occupied; an unmapped page
--    is refused; the kernel refuses the aperture as a source and as a
--    destination; Unmap then lets the page map again;
--  * the other three kinds answer Not_Yet, but Describe and Detach;
--  * Revoke ends a session (its handle answers Revoked) and removes its
--    views; killing the target ends its sessions.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with Debug_ABI;
with CuBit.Handles; use CuBit.Handles;
with CuBit.Handles.Boot_Roots;
with CuBit.Handles.Control;
with CuBit.Handles.Debug;
with CuBit.Handles.Test_Consoles;
with CuBit.Process_Calls;
with CuBit.Process_IDs;
with CuBit.Startup_Tables;
with CuBit.Waits;

procedure Main is
   package HC renames CuBit.Handles.Control;
   package HD renames CuBit.Handles.Debug;
   package D renames Debug_ABI;
   package ST renames CuBit.Startup_Tables;
   use type D.Debug_Kind;

   Roots : constant CuBit.Handles.Boot_Roots.Launcher_Handles :=
     CuBit.Handles.Boot_Roots.Process_Manager;
   Passed : Boolean := True;

   procedure Say (Text : String) is
      Answer : CuBit.Handles.Test_Consoles.Outcome;
      pragma Unreferenced (Answer);
   begin
      Answer := CuBit.Handles.Test_Consoles.Write
        (Roots.Test_Console_Object, "debug-observe: " & Text & ASCII.LF);
   end Say;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      Say (Name & (if Condition then " PASS" else " FAIL"));
      Passed := Passed and then Condition;
   end Check;

   function Hex (Value : Unsigned_64) return String is
      Digit_Text : constant String := "0123456789abcdef";
      Result : String (1 .. 16);
      V : Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Digit_Text (Natural (V and 15) + 1);
         V := Shift_Right (V, 4);
      end loop;
      return Result;
   end Hex;

   ---------------------------------------------------------------------------
   --  The target: an ELF64 executable whose second page is its code (one
   --  read-execute PT_LOAD at Load_Address) and PT_GNU_STACK.
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

   --  Inside the code page.
   Marker_Offset : constant := 16#800#;
   Word_Offset   : constant := 16#900#;
   Marker        : constant Unsigned_64 := 16#4B52_414D_4B47_4244#;   --  "DBGKMARK"
   Futex_Value   : constant := 16#2A#;
   Wait_Call     : constant := 149;   --  Wait_ABI.System_Call_Number
   Futex_Form    : constant := 8;     --  Wait_ABI.Futex_Sources
   After_Syscall : constant := 92;    --  the JMP after the SYSCALL

   type Register_Values is array (1 .. 6) of Unsigned_64;
   --  RBX, RBP, R12, R13, R14, R15.
   Expected : constant Register_Values :=
     [16#1111_1111_1111_1111#, 16#2222_2222_2222_2222#, 16#3333_3333_3333_3333#,
      16#4444_4444_4444_4444#, 16#5555_5555_5555_5555#, 16#6666_6666_6666_6666#];

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
      Code : constant := Page_Bytes;
      --  REX.W MOV r64, imm64 for RBX, RBP, R12 .. R15 (10 bytes each).
      Opcodes : constant array (1 .. 6) of Unsigned_64 :=
        [16#BB48#, 16#BD48#, 16#BC49#, 16#BD49#, 16#BE49#, 16#BF49#];
      P : Natural := Code;
   begin
      Image (0 .. 7) := [16#7F#, Character'Pos ('E'), Character'Pos ('L'),
                         Character'Pos ('F'), 2, 1, 1, 0];
      Put (16, Unsigned_64 (ET_EXEC), 2);
      Put (18, Unsigned_64 (EM_X86_64), 2);
      Put (20, 1, 4);
      Put (24, Load_Address, 8);
      Put (32, Header_Bytes, 8);
      Put (52, Header_Bytes, 2);
      Put (54, Program_Header_Bytes, 2);
      Put (56, 2, 2);
      Put (First_Header, Unsigned_64 (PT_LOAD), 4);
      Put (First_Header + 4, Unsigned_64 (PF_R or PF_X), 4);
      Put (First_Header + 8, Page_Bytes, 8);
      Put (First_Header + 16, Load_Address, 8);
      Put (First_Header + 24, Load_Address, 8);
      Put (First_Header + 32, Page_Bytes, 8);
      Put (First_Header + 40, Page_Bytes, 8);
      Put (First_Header + 48, Page_Bytes, 8);
      Put (Second_Header, Unsigned_64 (PT_GNU_STACK), 4);
      Put (Second_Header + 4, Unsigned_64 (PF_R or PF_W), 4);
      Put (Second_Header + 40, Stack_Bytes, 8);
      Put (Second_Header + 48, 16, 8);

      for R in Opcodes'Range loop
         Put (P, Opcodes (R), 2);
         Put (P + 2, Expected (R), 8);
         P := P + 10;
      end loop;
      --  P = Code + 60: the loop.
      Put (P, 16#B8#, 1); Put (P + 1, Wait_Call, 4);            --  mov eax, 149
      Put (P + 5, 16#BF#, 1); Put (P + 6, Futex_Form, 4);       --  mov edi, 8
      Put (P + 10, 16#C6C748#, 3); Put (P + 13, 16#FFFF_FFFF#, 4);  --  mov rsi, -1 (Forever)
      --  lea rdx, [rip + disp32]: the next instruction is at Code + 84.
      Put (P + 17, 16#158D48#, 3); Put (P + 20, Word_Offset - 84, 4);
      Put (P + 24, 16#BA41#, 2); Put (P + 26, Futex_Value, 4);  --  mov r10d, 0x2A
      Put (P + 30, 16#050F#, 2);                                --  syscall
      Put (P + 32, 16#DEEB#, 2);                                --  jmp back 34 bytes
      Put (Code + Marker_Offset, Marker, 8);
      Put (Code + Word_Offset, Futex_Value, 4);
   end Make_Image;

   procedure Idle with No_Return is
      Ignore : Unsigned_64;
   begin
      loop
         Ignore := CuBit.Waits.Sleep_For (1000);
      end loop;
   end Idle;

   procedure Pause (Milliseconds : Unsigned_64) is
      Ignore : Unsigned_64;
   begin
      Ignore := CuBit.Waits.Sleep_For (Milliseconds);
   end Pause;

   --  Thread entries (Debug_ABI.Thread_Entry_Bytes each).
   type Entry_Words is array (0 .. 5) of Unsigned_64;
   Entries : array (0 .. 3) of Entry_Words := [others => [others => 0]];
   General : array (0 .. D.General_Words - 1) of Unsigned_64 := [others => 0];
   Vector  : Byte_Array (0 .. 4095) := [others => 0] with Alignment => 64;

   function Reg (R : D.General_Register) return Unsigned_64 is
     (General (D.General_Register'Pos (R)));

   function Reason (E : Entry_Words) return Unsigned_64 is (Shift_Right (E (0), 16) and 16#FF#);
   function Slot (E : Entry_Words) return Unsigned_16 is (Unsigned_16 (E (0) and 16#FFFF#));
   function Generation (E : Entry_Words) return Unsigned_32 is (Unsigned_32 (Shift_Right (E (0), 32)));

   function Reason_Word (R : D.Wait_Reason) return Unsigned_64 is
     (Unsigned_64 (D.Wait_Reason'Enum_Rep (R)));

   Execute_Only : constant Rights := (Read | Execute => True, others => False);
begin
   if not Roots.Valid then
      Idle;
   end if;
   Say ("starting");
   Make_Image;

   declare
      Spawned : constant HC.Spawn_Result :=
        HC.Spawn (Roots.Process_Root, Image'Address, Image_Bytes, "debug-target",
                  CuBit.Process_IDs.No_Process, (Kind => ST.Standard_Limit));
   begin
      Check (Spawned.Spawned, "the target spawns");
      if not Spawned.Spawned then
         Say ("done FAIL");
         Idle;
      end if;
      declare
         Child : constant CuBit.Process_IDs.Process_ID := Spawned.Value.Child;
         Control : constant CuBit.Handles.Process := Spawned.Value.Control;
         Observe : constant HD.Mint_Result := HD.Mint (Control, D.Observe);
         S : HD.Session;
         R : HD.Reply;
         Waiting : Boolean := False;
         Slice : Unsigned_64 := 0;
      begin
         Check (HC.Resume (Child, Control, 0, 0), "the target resumes");
         Check (Observe.Minted, "the target's Control mints Debug_Observe");
         if not Observe.Minted then
            Say ("done FAIL");
            Idle;
         end if;
         S := Observe.Handle;

         R := HD.Call (S, D.Describe);
         Check (HD.Outcome_Is (R, D.Done) and then R.W1 = D.Kind_Word (D.Observe)
                and then R.W2 = CuBit.Process_IDs.To_Word (Child),
                "Describe names the kind and the target's identity");
         Slice := R.W3;
         Check (Slice >= D.Aperture_First and then Slice < D.Aperture_Limit
                and then (Slice - D.Aperture_First) mod D.Slice_Bytes = 0,
                "the session's slice lies in the view aperture");

         --  Threads, until the target sleeps in its futex Wait.
         for Attempt in 1 .. 200 loop
            R := HD.Threads (S, Entries'Address, Entries'Size / 8);
            Waiting := HD.Outcome_Is (R, D.Done) and then R.W1 = 1
              and then Reason (Entries (0)) = Reason_Word (D.Futex);
            exit when Waiting;
            Pause (10);
         end loop;
         Check (Waiting and then R.W2 = 1 and then Slot (Entries (0)) = 0
                and then Entries (0) (4) = D.No_Deadline,
                "Threads: one thread, in a futex wait with no deadline");

         --  Registers of the blocked thread.
         R := HD.Registers
           (S, D.Selector (Slot (Entries (0)), D.General, Generation (Entries (0))),
            General'Address, General'Size / 8);
         Check (HD.Outcome_Is (R, D.Done) and then R.W1 = D.General_Bytes,
                "Registers: the general block is written");
         Check (Reg (D.RBX) = Expected (1) and then Reg (D.RBP) = Expected (2)
                and then Reg (D.R12) = Expected (3) and then Reg (D.R13) = Expected (4)
                and then Reg (D.R14) = Expected (5) and then Reg (D.R15) = Expected (6),
                "Registers: a blocked thread's callee-saved registers are exact");
         Check (Reg (D.RIP) = Load_Address + After_Syscall
                and then Reg (D.RAX) = Wait_Call and then Reg (D.RDI) = Futex_Form
                and then Reg (D.RSI) = Unsigned_64'Last
                and then Reg (D.RDX) = Load_Address + Word_Offset
                and then Reg (D.R10) = Futex_Value,
                "Registers: RIP follows the SYSCALL, and its arguments are kept");
         Check (Reg (D.Saved_Frame) = Unsigned_64 (D.Frame_Kind'Enum_Rep (D.System_Call))
                and then Reg (D.Frame_Detail) = Wait_Call
                and then Reg (D.CS) = (32 or 3) and then Reg (D.SS) = (24 or 3),
                "Registers: a system-call frame, with the user selectors");
         Say ("rbx 0x" & Hex (Reg (D.RBX)) & " r15 0x" & Hex (Reg (D.R15)) &
              " rip 0x" & Hex (Reg (D.RIP)) & " rsp 0x" & Hex (Reg (D.RSP)));
         R := HD.Registers
           (S, D.Selector (Slot (Entries (0)), D.Vector, Generation (Entries (0))),
            Vector'Address, Vector'Length);
         Check (HD.Outcome_Is (R, D.Done) and then R.W1 = 4096
                and then Vector (24) = 16#80# and then Vector (25) = 16#1F#,
                "Registers: the XSAVE image, with the reset MXCSR");
         R := HD.Registers
           (S, D.Selector (Slot (Entries (0)), D.General, Generation (Entries (0)) + 1),
            General'Address, General'Size / 8);
         Check (HD.Outcome_Is (R, D.No_Such_Thread), "Registers: a stale thread generation is refused");

         --  Views.
         declare
            Mapped_Word : Unsigned_64 with Import, Volatile,
              Address => To_Address (Integer_Address (Slice + Marker_Offset));
            Probe_Text : String (1 .. 8) with Import,
              Address => To_Address (Integer_Address (Slice + Marker_Offset));
            Written : Unsigned_64;
         begin
            R := HD.Map_View (S, Load_Address, Slice, 1);
            Check (HD.Outcome_Is (R, D.Done) and then R.W1 = 1,
                   "Map_View maps the target's code page");
            if HD.Outcome_Is (R, D.Done) and then R.W1 = 1 then
               Check (Mapped_Word = Marker, "the view shows the target's bytes");
               R := HD.Map_View (S, Load_Address, Slice, 1);
               Check (HD.Outcome_Is (R, D.Occupied), "a second view on one page is Occupied");
               --  The kernel never reaches the aperture: as a source (a
               --  console write of the view's bytes) or a destination.
               Written := CuBit.Process_Calls.Write_Console
                 (To_Address (Integer_Address (Slice + Marker_Offset)), Probe_Text'Length);
               Check (Written = 0, "the kernel refuses to read the aperture");
               R := HD.Threads (S, To_Address (Integer_Address (Slice)), 64);
               Check (HD.Outcome_Is (R, D.Destination_Unwritable),
                      "the kernel refuses to write the aperture");
               R := HD.Unmap_View (S, Slice, 1);
               Check (HD.Outcome_Is (R, D.Done) and then R.W1 = 1, "Unmap_View removes the view");
               R := HD.Map_View (S, Load_Address, Slice, 1);
               Check (HD.Outcome_Is (R, D.Done) and then R.W1 = 1, "the page maps again after Unmap");
            end if;
            R := HD.Map_View (S, Load_Address + 64 * Page_Bytes, Slice + Page_Bytes, 1);
            Check (HD.Outcome_Is (R, D.Done) and then R.W1 = 0
                   and then R.W2 = Unsigned_64 (D.Map_Refusal'Enum_Rep (D.Unmapped)),
                   "Map_View refuses a page the target does not map");
            R := HD.Map_View (S, Load_Address, Slice + D.Slice_Bytes, 1);
            Check (HD.Outcome_Is (R, D.Outside_Slice), "Map_View refuses another session's slice");
            R := HD.Map_View (S, 16#FFFF_8000_0000_0000#, Slice + Page_Bytes, 1);
            Check (HD.Outcome_Is (R, D.Malformed), "Map_View refuses a kernel-half target");
         end;

         --  The other kinds.
         for K in D.Pause .. D.Instrument loop
            declare
               Other : constant HD.Mint_Result := HD.Mint (Control, K);
            begin
               Check (Other.Minted, "the target's Control mints " & K'Image);
               if Other.Minted then
                  R := HD.Threads (Other.Handle, Entries'Address, Entries'Size / 8);
                  Check (HD.Outcome_Is (R, D.Not_Yet), K'Image & " Threads answers Not_Yet");
                  R := HD.Call (Other.Handle, D.Describe);
                  Check (HD.Outcome_Is (R, D.Done) and then R.W1 = D.Kind_Word (K),
                         K'Image & " Describe answers");
                  R := HD.Call (Other.Handle, D.Detach);
                  Check (HD.Outcome_Is (R, D.Done), K'Image & " detaches");
                  R := HD.Call (Other.Handle, D.Describe);
                  Check (HD.Outcome_Is (R, D.Revoked), K'Image & " is revoked after Detach");
               end if;
            end;
         end loop;

         --  Who may mint.
         declare
            Narrow : constant CuBit.Handles.Process :=
              HC.Derive (Copy (Roots.Process_Root, Execute_Only));
            Refused : constant HD.Mint_Result := HD.Mint (Narrow, D.Observe, Child);
            Self : constant HD.Mint_Result :=
              HD.Mint (Roots.Process_Root, D.Observe, CuBit.Process_IDs.No_Process);
         begin
            Check (not Refused.Minted
                   and then Refused.Why_Word = D.Outcome_Word_Of (D.Not_Permitted),
                   "a Control without Grant does not mint");
            Check (not Self.Minted, "a stale or absent identity mints nothing");
         end;

         --  Revoke: the handle goes stale and its views go.
         declare
            Again : HD.Mint_Result;
         begin
            Check (HD.Revoke (Control, Observe.Session_Word) = D.Outcome_Word_Of (D.Done),
                   "Revoke_Debug through the Control");
            R := HD.Threads (S, Entries'Address, Entries'Size / 8);
            Check (HD.Outcome_Is (R, D.Revoked), "a revoked session answers Revoked");
            Again := HD.Mint (Control, D.Observe);
            if Again.Minted then
               R := HD.Call (Again.Handle, D.Describe);
               if R.W3 = Slice then
                  R := HD.Map_View (Again.Handle, Load_Address, Slice, 1);
                  Check (HD.Outcome_Is (R, D.Done) and then R.W1 = 1,
                         "Revoke removed the views (the slice maps again)");
               end if;
               --  Killing the target ends its sessions.
               Check (HC.Kill (Child, Control), "the target is stopped");
               declare
                  Ended : Boolean := False;
               begin
                  for Attempt in 1 .. 200 loop
                     R := HD.Threads (Again.Handle, Entries'Address, Entries'Size / 8);
                     Ended := HD.Outcome_Is (R, D.Revoked) or else HD.Outcome_Is (R, D.Target_Gone);
                     exit when Ended;
                     Pause (10);
                  end loop;
                  Check (Ended, "a stopped target's session ends");
               end;
            else
               Check (False, "a new session mints after Revoke");
            end if;
         end;
      end;
   end;
   Say (if Passed then "done PASS" else "done FAIL");
   Idle;
end Main;
