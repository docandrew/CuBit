------------------------------------------------------------------------------
--  CuBit capability non-amplification regression app
--
--  This deliberately authorityless process attempts the operations that once
--  made the self CAP_PROCESS an ambient privilege-escalation path.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System;

with CuBit.Messages; use CuBit.Messages;

procedure Main is
   use ASCII;

   CAP_NULL     : constant Unsigned_64 := 0;
   CAP_ENDPOINT : constant Unsigned_64 := 1;
   RIGHT_RW     : constant Unsigned_64 := 3;
   TEST_SLOT    : constant Unsigned_64 := 32;
   KBD_SLOT     : constant Unsigned_64 := 13;
   MOUSE_SLOT   : constant Unsigned_64 := 16;
   PROCESS_SLOT : constant Unsigned_64 := 5;
   ERROR_RESULT : constant Unsigned_64 := Unsigned_64'Last;

   type Inspection_Record is array (Natural range 0 .. 5) of Unsigned_64;
   Inspection : aliased Inspection_Record := [others => 0];

   function Address_Number is new Ada.Unchecked_Conversion
     (System.Address, Unsigned_64);
   function Number_Address is new Ada.Unchecked_Conversion
     (Unsigned_64, System.Address);

   PID    : Unsigned_64;
   Process_Manager_PID : Unsigned_64;
   Result : Unsigned_64;
   Passed : Boolean := True;

   function Syscall_Registers_Preserved
     (Number, Argument : Unsigned_64) return Unsigned_64
     with Import, Convention => C,
          External_Name => "syscall_registers_preserved";

   Endpoint_Slot : CapabilitySlot;
   Endpoint_Found : Boolean;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("capability-test: " & Name & " PASS" & LF);
      else
         debugPrint ("capability-test: " & Name & " FAIL" & LF);
         Passed := False;
      end if;
   end Check;

   procedure Check_Empty_Slot (Slot : Unsigned_64; Name : String) is
   begin
      Inspection := [others => 0];
      Result := syscall
        (SYSCALL_INSPECT_CAPABILITY, PID, Slot,
         Address_Number (Inspection'Address));
      Check
        (Result = 1 and then Inspection (0) = CAP_NULL,
         Name);
   end Check_Empty_Slot;

   procedure Check_Heap_Admission is
      Before : constant Unsigned_64 := syscall (SYSCALL_SBRK, 0);
      Allocation : Unsigned_64;
      Zeroed : Boolean := True;
      type Page_Bytes is array (Natural range 0 .. 8191) of Unsigned_8;
   begin
      Check (Before /= ERROR_RESULT, "heap query");
      Allocation := syscall (SYSCALL_SBRK, Unsigned_64'Last);
      Check (Allocation = ERROR_RESULT and then syscall (SYSCALL_SBRK, 0) = Before,
             "heap wrapping request rejected without growth");
      --  The 16 MiB stack fixture has at most 8192 tracked frames, including
      --  its ELF. This must reject BEFORE allocating anything, not panic in
      --  LinkedLists or return a partially advanced break as success.
      for Attempt in 1 .. 2 loop
         Allocation := syscall (SYSCALL_SBRK, 64 * 1024 * 1024);
         Check (Allocation = ERROR_RESULT and then syscall (SYSCALL_SBRK, 0) = Before,
                "heap oversized request rejected without growth");
      end loop;
      Allocation := syscall (SYSCALL_SBRK, 8192);
      Check (Allocation = Before and then syscall (SYSCALL_SBRK, 0) = Before + 8192,
             "heap usable after rejected requests");
      if Allocation /= ERROR_RESULT then
         declare
            Bytes : Page_Bytes with Import, Address => Number_Address (Allocation);
         begin
            for I in Bytes'Range loop
               Zeroed := Zeroed and then Bytes (I) = 0;
               Bytes (I) := Unsigned_8 (I mod 251);
            end loop;
            Check (Zeroed and then Bytes (8191) = Unsigned_8 (8191 mod 251),
                   "heap admitted pages zeroed and writable");
         end;
      end if;
   end Check_Heap_Admission;

   procedure Check_Stack_Growth is
      type Stack_Bytes is array (Natural range 0 .. 65_535) of Unsigned_8;
      Bytes : Stack_Bytes with Volatile;
      Correct : Boolean := True;
   begin
      -- Touch each page, then revisit it. Volatile keeps the compiler from
      -- replacing this with a scalar and bypassing real user page faults.
      for Page in 0 .. 15 loop
         Bytes (Page * 4096) := Unsigned_8 (Page + 1);
      end loop;
      for Page in 0 .. 15 loop
         Correct := Correct and then Bytes (Page * 4096) = Unsigned_8 (Page + 1);
      end loop;
      Check (Correct, "stack demand pages writable and retained");
   end Check_Stack_Growth;
begin
   Check_Stack_Growth;
   Check_Heap_Admission;
   Check (Syscall_Registers_Preserved (SYSCALL_GETPID, 0) = 1,
          "syscall registers normal return");
   Check (Syscall_Registers_Preserved (Unsigned_64'Last, 0) = 1,
          "syscall registers unknown call");
   Check (Syscall_Registers_Preserved (SYSCALL_SPAWN, 0) = 1,
          "syscall registers denied call");
   Check (Syscall_Registers_Preserved (SYSCALL_SLEEP, 10) = 1,
          "syscall registers blocking return");

   PID := syscall (SYSCALL_GETPID);
   Check (PID > 0 and then PID /= ERROR_RESULT, "getpid");

   --  The all-ones sentinel cannot name a kernel process. Exhausting the
   --  table exercises the optimized syscall loop that froze procmgr at launch.
   Find_Endpoint_Capability (ERROR_RESULT, Endpoint_Slot, Endpoint_Found);
   Check (not Endpoint_Found, "endpoint scan completes");

   Inspection := [others => 0];
   Result := syscall
     (SYSCALL_INSPECT_CAPABILITY, PID, CAP_SLOT_FS,
      Address_Number (Inspection'Address));
   Check
     (Result = 1 and then Inspection (0) = CAP_NULL,
      "no ambient filesystem");

   Inspection := (others => 0);
   Result := syscall
     (SYSCALL_INSPECT_CAPABILITY, PID, CAP_SLOT_SELF_PROC,
      Address_Number (Inspection'Address));
   Check
     (Result = 1 and then Inspection (1) = RIGHT_RW,
      "self process rights attenuated");

   Check_Empty_Slot (KBD_SLOT, "no ambient keyboard");
   Check_Empty_Slot (MOUSE_SLOT, "no ambient mouse");
   Check_Empty_Slot (PROCESS_SLOT, "no ambient process management");

   --  An endpoint/notification grant, not hardware IRQ ownership or a
   --  caller-spelled PID, is required to publish an unsolicited event.
   Process_Manager_PID :=
     getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_PROCMGR);
   Result := syscall
     (SYSCALL_SEND_EVENT, Process_Manager_PID, 0, 0, 0, 0, 0);
   Check (Result = ERROR_RESULT, "ambient event publication denied");

   --  Deliberately invoke the retired ABI number, not a runtime alias.
   --  Unknown syscalls return zero; the old unchecked submit returned one.
   Result := syscall (23, Process_Manager_PID, 0, 0, 0, 0,
                      NO_COMPLETION_TOKEN);
   Check (Result = 0, "retired PID submit rejected");
   Check (not capSubmit (TEST_SLOT, NULL_MESSAGE, NO_COMPLETION_TOKEN),
          "authorityless capability submit rejected");

   Result := syscall
     (SYSCALL_POLICY_MINT_CAPABILITY, PID, CAP_ENDPOINT, PID, 0, RIGHT_RW,
      TEST_SLOT);
   Check (Result = ERROR_RESULT, "self mint denied");

   Inspection := (others => 0);
   Result := syscall
     (SYSCALL_INSPECT_CAPABILITY, PID, TEST_SLOT,
      Address_Number (Inspection'Address));
   Check
     (Result = 1 and then Inspection (0) = CAP_NULL,
      "mint denial leaves slot empty");

   --  A null ELF is never dereferenced because authority admission precedes
   --  ELF validation. An authorityless process cannot spawn another process.
   Result := syscall (SYSCALL_SPAWN);
   Check (Result = ERROR_RESULT, "ambient spawn denied");

   if Passed then
      debugPrint ("capability-test: all tests passed" & LF);
   else
      debugPrint ("capability-test: test failure" & LF);
   end if;

   loop
      Result := syscall (SYSCALL_SLEEP, 1_000);
   end loop;
end Main;
