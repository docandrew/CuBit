------------------------------------------------------------------------------
--  ipctest-client's shared channel clients (CAP-002 step 3).
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Deadlines.Clock;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;

package body IPCtest_Channel_Clients is

   package KA renames CuBit.Kernel_ABI;
   package KC renames CuBit.Kernel_Calls;
   use type Clients.Submit_Result, Clients.Wait_Outcome;

   --  Kernel thread calls (docs/threads.md; kernel Syscall.SyscallNumber).
   Thread_Create : constant := 90;
   Thread_Exit   : constant := 91;
   Stack_Size    : constant := 64 * 1024;
   Phase_Ms      : constant := 30_000;
   Create_Failed : constant Unsigned_64 := Unsigned_64'Last;

   procedure Worker (Argument : Unsigned_64) is
      pragma Unreferenced (Argument);
      Answer : IC.Result;
      Number : Clients.Tag;
      Submitted : Clients.Submit_Result;
      Outcome : Clients.Wait_Outcome;
      Deadline : constant CuBit.Deadlines.Deadline := CuBit.Deadlines.Clock.After (Phase_Ms);
      Right : Unsigned_32 := 0;
      Ignore : Unsigned_64;
   begin
      for K in 1 .. Worker_Calls loop
         Clients.Submit (Pool (1), (Value => Worker_Base + Unsigned_64 (K), Kind => IC.Echo), Number,
                         Submitted);
         Outcome := Clients.Ended;
         if Submitted = Clients.Queued then
            Clients.Wait (Pool (1), Number, Deadline, Answer, Outcome);
         end if;
         if Outcome = Clients.Answered
           and then Answer.Value = (Worker_Base + Unsigned_64 (K)) * IC.Multiplier
         then
            Right := Right + 1;
         end if;
      end loop;
      Worker_Right := Word (Right);
      Ignore := KC.Call (Thread_Exit);
   end Worker;

   function Start_Worker return Boolean is
      Base : constant Unsigned_64 := KC.Call (KA.Grow_Heap, Stack_Size);
      Stack : Unsigned_64;
   begin
      if Base = Unsigned_64'Last or else Base = 0 then
         return False;
      end if;
      --  A SysV function expects RSP + 8 to be 16-byte aligned.
      Stack := ((Base + Stack_Size) and not 15) - 8;
      return KC.Call (Thread_Create, Unsigned_64 (To_Integer (Worker'Address)), Stack, 0, 0,
                      Unsigned_64 (To_Integer (Worker_Exit'Address))) /= Create_Failed;
   end Start_Worker;

   procedure Join_Worker is
      V : Unsigned_32;
      Ignore : Unsigned_64;
   begin
      loop
         V := Unsigned_32 (Worker_Exit);
         exit when V = 0;
         Ignore := KC.Call (KA.Futex_Wait, Unsigned_64 (To_Integer (Worker_Exit'Address)),
                            Unsigned_64 (V), Unsigned_64'Last);
      end loop;
   end Join_Worker;

end IPCtest_Channel_Clients;
