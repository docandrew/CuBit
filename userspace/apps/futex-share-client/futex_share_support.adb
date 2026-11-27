pragma Ada_2022;
with System.Machine_Code; use System.Machine_Code;
with Wait_ABI;
with CuBit.Process_Calls;
with CuBit.Process_Events;

package body Futex_Share_Support is

   Ignore : Unsigned_64;

   function Fetch_Add (Target : Word_Access; Value : Unsigned_32) return Unsigned_32 is
      Previous : Unsigned_32 := Value;
   begin
      Asm ("lock xaddl %0, %1",
           Outputs => (Unsigned_32'Asm_Output ("+r", Previous),
                       Word'Asm_Output ("+m", Target.all)),
           Clobber => "memory, cc", Volatile => True);
      return Previous;
   end Fetch_Add;

   --  Stack memory from the heap. The kernel starts a thread with RSP as
   --  given; a SysV function expects RSP + 8 to be 16-byte aligned.
   function New_Stack return Unsigned_64 is
      Base : constant Unsigned_64 := CuBit.Process_Calls.Grow_Heap (Stack_Size);
   begin
      if Base = Unsigned_64'Last or else Base = 0 then
         return 0;
      end if;
      return ((Base + Stack_Size) and not 15) - 8;
   end New_Stack;

   function Spawn (Code : Entry_Point; Stack, Argument : Unsigned_64;
                   Exit_Word : Word_Access) return Unsigned_64 is
     (CuBit.Process_Calls.Create_Thread (Unsigned_64 (To_Integer (Code.all'Address)), Stack, Argument, 0,
                                         Unsigned_64 (To_Integer (Words.To_Address (Exit_Word)))));

   procedure Thread_Exit is
   begin
      CuBit.Process_Calls.Exit_Thread;
   end Thread_Exit;

   procedure Join (Exit_Word : Word_Access) is
      V : Unsigned_32;
   begin
      loop
         V := Unsigned_32 (Exit_Word.all);
         exit when V = 0;
         Ignore := W.Futex_Wait (Words.To_Address (Exit_Word), Unsigned_64 (V), W.Forever);
      end loop;
   end Join;

   procedure Fifo_Waiter (Argument : Unsigned_64) is
      Index : constant Fifo_Index := Fifo_Index (Argument);
      Taken : constant Word_Access := At_Offset (First_Base, Taken_At);
      Result : Unsigned_64;
      Turn : Unsigned_32;
   begin
      --  The word stays 0: only a wake ends this wait.
      Result := W.Futex_Wait (W.Shared_Word (First, Fifo_At), 0, Fifo_Deadline);
      if Result /= Wait_ABI.Futex_Woken then
         Fifo_Failed := 1;
      end if;
      Turn := Fetch_Add (Taken, 1) + 1;
      if Turn in 1 .. Fifo_Waiters then
         Order (Fifo_Index (Turn)) := Word (Index);
      else
         Fifo_Failed := 1;
      end if;
      Thread_Exit;
   end Fifo_Waiter;

   procedure Doomed_Waiter (Argument : Unsigned_64) is
      pragma Unreferenced (Argument);
   begin
      Doomed_Result := Word (W.Futex_Wait (W.Shared_Word (Second, Doomed_At), 0, Doomed_Deadline)
                             mod 2 ** 32);
      Thread_Exit;
   end Doomed_Waiter;

   procedure Notice_Poller (Argument : Unsigned_64) is
      pragma Unreferenced (Argument);
   begin
      while Notice_Poller_Stop = 0 loop
         CuBit.Process_Events.Serve_Defaults;
      end loop;
      Thread_Exit;
   end Notice_Poller;

   procedure Bystander (Argument : Unsigned_64) is
      Index : constant Bystander_Index := Bystander_Index (Argument);
   begin
      while Running /= 0 loop
         Progress (Index) := Progress (Index) + 1;
      end loop;
      Thread_Exit;
   end Bystander;

end Futex_Share_Support;
