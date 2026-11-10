------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Exceptions; use Ada.Exceptions;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Check_Results; use Check_Results;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Thread_Areas;

package body Exception_Threads is

   package ABI renames CuBit.Kernel_ABI;

   Thread_Error : exception;

   Workers : constant := 2;
   subtype Worker_Index is Positive range 1 .. Workers;
   --  Thread 0 is the main thread.
   subtype Thread_Number is Natural range 0 .. Workers;
   Rounds : constant := 200;
   Stack_Bytes : constant := 1024 * 1024;
   Stack_Alignment : constant := 16;
   Return_Address_Bytes : constant := 8;

   type Word is mod 2 ** 32 with Atomic, Size => 32;
   --  Cleared and futex-woken by the kernel when the thread ends.
   Exit_Words : array (Worker_Index) of aliased Word := [others => 1];
   --  Handled rounds whose occurrence was the thread's own, per thread.
   Own : array (Thread_Number) of Natural := [others => 0] with Volatile_Components;

   Digit_Zero : constant := Character'Pos ('0');

   function Message_Of (Thread : Thread_Number) return String is
     ("raised on thread " & Character'Val (Digit_Zero + Thread));

   procedure Raise_From (Thread : Thread_Number) is
   begin
      raise Thread_Error with Message_Of (Thread);
   end Raise_From;
   pragma No_Inline (Raise_From);

   procedure Rounds_On (Thread : Thread_Number) is
      Ignore : Unsigned_64;
   begin
      for Round in 1 .. Rounds loop
         begin
            Raise_From (Thread);
         exception
            when E : Thread_Error =>
               --  Let the other threads raise before this one looks.
               Ignore := CuBit.Kernel_Calls.Call (ABI.Yield);
               if Exception_Message (E) = Message_Of (Thread) then
                  Own (Thread) := Own (Thread) + 1;
               end if;
         end;
      end loop;
   end Rounds_On;

   procedure Worker (Argument : Unsigned_64) with Convention => C, No_Return;
   procedure Worker (Argument : Unsigned_64) is
   begin
      Rounds_On (Thread_Number (Argument));
      CuBit.Thread_Areas.Exit_Thread;
   end Worker;

   procedure Run is
      Stacks : array (Worker_Index) of Unsigned_64 := [others => 0];
      Created : array (Worker_Index) of Boolean := [others => False];
      Area : Unsigned_64;
      Ignore : Unsigned_64;
      Seen : Word;
   begin
      for W in Worker_Index loop
         Stacks (W) := CuBit.Kernel_Calls.Call (ABI.Allocate_Owned_Memory, Stack_Bytes);
         Area := (if Stacks (W) = 0 then CuBit.Thread_Areas.No_Area else CuBit.Thread_Areas.Create);
         if Area /= CuBit.Thread_Areas.No_Area then
            if CuBit.Kernel_Calls.Call
                 (ABI.Thread_Create, Unsigned_64 (To_Integer (Worker'Address)),
                  ((Stacks (W) + Stack_Bytes) and not (Stack_Alignment - 1)) - Return_Address_Bytes,
                  Unsigned_64 (W), Area,
                  Unsigned_64 (To_Integer (Exit_Words (W)'Address))) = ABI.Failed
            then
               CuBit.Thread_Areas.Discard (Area);
            else
               Created (W) := True;
            end if;
         end if;
      end loop;
      Rounds_On (0);
      for W in Worker_Index loop
         if Created (W) then
            loop
               Seen := Exit_Words (W);
               exit when Seen = 0;
               Ignore := CuBit.Kernel_Calls.Call
                 (ABI.Futex_Wait, Unsigned_64 (To_Integer (Exit_Words (W)'Address)),
                  Unsigned_64 (Seen), ABI.Forever);
            end loop;
         end if;
         if Stacks (W) /= 0 then
            Ignore := CuBit.Kernel_Calls.Call (ABI.Release_Owned_Memory, Stacks (W), Stack_Bytes);
         end if;
      end loop;
      Check ((for all W in Worker_Index => Created (W))
             and then (for all T in Thread_Number => Own (T) = Rounds),
             "three threads raise and handle at once, each sees its own occurrence ("
             & Own (0)'Image & Own (1)'Image & Own (2)'Image & " of" & Rounds'Image & ")");
   end Run;

end Exception_Threads;
