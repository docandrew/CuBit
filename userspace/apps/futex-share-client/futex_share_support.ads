------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  futex-share-client's threads, atomics and shared state (IPC-010).
--
--  @description
--  Library-level, so thread entry points need no trampolines. Thread
--  bodies avoid the secondary stack and printing: only the main thread
--  prints. Shared words live in the two pages this process grants the
--  server (the layout is the server's).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Address_To_Access_Conversions;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Grant_References;
with CuBit.Waits;

package Futex_Share_Support is

   package W renames CuBit.Waits;
   package GR renames CuBit.Grant_References;

   SYSCALL_THREAD_CREATE : constant Unsigned_64 := 90;
   SYSCALL_THREAD_EXIT   : constant Unsigned_64 := 91;

   type Word is mod 2 ** 32 with Atomic, Size => 32;
   package Words is new System.Address_To_Access_Conversions (Word);
   subtype Word_Access is Words.Object_Pointer;

   --  The first page's words (the server's layout) and the second's.
   Ping_At    : constant := 0;
   Pong_At    : constant := 64;
   Fifo_At    : constant := 128;
   Taken_At   : constant := 192;
   Revoked_At : constant := 256;
   Doomed_At  : constant := 0;

   --  The two granted pages, and their grants (set by the main program).
   First_Base, Second_Base : System.Address := System.Null_Address;
   First, Second : GR.Reference;

   function At_Offset (Base : System.Address; Offset : Natural) return Word_Access is
     (Words.To_Pointer (Base + Storage_Offset (Offset)));

   function Fetch_Add (Target : Word_Access; Value : Unsigned_32) return Unsigned_32;

   --  Threads.
   Stack_Size : constant := 16 * 1024;
   function New_Stack return Unsigned_64;
   type Entry_Point is access procedure (Argument : Unsigned_64)
     with Convention => C;
   function Spawn (Code : Entry_Point; Stack, Argument : Unsigned_64;
                   Exit_Word : Word_Access) return Unsigned_64;
   procedure Thread_Exit;
   --  The kernel clears the exit word, then wakes it, when the thread ends.
   procedure Join (Exit_Word : Word_Access);
   CREATE_FAILED : constant Unsigned_64 := Unsigned_64'Last;

   --  FIFO: waiter I waits on the shared Fifo word, and records its turn.
   Fifo_Waiters : constant := 8;
   type Fifo_Index is range 1 .. Fifo_Waiters;
   Order : array (Fifo_Index) of aliased Word := [others => 0];
   Fifo_Done : array (Fifo_Index) of aliased Word := [others => 1];
   Fifo_Failed : aliased Word := 0;
   Fifo_Deadline : Unsigned_64 := 0 with Volatile;
   procedure Fifo_Waiter (Argument : Unsigned_64) with Convention => C;

   --  Revocation: a thread of the owner waits on the second page's word.
   Doomed_Result : aliased Word := 0;
   Doomed_Done : aliased Word := 1;
   Doomed_Deadline : Unsigned_64 := 0 with Volatile;
   procedure Doomed_Waiter (Argument : Unsigned_64) with Convention => C;

   --  Starvation: CPU-bound bystanders count while Running.
   Maximum_Bystanders : constant := 16;
   type Bystander_Index is range 1 .. Maximum_Bystanders;
   Progress : array (Bystander_Index) of Unsigned_64 := [others => 0]
     with Volatile_Components;
   Running : aliased Word := 0;
   Bystander_Done : array (Bystander_Index) of aliased Word := [others => 1];

   --  Takes this process's kernel notices as they come until Stop is set.
   Notice_Poller_Stop : aliased Word := 0;
   Notice_Poller_Done : aliased Word := 1;
   procedure Notice_Poller (Argument : Unsigned_64) with Convention => C;
   procedure Bystander (Argument : Unsigned_64) with Convention => C;

end Futex_Share_Support;
