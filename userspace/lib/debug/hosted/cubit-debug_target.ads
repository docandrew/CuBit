------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The engine's way to a target, hosted (tests/debug-engine): the target is
--  an unstripped ELF file. Its memory is the file's loaded segments as
--  built (initial values), it has no threads, and its debug file is itself.
--  Same operations as the native unit (userspace/lib/debug/native).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Debug_Target is

   type Byte_Buffer is array (Natural range <>) of Unsigned_8;
   type Buffer_Access is access Byte_Buffer;

   Hosted_Path_Ceiling : constant := 512;
   type Process_Handle is record
      Program : String (1 .. Hosted_Path_Ceiling) := [others => ' '];
      Program_Length : Natural range 0 .. Hosted_Path_Ceiling := 0;
      Source_Root : String (1 .. Hosted_Path_Ceiling) := [others => ' '];
      Source_Root_Length : Natural range 0 .. Hosted_Path_Ceiling := 0;
   end record;
   function Hosted (Program, Source_Root : String) return Process_Handle;

   type Status is (Ok, No_Target, Refused, Timed_Out, Not_Found, Malformed);

   type Connection is limited private;
   type Connection_Access is access Connection;

   State_Page_Bytes : constant := 4096;
   subtype State_Page is Byte_Buffer (0 .. State_Page_Bytes - 1);

   Debug_Root : constant String := "@system/Debug/";

   function Deadline_After (Milliseconds : Unsigned_64) return Unsigned_64;

   procedure Connect
     (Handle : Process_Handle; Until_Time : Unsigned_64;
      C : out Connection_Access; Result : out Status);
   procedure Disconnect (C : in out Connection_Access; Until_Time : Unsigned_64);
   procedure Read_State (C : Connection; Page : out State_Page);
   procedure Read
     (C : in out Connection; Address : Unsigned_64; Into : out Byte_Buffer; Got : out Natural);
   procedure Pause (C : Connection; Until_Time : Unsigned_64; Epoch : out Unsigned_64; Result : out Status);
   procedure Resume (C : Connection; Until_Time : Unsigned_64; Result : out Status);
   procedure Load_Debug_File
     (C : Connection; Build_Id : String; Until_Time : Unsigned_64;
      Data : out Buffer_Access; Path : out String; Path_Length : out Natural; Result : out Status);
   procedure Load_Source
     (C : Connection; Build_Id : String; File : String; Until_Time : Unsigned_64;
      Data : out Buffer_Access; Result : out Status);

   ---------------------------------------------------------------------------
   --  Through the kernel (DBG-001, docs/debugging.md 3.1): a Debug_Observe
   --  session the debugger holds (procmgr minted it through a launch handle
   --  or the session's debug authority). The target needs no stub: its
   --  threads and what each waits on, the registers the kernel saved for a
   --  thread off every CPU, and its memory through read-only views.
   ---------------------------------------------------------------------------
   function Through_Kernel (C : Connection) return Boolean;

   --  Debug_ABI.Wait_Reason, and Not_Known for a code this unit does not know.
   type Wait_Code is
     (Not_Known, Running, Ready, In_Call, Receiving, Completion_Wait, Futex, Sleeping,
      Not_Started, Other);

   type Kernel_Thread is record
      Slot        : Unsigned_16 := 0;
      Generation  : Unsigned_32 := 0;
      Wait        : Wait_Code := Not_Known;
      CPU         : Unsigned_32 := 0;
      Peer        : Unsigned_64 := 0;   --  In_Call: the server's identity
      Since       : Unsigned_64 := 0;   --  In_Call: when the call began (ms)
      Deadline    : Unsigned_64 := Unsigned_64'Last;   --  none: all ones
      Sampled_RIP : Unsigned_64 := 0;   --  at its last timer interrupt
   end record;
   Kernel_Thread_Ceiling : constant := 64;
   subtype Kernel_Thread_Count is Natural range 0 .. Kernel_Thread_Ceiling;
   type Kernel_Thread_Array is array (1 .. Kernel_Thread_Ceiling) of Kernel_Thread;

   --  Up to Kernel_Thread_Ceiling of the target's live threads (Live: how
   --  many it has).
   procedure Kernel_Threads
     (C : Connection; List : out Kernel_Thread_Array; Count : out Kernel_Thread_Count;
      Live : out Natural; Result : out Status);

   --  Debug_ABI's General block: RAX .. R15, RIP, RFLAGS, CS, SS, FS and GS
   --  bases, how they were saved, the call number or vector, the error code.
   General_Register_Words : constant := 32;
   type General_Registers is array (0 .. General_Register_Words - 1) of Unsigned_64;
   type Register_Status is (Saved, On_CPU, Gone, Failed);

   procedure Kernel_Registers
     (C : Connection; Thread : Kernel_Thread; Regs : out General_Registers;
      Result : out Register_Status);

   --  The target's identity word (0 for a cooperative connection).
   function Target_Identity (C : Connection) return Unsigned_64;

   --  Forget the views mapped so far: the next reads see the target's
   --  memory as it is now (a view keeps the bytes it mapped).
   procedure Refresh (C : in out Connection);

private
   Segment_Ceiling : constant := 16;
   type Segment is record
      Address, File_Offset, File_Size, Memory_Size : Unsigned_64 := 0;
   end record;
   type Segment_Array is array (1 .. Segment_Ceiling) of Segment;
   type Connection is limited record
      Handle : Process_Handle;
      Image : Buffer_Access;
      Segments : Segment_Array;
      Segment_Count : Natural range 0 .. Segment_Ceiling := 0;
      State : State_Page := [others => 0];
   end record;
end CuBit.Debug_Target;
