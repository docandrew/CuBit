------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The engine's way to a target, native (DBG-001 preview): the cooperative
--  debug-target protocol (CuBit.Debug_Protocol), reads through the target's
--  read-only grants, and the build-id debug file from @system/Debug.
--
--  @description
--  The hosted build has its own body of this spec's operations (a file-
--  backed target for tests); CuBit.Debug is the same on both.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Handles.Debug;

package CuBit.Debug_Target is

   type Byte_Buffer is array (Natural range <>) of Unsigned_8;
   type Buffer_Access is access Byte_Buffer;

   --  Target: an endpoint to the target's debug-target service (the
   --  debugger's (service "debug-target" ...) startup handle), for the
   --  cooperative stub. Session: a Debug_Observe session on the target
   --  (the kernel path, no stub needed); when valid it is used instead.
   --  Filesystem: one that may read @system/Debug/ (the debug file and the
   --  sources).
   type Process_Handle is record
      Target     : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      Session    : CuBit.Handles.Debug.Session;
      Filesystem : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   end record;

   type Status is (Ok, No_Target, Refused, Timed_Out, Not_Found, Malformed);

   type Connection is limited private;
   type Connection_Access is access Connection;

   State_Page_Bytes : constant := 4096;
   subtype State_Page is Byte_Buffer (0 .. State_Page_Bytes - 1);

   --  Where debug files and sources live (docs/image-debug-info.md).
   Debug_Root : constant String := "@system/Debug/";

   --  An absolute deadline Milliseconds from now (monotonic).
   function Deadline_After (Milliseconds : Unsigned_64) return Unsigned_64;

   procedure Connect
     (Handle : Process_Handle; Until_Time : Unsigned_64;
      C : out Connection_Access; Result : out Status);
   procedure Disconnect (C : in out Connection_Access; Until_Time : Unsigned_64);

   --  A copy of the target's state page, as it is now.
   procedure Read_State (C : Connection; Page : out State_Page);

   --  Up to Into'Length bytes at Address; Got: how many could be read (the
   --  rest is not shared, or not mapped in the target).
   procedure Read
     (C : in out Connection; Address : Unsigned_64; Into : out Byte_Buffer; Got : out Natural);

   procedure Pause (C : Connection; Until_Time : Unsigned_64; Epoch : out Unsigned_64; Result : out Status);
   procedure Resume (C : Connection; Until_Time : Unsigned_64; Result : out Status);

   --  A host whose process already holds its one filesystem queue (the
   --  CCL console's places, CCL_Places: the service gives a process one)
   --  lends the engine a reader over it; debug files and sources are then
   --  read through it instead of a queue of the engine's own. Into gets the
   --  bytes from Offset; Got how many (fewer: the file ended); Found False
   --  when the file cannot be read (absent, outside the scope).
   type File_Reader is access procedure
     (Path : String; Offset : Natural; Into : out Byte_Buffer; Got : out Natural; Found : out Boolean);
   procedure Use_File_Reader (Reader : File_Reader);

   --  The debug file for Build_Id (lowercase hex), and its path.
   procedure Load_Debug_File
     (C : Connection; Build_Id : String; Until_Time : Unsigned_64;
      Data : out Buffer_Access; Path : out String; Path_Length : out Natural; Result : out Status);
   --  A source file of that build (File: its name as the line table has it).
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
   Region_Ceiling : constant := 1024;
   Refused_Ceiling : constant := 64;
   type Region is record
      Base   : Unsigned_64 := 0;    --  in the target
      Pages  : Unsigned_64 := 0;
      Mapped : Unsigned_64 := 0;    --  here
   end record;
   type Region_Array is array (1 .. Region_Ceiling) of Region;
   type Refused_Array is array (1 .. Refused_Ceiling) of Unsigned_64;
   --  Kernel views: pages of the target mapped into this session's slice,
   --  a few at a time (View_Cache_Ceiling, reused round-robin).
   View_Cache_Ceiling : constant := 256;
   type View_Slot is record
      Target_Page : Unsigned_64 := 0;
      Mapped      : Boolean := False;
   end record;
   type View_Slots is array (1 .. View_Cache_Ceiling) of View_Slot;
   type Connection is limited record
      Kernel      : Boolean := False;
      Slice       : Unsigned_64 := 0;     --  the session's view slice here
      Target_Id   : Unsigned_64 := 0;
      Views       : View_Slots;
      View_Next   : Positive range 1 .. View_Cache_Ceiling := 1;
      Handle      : Process_Handle;
      State_At    : Unsigned_64 := 0;     --  the state page's mapping here
      Regions     : Region_Array;
      Region_Count : Natural range 0 .. Region_Ceiling := 0;
      Refused     : Refused_Array := [others => Unsigned_64'Last];
      Refused_Next : Positive range 1 .. Refused_Ceiling := 1;
   end record;
end CuBit.Debug_Target;
