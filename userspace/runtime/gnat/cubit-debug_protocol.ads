------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The cooperative debug protocol (DBG-001 preview): between a target that
--  links CuBit.Debug_Stub and a debugger (CuBit.Debug).
--
--  @description
--  The target registers the debug-target service role (Service_Role). A
--  debugger holding (service "debug-target" ...) calls it. Every grant the
--  stub makes is READ-ONLY, of its own pages, to the process whose call it
--  is answering (the kernel's current-reply rule), so no write path to the
--  target exists and the target consented by linking the stub.
--
--  Op_Attach: the reply's word 0 is the encoded grant of the state page
--  (State_Page layout below), word 1 the target's identity word.
--  Op_Share: words 0 and 1 are a page-aligned address and a page count
--  (at most Share_Pages_Ceiling); the reply's word 0 is the encoded grant,
--  word 1 the pages granted. The kernel refuses pages the target does not
--  own or has not mapped.
--  Op_Pause: word 0 is how long to wait for the threads to park, in ms (at
--  most Pause_Wait_Ceiling_Ms). Reply_Ok: all parked, word 0 the new stop
--  epoch; Reply_Not_Yet: some did not park in time (they park later).
--  Op_Resume releases them. Op_Detach revokes the caller's grants.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package CuBit.Debug_Protocol with Pure, SPARK_Mode is

   --  native-runtime-services.ccl: (service debug-target 47 read-write).
   Service_Role : constant := 47;

   Op_Attach : constant Unsigned_32 := 16#0D01#;
   Op_Share  : constant Unsigned_32 := 16#0D02#;
   Op_Pause  : constant Unsigned_32 := 16#0D03#;
   Op_Resume : constant Unsigned_32 := 16#0D04#;
   Op_Detach : constant Unsigned_32 := 16#0D05#;

   Reply_Ok      : constant Unsigned_32 := 16#F000#;
   Reply_Not_Yet : constant Unsigned_32 := 16#F001#;
   Reply_Refused : constant Unsigned_32 := 16#F002#;

   Page_Bytes : constant := 4096;
   Share_Pages_Ceiling : constant := 256;
   Pause_Wait_Ceiling_Ms : constant := 2_000;
   --  Grants one debugger may hold from one target at once.
   Grants_Per_Debugger_Ceiling : constant := 512;

   ---------------------------------------------------------------------------
   --  The state page (byte offsets, little-endian). The target writes it;
   --  each thread record is guarded by its own sequence word (odd while the
   --  thread writes it).
   ---------------------------------------------------------------------------
   Magic : constant Unsigned_32 := 16#4742_4443#;   --  "CDBG"
   Version : constant Unsigned_16 := 1;

   Magic_At          : constant := 0;     --  u32
   Version_At        : constant := 4;     --  u16
   Thread_Count_At   : constant := 6;     --  u16, threads registered
   Process_At        : constant := 8;     --  u64, the target's identity word
   Checkpoints_At    : constant := 16;    --  u64, checkpoints passed
   Stop_Epoch_At     : constant := 24;    --  u64, bumped at each full stop
   Pause_Requested_At : constant := 32;   --  u32, 1 while a pause is asked
   Parked_Count_At   : constant := 36;    --  u32, threads parked now
   Build_Id_Length_At : constant := 40;   --  u32
   Build_Id_At       : constant := 44;    --  Build_Id_Ceiling bytes
   Build_Id_Ceiling  : constant := 20;
   Name_At           : constant := 64;    --  Name_Bytes bytes, space padded
   Name_Bytes        : constant := 32;
   Image_Low_At      : constant := 96;    --  u64, the ELF image's first byte
   Image_High_At     : constant := 104;   --  u64, past its last loaded byte
   Threads_At        : constant := 128;

   Thread_Ceiling : constant := 16;
   Thread_Bytes   : constant := 128;
   --  Within a thread record:
   T_Id_At         : constant := 0;     --  u32, 1-based registration order
   T_State_At      : constant := 4;     --  u32, Thread_Running .. Thread_Ended
   T_Sequence_At   : constant := 8;     --  u64, odd while being written
   T_Checkpoints_At : constant := 16;   --  u64
   T_Name_At       : constant := 24;    --  T_Name_Bytes, space padded
   T_Name_Bytes    : constant := 24;
   T_Stack_Low_At  : constant := 48;    --  u64
   T_Stack_High_At : constant := 56;    --  u64
   --  The registers at the checkpoint's capture point, inside
   --  CuBit.Debug_Stub.Checkpoint (its CFI unwinds from there).
   T_RIP_At : constant := 64;
   T_RSP_At : constant := 72;
   T_RBP_At : constant := 80;
   T_RBX_At : constant := 88;
   T_R12_At : constant := 96;
   T_R13_At : constant := 104;
   T_R14_At : constant := 112;
   T_R15_At : constant := 120;

   Thread_Running : constant Unsigned_32 := 0;
   Thread_Parked  : constant Unsigned_32 := 1;
   Thread_Ended   : constant Unsigned_32 := 2;

   pragma Compile_Time_Error
     (Threads_At + Thread_Ceiling * Thread_Bytes > Page_Bytes,
      "the debug state page must hold every thread record");
end CuBit.Debug_Protocol;
