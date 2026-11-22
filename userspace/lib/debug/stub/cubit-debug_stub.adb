------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with System.Address_To_Access_Conversions;
with Thread_ABI;
with CuBit.Debug_Protocol;
with CuBit.Grant_References;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Owned_Reservations;
with CuBit.System_Facts;
with CuBit.Thread_Areas;
with CuBit.Waits;

package body CuBit.Debug_Stub is
   package P renames CuBit.Debug_Protocol;
   package GR renames CuBit.Grant_References;
   package ABI renames CuBit.Kernel_ABI;
   package W renames CuBit.Waits;

   Stack_Bytes : constant := 256 * 1024;
   Server_Stack_Bytes : constant := 64 * 1024;
   --  A SysV function expects RSP + 8 to be 16-byte aligned at entry.
   Stack_Alignment : constant := 16;
   Return_Address_Bytes : constant := 8;
   --  A parked thread rechecks the request this often, so a lost wake
   --  costs at most this long.
   Park_Recheck_Ms : constant := 100;
   Pause_Poll_Ms : constant := 1;
   --  The ELF image (static executables load at their link address).
   Image_Base : constant := 16#40_0000#;
   Image_Scan_Bytes : constant := 4096;
   GNU_Build_Id_Note : constant := 3;
   GNU_Note_Name_Bytes : constant := 4;

   type Word64 is mod 2 ** 64 with Atomic, Size => 64;
   type Word32 is mod 2 ** 32 with Atomic, Size => 32;
   type Byte is mod 2 ** 8 with Size => 8;
   package A64 is new System.Address_To_Access_Conversions (Word64);
   package A32 is new System.Address_To_Access_Conversions (Word32);
   package A8 is new System.Address_To_Access_Conversions (Byte);

   State_Base : Unsigned_64 := 0;
   State_Ready : Boolean := False;

   function At_Address (Offset : Natural) return System.Address is
     (To_Address (Integer_Address (State_Base + Unsigned_64 (Offset))));

   procedure Put64 (Offset : Natural; Value : Unsigned_64) is
   begin
      A64.To_Pointer (At_Address (Offset)).all := Word64 (Value);
   end Put64;
   function Get64 (Offset : Natural) return Unsigned_64 is
     (Unsigned_64 (A64.To_Pointer (At_Address (Offset)).all));
   procedure Put32 (Offset : Natural; Value : Unsigned_32) is
   begin
      A32.To_Pointer (At_Address (Offset)).all := Word32 (Value);
   end Put32;
   function Get32 (Offset : Natural) return Unsigned_32 is
     (Unsigned_32 (A32.To_Pointer (At_Address (Offset)).all));
   procedure Put8 (Offset : Natural; Value : Unsigned_8) is
   begin
      A8.To_Pointer (At_Address (Offset)).all := Byte (Value);
   end Put8;

   procedure Put_Text (Offset, Width : Natural; Text : String) is
   begin
      for I in 0 .. Width - 1 loop
         Put8 (Offset + I,
               (if I < Text'Length then Character'Pos (Text (Text'First + I))
                else Character'Pos (' ')));
      end loop;
   end Put_Text;

   ---------------------------------------------------------------------------
   --  Observed threads
   ---------------------------------------------------------------------------
   subtype Thread_Count is Natural range 0 .. P.Thread_Ceiling;
   subtype Thread_Index is Positive range 1 .. P.Thread_Ceiling;
   type Thread_Entry is record
      Run : Thread_Body;
      Low, High : Unsigned_64 := 0;
   end record;
   Threads : array (Thread_Index) of Thread_Entry;
   Registered : Thread_Count := 0;
   --  Cleared by the kernel when each thread ends.
   Exit_Words : array (Thread_Index) of aliased Word32 := [others => 1];
   --  Bumped at each resume; parked threads wait on it.
   Resume_Word : aliased Word32 := 0 with Volatile;

   function Record_At (T : Thread_Index) return Natural is
     (P.Threads_At + (T - 1) * P.Thread_Bytes);

   ---------------------------------------------------------------------------
   --  The build-id: the GNU note the link keeps in the image's read-only
   --  segment (docs/image-debug-info.md), found through the ELF header and
   --  program headers, which the first loaded segment maps at Image_Base.
   ---------------------------------------------------------------------------
   procedure Publish_Build_Id is
      function Peek64 (A : Unsigned_64) return Unsigned_64 is
        (Unsigned_64 (A64.To_Pointer (To_Address (Integer_Address (A))).all));
      function Peek32 (A : Unsigned_64) return Unsigned_32 is
        (Unsigned_32 (A32.To_Pointer (To_Address (Integer_Address (A))).all));
      function Peek8 (A : Unsigned_64) return Unsigned_8 is
        (Unsigned_8 (A8.To_Pointer (To_Address (Integer_Address (A))).all));
      ELF_Magic : constant Unsigned_32 := 16#464C_457F#;
      GNU : constant Unsigned_32 := 16#0055_4E47#;   --  "GNU" & NUL
      Phoff_At : constant := 32;
      Phentsize_At : constant := 54;
      Phnum_At : constant := 56;
      Program_Header_Bytes : constant := 56;
      Program_Header_Ceiling : constant := 64;
      PT_Load : constant := 1;
      PT_Note : constant := 4;
      Note_Header_Bytes : constant := 12;
      Phoff, Phnum, High : Unsigned_64 := 0;
   begin
      Put32 (P.Build_Id_Length_At, 0);
      if Peek32 (Image_Base) /= ELF_Magic
        or else Unsigned_64 (Peek32 (Image_Base + Phentsize_At)) mod 2 ** 16 /= Program_Header_Bytes
      then
         return;
      end if;
      Phoff := Peek64 (Image_Base + Phoff_At);
      Phnum := Unsigned_64 (Peek32 (Image_Base + Phnum_At)) mod 2 ** 16;
      --  The headers must lie in the page the first segment maps.
      if Phnum > Program_Header_Ceiling
        or else Phoff + Phnum * Program_Header_Bytes > Image_Scan_Bytes
      then
         return;
      end if;
      for I in 0 .. Phnum - 1 loop
         declare
            Header : constant Unsigned_64 := Image_Base + Phoff + I * Program_Header_Bytes;
            Kind : constant Unsigned_32 := Peek32 (Header);
            Address : constant Unsigned_64 := Peek64 (Header + 16);
            Size : constant Unsigned_64 := Peek64 (Header + 40);
         begin
            if Kind = PT_Load then
               High := Unsigned_64'Max (High, Address + Size);
            elsif Kind = PT_Note and then Size >= Note_Header_Bytes + 4 + P.Build_Id_Ceiling
              and then Peek32 (Address) = GNU_Note_Name_Bytes
              and then Peek32 (Address + 4) = P.Build_Id_Ceiling
              and then Peek32 (Address + 8) = GNU_Build_Id_Note
              and then Peek32 (Address + 12) = GNU
            then
               for B in 0 .. P.Build_Id_Ceiling - 1 loop
                  Put8 (P.Build_Id_At + B, Peek8 (Address + 16 + Unsigned_64 (B)));
               end loop;
               Put32 (P.Build_Id_Length_At, P.Build_Id_Ceiling);
            end if;
         end;
      end loop;
      Put64 (P.Image_High_At, High);
   end Publish_Build_Id;

   ---------------------------------------------------------------------------
   --  Grants made, per debugger, so Detach can revoke them.
   ---------------------------------------------------------------------------
   subtype Grant_Count is Natural range 0 .. P.Grants_Per_Debugger_Ceiling;
   type Grant_Entry is record
      Holder : Unsigned_64 := 0;
      Ref : GR.Reference := (slot => 0, generation => 1);
   end record;
   Made : array (1 .. P.Grants_Per_Debugger_Ceiling) of Grant_Entry;
   Made_Count : Grant_Count := 0;

   procedure Grant_To
     (Holder : Process_ID; Base : Unsigned_64; Pages : Positive;
      Ref : out GR.Reference; Ok : out Boolean) is
   begin
      Ok := False;
      Ref := (slot => 0, generation => 1);
      if Made_Count = Grant_Count'Last then
         return;
      end if;
      --  READ-ONLY, to the caller being answered (current-reply rule).
      CuBit.Memory_Grants.Create_For_Process
        (Holder, To_Address (Integer_Address (Base)), Pages, False, Ref, Ok);
      if Ok then
         Made_Count := Made_Count + 1;
         Made (Made_Count) := (Holder => To_Word (Holder), Ref => Ref);
      end if;
   end Grant_To;

   procedure Revoke_All (Holder : Process_ID) is
      Kept : Grant_Count := 0;
      Ignore : Boolean;
   begin
      for I in 1 .. Made_Count loop
         if Made (I).Holder = To_Word (Holder) then
            CuBit.Memory_Grants.Revoke (Made (I).Ref, Ignore);
         else
            Kept := Kept + 1;
            Made (Kept) := Made (I);
         end if;
      end loop;
      Made_Count := Kept;
   end Revoke_All;

   ---------------------------------------------------------------------------
   --  The server thread
   ---------------------------------------------------------------------------
   function Answer (Label : Unsigned_32; W0, W1 : Unsigned_64 := 0) return Message is
      R : Message := NULL_MESSAGE;
   begin
      R.tag := (label => Label, length => 2, protocol => 0, ipc => <>, reserved => 0);
      R.words (0) := W0;
      R.words (1) := W1;
      return R;
   end Answer;

   function All_Parked return Boolean is
   begin
      for T in 1 .. Registered loop
         if Get32 (Record_At (T) + P.T_State_At) = P.Thread_Running then
            return False;
         end if;
      end loop;
      return True;
   end All_Parked;

   function Handle (From : Process_ID; Msg : Message) return Message is
      Label : constant Unsigned_32 := Msg.tag.label;
      Ref : GR.Reference;
      Ok : Boolean;
   begin
      if Label = P.Op_Attach then
         --  A debugger attaching again starts afresh: its earlier grants go.
         Revoke_All (From);
         Grant_To (From, State_Base, 1, Ref, Ok);
         return (if Ok then Answer (P.Reply_Ok, GR.Encode (Ref), Own_Process_Word)
                 else Answer (P.Reply_Refused));
      elsif Label = P.Op_Share then
         declare
            Base : constant Unsigned_64 := Msg.words (0);
            Pages : constant Unsigned_64 := Msg.words (1);
         begin
            if Base mod P.Page_Bytes /= 0 or else Pages not in 1 .. P.Share_Pages_Ceiling then
               return Answer (P.Reply_Refused);
            end if;
            Grant_To (From, Base, Positive (Pages), Ref, Ok);
            return (if Ok then Answer (P.Reply_Ok, GR.Encode (Ref), Pages)
                    else Answer (P.Reply_Refused));
         end;
      elsif Label = P.Op_Pause then
         declare
            Wait_Ms : constant Unsigned_64 :=
              Unsigned_64'Min (Msg.words (0), P.Pause_Wait_Ceiling_Ms);
            Until_Time : constant Unsigned_64 := Deadline_After (Wait_Ms);
         begin
            Put32 (P.Pause_Requested_At, 1);
            loop
               if All_Parked then
                  Put64 (P.Stop_Epoch_At, Get64 (P.Stop_Epoch_At) + 1);
                  Put32 (P.Parked_Count_At, Unsigned_32 (Registered));
                  return Answer (P.Reply_Ok, Get64 (P.Stop_Epoch_At));
               end if;
               exit when Deadline_After (0) >= Until_Time;
               W.Sleep_For (Pause_Poll_Ms);
            end loop;
            return Answer (P.Reply_Not_Yet);
         end;
      elsif Label = P.Op_Resume then
         Put32 (P.Pause_Requested_At, 0);
         Put32 (P.Parked_Count_At, 0);
         Resume_Word := Resume_Word + 1;
         declare
            Ignore : constant Unsigned_64 :=
              W.Futex_Wake (W.Private_Word (Resume_Word'Address), P.Thread_Ceiling);
         begin
            null;
         end;
         return Answer (P.Reply_Ok);
      elsif Label = P.Op_Detach then
         Revoke_All (From);
         return Answer (P.Reply_Ok);
      end if;
      return Answer (P.Reply_Refused);
   end Handle;

   procedure Server (Argument : Unsigned_64; Slot : CuBit.System_Facts.SP.Thread_Slot)
   with Convention => C, No_Return;
   procedure Server (Argument : Unsigned_64; Slot : CuBit.System_Facts.SP.Thread_Slot) is
      pragma Unreferenced (Argument);
      From : Process_ID;
      Msg : Message;
   begin
      CuBit.System_Facts.Enter_Thread (Slot);
      receive (From, Msg);
      loop
         replyWait (From, Handle (From, Msg), From, Msg);
      end loop;
   end Server;

   ---------------------------------------------------------------------------
   --  Threads
   ---------------------------------------------------------------------------
   procedure Runner (Argument : Unsigned_64; Slot : CuBit.System_Facts.SP.Thread_Slot)
   with Convention => C, No_Return;
   procedure Runner (Argument : Unsigned_64; Slot : CuBit.System_Facts.SP.Thread_Slot) is
   begin
      CuBit.System_Facts.Enter_Thread (Slot);
      if Argument in 1 .. Unsigned_64 (Registered) then
         Threads (Thread_Index (Argument)).Run.all;
         Put32 (Record_At (Thread_Index (Argument)) + P.T_State_At, P.Thread_Ended);
      end if;
      CuBit.Thread_Areas.Exit_Thread;
   end Runner;

   type Entry_Point is access procedure
     (Argument : Unsigned_64; Slot : CuBit.System_Facts.SP.Thread_Slot)
   with Convention => C;

   procedure Make_Thread
     (Code : Entry_Point; Bytes : Unsigned_64; Argument : Unsigned_64;
      Exit_Word : System.Address; Low, High : out Unsigned_64; Ok : out Boolean)
   is
      Stack, Area, Created : Unsigned_64;
      Ignore : Boolean;
   begin
      Ok := False;
      Low := 0;
      High := 0;
      Stack := CuBit.Owned_Reservations.Reserve (Bytes);
      if Stack = 0 then
         return;
      end if;
      --  The lowest page stays uncommitted: a guard.
      if not CuBit.Owned_Reservations.Commit_Steps (Stack + ABI.Page_Bytes, Bytes - ABI.Page_Bytes) then
         Ignore := CuBit.Owned_Reservations.Release (Stack, Bytes);
         return;
      end if;
      Area := CuBit.Thread_Areas.Create;
      if Area = CuBit.Thread_Areas.No_Area then
         Ignore := CuBit.Owned_Reservations.Release (Stack, Bytes);
         return;
      end if;
      Low := Stack + ABI.Page_Bytes;
      High := Stack + Bytes;
      Created := CuBit.Kernel_Calls.Call
        (ABI.Thread_Create, Unsigned_64 (To_Integer (Code.all'Address)),
         ((Stack + Bytes) and not (Stack_Alignment - 1)) - Return_Address_Bytes,
         Argument, Area, Unsigned_64 (To_Integer (Exit_Word)));
      if Created = Thread_ABI.Refused or else not Thread_ABI.Is_Created (Created) then
         CuBit.Thread_Areas.Discard (Area);
         Ignore := CuBit.Owned_Reservations.Release (Stack, Bytes);
         return;
      end if;
      Ok := True;
   end Make_Thread;

   Server_Exit : aliased Word32 := 1;

   procedure Start
     (Program      : String;
      Registration : CuBit.Handles.Notification;
      Started      : out Boolean)
   is
      Low, High : Unsigned_64;
   begin
      Started := False;
      if State_Ready then
         return;
      end if;
      State_Base := CuBit.Owned_Reservations.Allocate (P.Page_Bytes);
      if State_Base = 0 then
         return;
      end if;
      Put16 : declare
         Version_Word : constant Unsigned_32 :=
           Unsigned_32 (P.Version);   --  Thread_Count (the high half) is 0
      begin
         Put32 (P.Magic_At, P.Magic);
         Put32 (P.Version_At, Version_Word);
      end Put16;
      Put64 (P.Process_At, Own_Process_Word);
      Put_Text (P.Name_At, P.Name_Bytes, Program);
      Put64 (P.Image_Low_At, Image_Base);
      Publish_Build_Id;
      State_Ready := True;
      if registerDriver (P.Service_Role, Registration) = Unsigned_64'Last then
         debugPrint ("debug-stub: debug-target registration refused" & ASCII.LF);
         return;
      end if;
      Make_Thread (Server'Access, Server_Stack_Bytes, 0, Server_Exit'Address, Low, High, Started);
   end Start;

   procedure Spawn (Name : String; Run : not null Thread_Body; Started : out Boolean) is
      Index : Thread_Index;
      Low, High : Unsigned_64;
      Base : Natural;
   begin
      Started := False;
      if not State_Ready or else Registered = Thread_Count'Last then
         return;
      end if;
      Index := Registered + 1;
      Base := Record_At (Index);
      Threads (Index).Run := Run;
      --  Published before the thread runs: its stack is known when it first
      --  reaches a checkpoint.
      Registered := Index;
      Make_Thread (Runner'Access, Stack_Bytes, Unsigned_64 (Index), Exit_Words (Index)'Address,
                   Low, High, Started);
      if not Started then
         Registered := Index - 1;
         return;
      end if;
      Threads (Index).Low := Low;
      Threads (Index).High := High;
      Put32 (Base + P.T_Id_At, Unsigned_32 (Index));
      Put_Text (Base + P.T_Name_At, P.T_Name_Bytes, Name);
      Put64 (Base + P.T_Stack_Low_At, Low);
      Put64 (Base + P.T_Stack_High_At, High);
      Put32 (P.Version_At, Unsigned_32 (P.Version) + Unsigned_32 (Index) * 2 ** 16);
   end Spawn;

   ---------------------------------------------------------------------------
   --  Checkpoint
   ---------------------------------------------------------------------------
   type Register_Capture is record
      RIP, RSP, RBP, RBX, R12, R13, R14, R15 : Unsigned_64 := 0;
   end record;

   procedure Checkpoint is
      use System.Machine_Code;
      Regs : Register_Capture;
      Found : Natural := 0;
      Base : Natural;
      Seen : Word32;
      Ignore : Unsigned_64;
   begin
      --  One capture point: the unwinder starts at this RIP with these
      --  registers, and this procedure's CFI says where its caller's are.
      Asm ("lea 0(%%rip), %%rax" & ASCII.LF & ASCII.HT &
           "movq %%rax, %0" & ASCII.LF & ASCII.HT &
           "movq %%rsp, %1" & ASCII.LF & ASCII.HT &
           "movq %%rbp, %2" & ASCII.LF & ASCII.HT &
           "movq %%rbx, %3" & ASCII.LF & ASCII.HT &
           "movq %%r12, %4" & ASCII.LF & ASCII.HT &
           "movq %%r13, %5" & ASCII.LF & ASCII.HT &
           "movq %%r14, %6" & ASCII.LF & ASCII.HT &
           "movq %%r15, %7",
           Outputs => [Unsigned_64'Asm_Output ("=m", Regs.RIP),
                       Unsigned_64'Asm_Output ("=m", Regs.RSP),
                       Unsigned_64'Asm_Output ("=m", Regs.RBP),
                       Unsigned_64'Asm_Output ("=m", Regs.RBX),
                       Unsigned_64'Asm_Output ("=m", Regs.R12),
                       Unsigned_64'Asm_Output ("=m", Regs.R13),
                       Unsigned_64'Asm_Output ("=m", Regs.R14),
                       Unsigned_64'Asm_Output ("=m", Regs.R15)],
           Clobber => "rax",
           Volatile => True);
      if not State_Ready then
         return;
      end if;
      for T in 1 .. Registered loop
         if Regs.RSP >= Threads (T).Low and then Regs.RSP < Threads (T).High then
            Found := T;
            exit;
         end if;
      end loop;
      if Found = 0 then
         return;
      end if;
      Base := Record_At (Found);
      --  Odd while writing: a reader that sees the same even value before
      --  and after its copy has a consistent record.
      Put64 (Base + P.T_Sequence_At, Get64 (Base + P.T_Sequence_At) + 1);
      Put64 (Base + P.T_RIP_At, Regs.RIP);
      Put64 (Base + P.T_RSP_At, Regs.RSP);
      Put64 (Base + P.T_RBP_At, Regs.RBP);
      Put64 (Base + P.T_RBX_At, Regs.RBX);
      Put64 (Base + P.T_R12_At, Regs.R12);
      Put64 (Base + P.T_R13_At, Regs.R13);
      Put64 (Base + P.T_R14_At, Regs.R14);
      Put64 (Base + P.T_R15_At, Regs.R15);
      Put64 (Base + P.T_Checkpoints_At, Get64 (Base + P.T_Checkpoints_At) + 1);
      Put64 (Base + P.T_Sequence_At, Get64 (Base + P.T_Sequence_At) + 1);

      while Get32 (P.Pause_Requested_At) /= 0 loop
         Seen := Resume_Word;
         Put32 (Base + P.T_State_At, P.Thread_Parked);
         exit when Get32 (P.Pause_Requested_At) = 0;
         Ignore := W.Futex_Wait
           (Resume_Word'Address, Unsigned_64 (Seen), Deadline_After (Park_Recheck_Ms));
      end loop;
      Put32 (Base + P.T_State_At, P.Thread_Running);
   end Checkpoint;

end CuBit.Debug_Stub;
