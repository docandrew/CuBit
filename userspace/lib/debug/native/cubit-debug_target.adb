------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Deallocation;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Debug_Protocol;
with CuBit.Filesystem_Queues;
with CuBit.Filesystem_Sessions;
with CuBit.Filesystems;
with CuBit.Grant_References;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with Debug_ABI;

package body CuBit.Debug_Target is
   package P renames CuBit.Debug_Protocol;
   package GR renames CuBit.Grant_References;
   package MG renames CuBit.Memory_Grants;
   package FS renames CuBit.Filesystem_Sessions;
   package FQ renames CuBit.Filesystem_Queues;
   use type FS.Token;
   use type FS.Wait_Result;

   Page : constant := P.Page_Bytes;
   --  One share asks for this many pages around the address first, then
   --  for the one page if the target refuses the chunk.
   Chunk_Pages : constant := 16;
   --  A share or detach answer is immediate; this bounds a stuck target.
   Share_Answer_Ms : constant := 250;
   --  Past the target's own pause wait, for the answer to arrive.
   Reply_Slack_Ms : constant := 250;
   File_Ceiling_Bytes : constant := 64 * 1024 * 1024;
   Initial_File_Bytes : constant := 1024 * 1024;
   Arena_Pages : constant := 256;
   Path_At : constant := 0;
   Data_At : constant := FQ.Page_Bytes;
   Chunk_Bytes : constant := (Arena_Pages - 1) * FQ.Page_Bytes;
   Hex_Split : constant := 2;

   function Message_Of (Label : Unsigned_32; W0, W1 : Unsigned_64 := 0) return Message is
      M : Message := NULL_MESSAGE;
   begin
      M.tag := (label => Label, length => 2, protocol => 0, ipc => <>, reserved => 0);
      M.words (0) := W0;
      M.words (1) := W1;
      return M;
   end Message_Of;

   procedure Free is new Ada.Unchecked_Deallocation (Connection, Connection_Access);

   function Answered (Tag : MessageTag) return Boolean is
     (Ending_Of (Tag) = Call_Answered);

   function Deadline_After (Milliseconds : Unsigned_64) return Unsigned_64 is
     (CuBit.Messages.Deadline_After (Milliseconds));

   procedure Acquire
     (C : Connection; Ref_Word : Unsigned_64; Pages : Unsigned_64;
      Mapped : out Unsigned_64; Ok : out Boolean)
   is
      Where : System.Address;
   begin
      Mapped := 0;
      Ok := False;
      if not GR.Valid_Wire (Ref_Word) then
         return;
      end if;
      MG.Acquire_Via_Capability
        (C.Handle.Target, GR.Decode (Ref_Word), 0, Pages * Page, MG.Read_Access, Where, Ok);
      if Ok then
         Mapped := Unsigned_64 (To_Integer (Where));
      end if;
   end Acquire;

   ---------------------------------------------------------------------------
   --  The kernel path
   ---------------------------------------------------------------------------
   package HD renames CuBit.Handles.Debug;
   package DA renames Debug_ABI;

   function Through_Kernel (C : Connection) return Boolean is (C.Kernel);
   function Target_Identity (C : Connection) return Unsigned_64 is (C.Target_Id);

   function View_Address (C : Connection; Slot : Positive) return Unsigned_64 is
     (C.Slice + Unsigned_64 (Slot - 1) * Page);

   procedure Connect_Kernel
     (Handle : Process_Handle; C : out Connection_Access; Result : out Status)
   is
      R : constant HD.Reply := HD.Call (Handle.Session, DA.Describe);
   begin
      C := null;
      if HD.Outcome_Is (R, DA.Revoked) or else HD.Outcome_Is (R, DA.Target_Gone) then
         Result := No_Target;
         return;
      elsif not HD.Outcome_Is (R, DA.Done) or else R.W1 /= DA.Kind_Word (DA.Observe) then
         Result := Refused;
         return;
      end if;
      C := new Connection;
      C.Handle := Handle;
      C.Kernel := True;
      C.Target_Id := R.W2;
      C.Slice := R.W3;
      Result := Ok;
   end Connect_Kernel;

   procedure Refresh (C : in out Connection) is
      Ignore : HD.Reply;
   begin
      if not C.Kernel then
         return;
      end if;
      for K in C.Views'Range loop
         if C.Views (K).Mapped then
            Ignore := HD.Unmap_View (C.Handle.Session, View_Address (C, K), 1);
            C.Views (K) := (others => <>);
         end if;
      end loop;
      C.View_Next := 1;
   end Refresh;

   --  The view slot holding Target_Page, mapping it if need be; 0 when the
   --  kernel will not map it (unmapped, not the target's own RAM, ...).
   function View_Of (C : in out Connection; Target_Page : Unsigned_64) return Natural is
      K : Positive;
      R : HD.Reply;
      Ignore : HD.Reply;
   begin
      for J in C.Views'Range loop
         if C.Views (J).Mapped and then C.Views (J).Target_Page = Target_Page then
            return J;
         end if;
      end loop;
      K := C.View_Next;
      C.View_Next := (if K = View_Cache_Ceiling then 1 else K + 1);
      if C.Views (K).Mapped then
         Ignore := HD.Unmap_View (C.Handle.Session, View_Address (C, K), 1);
         C.Views (K) := (others => <>);
      end if;
      R := HD.Map_View (C.Handle.Session, Target_Page, View_Address (C, K), 1);
      if not HD.Outcome_Is (R, DA.Done) or else R.W1 /= 1 then
         return 0;
      end if;
      C.Views (K) := (Target_Page => Target_Page, Mapped => True);
      return K;
   end View_Of;

   procedure Read_Kernel
     (C : in out Connection; Address : Unsigned_64; Into : out Byte_Buffer; Got : out Natural)
   is
      Cur : Unsigned_64 := Address;
      Slot : Natural;
   begin
      Into := [others => 0];
      Got := 0;
      while Got < Into'Length loop
         Slot := View_Of (C, Cur - Cur mod Page);
         exit when Slot = 0;
         declare
            In_Page : constant Unsigned_64 := Page - Cur mod Page;
            Count : constant Natural :=
              Natural (Unsigned_64'Min (In_Page, Unsigned_64 (Into'Length - Got)));
            Viewed : constant Byte_Buffer (0 .. Count - 1)
              with Import, Address => To_Address
                (Integer_Address (View_Address (C, Slot) + Cur mod Page));
         begin
            Into (Into'First + Got .. Into'First + Got + Count - 1) := Viewed;
            Got := Got + Count;
            Cur := Cur + Unsigned_64 (Count);
         end;
      end loop;
   end Read_Kernel;

   function Wait_Of (Code : Unsigned_64) return Wait_Code is
     (case Code is
        when 1 => Running, when 2 => Ready, when 3 => In_Call, when 4 => Receiving,
        when 5 => Completion_Wait, when 6 => Futex, when 7 => Sleeping,
        when 8 => Not_Started, when 9 => Other, when others => Not_Known);

   procedure Kernel_Threads
     (C : Connection; List : out Kernel_Thread_Array; Count : out Kernel_Thread_Count;
      Live : out Natural; Result : out Status)
   is
      Entries : Byte_Buffer (0 .. Kernel_Thread_Ceiling * DA.Thread_Entry_Bytes - 1) := [others => 0];
      R : HD.Reply;
      function U64 (At_Byte : Natural; Width : Positive) return Unsigned_64 is
         V : Unsigned_64 := 0;
      begin
         for B in reverse 0 .. Width - 1 loop
            V := Shift_Left (V, 8) or Unsigned_64 (Entries (At_Byte + B));
         end loop;
         return V;
      end U64;
   begin
      List := [others => <>];
      Count := 0;
      Live := 0;
      if not C.Kernel then
         Result := Refused;
         return;
      end if;
      R := HD.Threads (C.Handle.Session, Entries'Address, Entries'Length);
      if not HD.Outcome_Is (R, DA.Done) then
         Result := (if HD.Outcome_Is (R, DA.Target_Gone) or else HD.Outcome_Is (R, DA.Revoked)
                    then No_Target else Refused);
         return;
      end if;
      Count := Natural (Unsigned_64'Min (R.W1, Kernel_Thread_Ceiling));
      Live := Natural (Unsigned_64'Min (R.W2, Unsigned_64 (Natural'Last)));
      for T in 1 .. Count loop
         declare
            At_Byte : constant Natural := (T - 1) * DA.Thread_Entry_Bytes;
         begin
            List (T) :=
              (Slot        => Unsigned_16 (U64 (At_Byte, 2)),
               Wait        => Wait_Of (U64 (At_Byte + 2, 1)),
               Generation  => Unsigned_32 (U64 (At_Byte + 4, 4)),
               CPU         => Unsigned_32 (U64 (At_Byte + 8, 4)),
               Peer        => U64 (At_Byte + 16, 8),
               Since       => U64 (At_Byte + 24, 8),
               Deadline    => U64 (At_Byte + 32, 8),
               Sampled_RIP => U64 (At_Byte + 40, 8));
         end;
      end loop;
      Result := Ok;
   end Kernel_Threads;

   procedure Kernel_Registers
     (C : Connection; Thread : Kernel_Thread; Regs : out General_Registers;
      Result : out Register_Status)
   is
      R : HD.Reply;
   begin
      Regs := [others => 0];
      if not C.Kernel then
         Result := Failed;
         return;
      end if;
      R := HD.Registers
        (C.Handle.Session, DA.Selector (Thread.Slot, DA.General, Thread.Generation),
         Regs'Address, DA.General_Bytes);
      Result := (if HD.Outcome_Is (R, DA.Done) then Saved
                 elsif HD.Outcome_Is (R, DA.Running) then On_CPU
                 elsif HD.Outcome_Is (R, DA.No_Such_Thread) or else HD.Outcome_Is (R, DA.Target_Gone)
                 then Gone
                 else Failed);
   end Kernel_Registers;

   procedure Connect
     (Handle : Process_Handle; Until_Time : Unsigned_64;
      C : out Connection_Access; Result : out Status)
   is
      M : Message := Message_Of (P.Op_Attach);
      Tag : MessageTag;
      Mapped : Unsigned_64;
      Granted : Boolean;
   begin
      C := null;
      Result := No_Target;
      if HD.Valid (Handle.Session) then
         Connect_Kernel (Handle, C, Result);
         return;
      elsif not CuBit.Handles.Valid (Handle.Target) then
         return;
      end if;
      Tag := capCall (Handle.Target, M, Until_Time);
      if Ending_Of (Tag) = Call_Timed_Out then
         Result := Timed_Out;
         return;
      elsif not Answered (Tag) then
         return;
      elsif M.tag.label /= P.Reply_Ok then
         Result := Refused;
         return;
      end if;
      C := new Connection;
      C.Handle := Handle;
      Acquire (C.all, M.words (0), 1, Mapped, Granted);
      if not Granted then
         Free (C);
         Result := Refused;
         return;
      end if;
      C.State_At := Mapped;
      Result := Ok;
   end Connect;

   procedure Disconnect (C : in out Connection_Access; Until_Time : Unsigned_64) is
      M : Message := Message_Of (P.Op_Detach);
      Ignore : MessageTag;
      Ignore_Reply : HD.Reply;
      Ignore_Closed : Boolean;
   begin
      if C = null then
         return;
      elsif C.Kernel then
         --  The session ends (its views go with it); this copy is let go.
         Refresh (C.all);
         Ignore_Reply := HD.Call (C.Handle.Session, DA.Detach);
         Ignore_Closed := HD.Close (C.Handle.Session);
         Free (C);
         return;
      end if;
      --  The target revokes every grant it made to this process.
      Ignore := capCall (C.Handle.Target, M, Until_Time);
      Free (C);
   end Disconnect;

   procedure Read_State (C : Connection; Page : out State_Page) is
      Shared : constant State_Page
        with Import, Address => To_Address (Integer_Address (C.State_At));
   begin
      if C.State_At = 0 then
         Page := [others => 0];
      else
         Page := Shared;
      end if;
   end Read_State;

   function Was_Refused (C : Connection; Page_Base : Unsigned_64) return Boolean is
     (for some R of C.Refused => R = Page_Base);

   procedure Share (C : in out Connection; Base, Pages : Unsigned_64; Ok : out Boolean) is
      M : Message := Message_Of (P.Op_Share, Base, Pages);
      Tag : MessageTag;
      Mapped : Unsigned_64;
   begin
      Ok := False;
      if C.Region_Count = Region_Ceiling then
         return;
      end if;
      Tag := capCall (C.Handle.Target, M, Deadline_After (Share_Answer_Ms));
      if not Answered (Tag) or else M.tag.label /= P.Reply_Ok or else M.words (1) /= Pages then
         return;
      end if;
      Acquire (C, M.words (0), Pages, Mapped, Ok);
      if Ok then
         C.Region_Count := C.Region_Count + 1;
         C.Regions (C.Region_Count) := (Base => Base, Pages => Pages, Mapped => Mapped);
      end if;
   end Share;

   function Region_Of (C : Connection; Address : Unsigned_64) return Natural is
   begin
      for I in reverse 1 .. C.Region_Count loop
         if Address >= C.Regions (I).Base
           and then Address - C.Regions (I).Base < C.Regions (I).Pages * Page
         then
            return I;
         end if;
      end loop;
      return 0;
   end Region_Of;

   procedure Read
     (C : in out Connection; Address : Unsigned_64; Into : out Byte_Buffer; Got : out Natural)
   is
      Cur : Unsigned_64 := Address;
      Index : Natural;
      Ok : Boolean;
   begin
      if C.Kernel then
         Read_Kernel (C, Address, Into, Got);
         return;
      end if;
      Into := [others => 0];
      Got := 0;
      while Got < Into'Length loop
         Index := Region_Of (C, Cur);
         if Index = 0 then
            declare
               Page_Base : constant Unsigned_64 := Cur - Cur mod Page;
               Chunk_Base : constant Unsigned_64 := Cur - Cur mod (Chunk_Pages * Page);
            begin
               exit when Was_Refused (C, Page_Base);
               Share (C, Chunk_Base, Chunk_Pages, Ok);
               if not Ok then
                  Share (C, Page_Base, 1, Ok);
               end if;
               if not Ok then
                  C.Refused (C.Refused_Next) := Page_Base;
                  C.Refused_Next := (if C.Refused_Next = Refused_Ceiling then 1 else C.Refused_Next + 1);
                  exit;
               end if;
               Index := Region_Of (C, Cur);
               exit when Index = 0;
            end;
         end if;
         declare
            R : Region renames C.Regions (Index);
            In_Region : constant Unsigned_64 := R.Base + R.Pages * Page - Cur;
            Count : constant Natural :=
              Natural (Unsigned_64'Min (In_Region, Unsigned_64 (Into'Length - Got)));
            Shared : constant Byte_Buffer (0 .. Count - 1)
              with Import, Address => To_Address (Integer_Address (R.Mapped + (Cur - R.Base)));
         begin
            Into (Into'First + Got .. Into'First + Got + Count - 1) := Shared;
            Got := Got + Count;
            Cur := Cur + Unsigned_64 (Count);
         end;
      end loop;
   end Read;

   procedure Pause (C : Connection; Until_Time : Unsigned_64; Epoch : out Unsigned_64; Result : out Status) is
      Now : constant Unsigned_64 := Deadline_After (0);
      Wait : constant Unsigned_64 :=
        (if Until_Time > Now then Unsigned_64'Min (Until_Time - Now, P.Pause_Wait_Ceiling_Ms) else 0);
      M : Message := Message_Of (P.Op_Pause, Wait);
      Tag : MessageTag;
   begin
      Epoch := 0;
      --  The kernel path has no stop yet (Debug_Pause answers Not_Yet).
      if C.Kernel then
         Result := Refused;
         return;
      end if;
      Tag := capCall (C.Handle.Target, M, Deadline_After (Wait + Reply_Slack_Ms));
      if not Answered (Tag) then
         Result := (if Ending_Of (Tag) = Call_Timed_Out then Timed_Out else No_Target);
      elsif M.tag.label = P.Reply_Ok then
         Epoch := M.words (0);
         Result := Ok;
      elsif M.tag.label = P.Reply_Not_Yet then
         Result := Timed_Out;
      else
         Result := Refused;
      end if;
   end Pause;

   procedure Resume (C : Connection; Until_Time : Unsigned_64; Result : out Status) is
      M : Message := Message_Of (P.Op_Resume);
      Tag : MessageTag;
   begin
      if C.Kernel then
         Result := Refused;
         return;
      end if;
      Tag := capCall (C.Handle.Target, M, Until_Time);
      Result := (if Answered (Tag) and then M.tag.label = P.Reply_Ok then Ok
                 elsif Ending_Of (Tag) = Call_Timed_Out then Timed_Out else No_Target);
   end Resume;

   ---------------------------------------------------------------------------
   --  Files
   ---------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation (Byte_Buffer, Buffer_Access);

   Lent_Reader : File_Reader := null;

   procedure Use_File_Reader (Reader : File_Reader) is
   begin
      Lent_Reader := Reader;
   end Use_File_Reader;

   --  The whole file through the host's reader, a chunk at a time.
   procedure Load_Through_Reader (Path : String; Data : out Buffer_Access; Result : out Status) is
      Filled : Natural := 0;
      Got : Natural;
      Found : Boolean;
   begin
      Data := new Byte_Buffer (0 .. Initial_File_Bytes - 1);
      Result := Ok;
      loop
         if Filled = Data'Length then
            if Filled >= File_Ceiling_Bytes then
               Result := Malformed;
               exit;
            end if;
            declare
               Bigger : constant Buffer_Access :=
                 new Byte_Buffer (0 .. Natural'Min (2 * Data'Length, File_Ceiling_Bytes) - 1);
            begin
               Bigger (0 .. Filled - 1) := Data (0 .. Filled - 1);
               Free (Data);
               Data := Bigger;
            end;
         end if;
         Lent_Reader (Path, Filled, Data (Filled .. Data'Last), Got, Found);
         if not Found then
            Result := (if Filled = 0 then Not_Found else Malformed);
            exit;
         end if;
         Filled := Filled + Got;
         exit when Filled < Data'Length;
      end loop;
      if Result /= Ok then
         Free (Data);
         return;
      end if;
      declare
         Exact : constant Buffer_Access := new Byte_Buffer (0 .. Filled - 1);
      begin
         Exact.all := Data (0 .. Filled - 1);
         Free (Data);
         Data := Exact;
      end;
   end Load_Through_Reader;

   procedure Load_Path
     (Filesystem : CuBit.Handles.Endpoint; Path : String; Until_Time : Unsigned_64;
      Data : out Buffer_Access; Result : out Status)
   is
      Session : FS.Session;
      Opened : Boolean;
      Handle, Count, Ignore : Unsigned_64 := 0;
      Filled : Natural := 0;
      Answer : Unsigned_32;

      function Call (Item : FQ.Request; Value : out Unsigned_64) return Unsigned_32 is
         Tag : FS.Token;
         Done : FQ.Queues.Completion;
         Got : Boolean;
         Waited : FS.Wait_Result;
      begin
         Value := 0;
         if not FS.Can_Submit (Session) then
            return CuBit.Filesystems.REPLY_ERR;
         end if;
         FS.Submit (Session, Item, Tag);
         if Tag = 0 then
            return CuBit.Filesystems.REPLY_ERR;
         end if;
         FS.Wait_Answer (Session, Until_Time, Waited);
         if Waited /= FS.Answer_Waiting then
            return CuBit.Messages.REPLY_TIMEOUT;
         end if;
         FS.Reap (Session, Done, Got);
         if not Got or else Done.Tag /= Tag then
            return CuBit.Filesystems.REPLY_ERR;
         end if;
         Value := Done.Answer.Value;
         return Done.Answer.Status;
      end Call;
   begin
      Data := null;
      Result := Not_Found;
      if Lent_Reader /= null then
         Load_Through_Reader (Path, Data, Result);
         return;
      end if;
      if Path'Length > FQ.Page_Bytes or else not CuBit.Handles.Valid (Filesystem) then
         return;
      end if;
      FS.Open (Session, Filesystem, Arena_Pages, Opened);
      if not Opened then
         debugPrint ("debug-target: " & Path & ": no filesystem session" & ASCII.LF);
         Result := Refused;
         return;
      end if;
      declare
         Shared : String (1 .. Path'Length)
           with Import, Address => FS.Arena (Session) + Storage_Offset (Path_At);
      begin
         Shared := Path;
      end;
      if Call ((Operation => FQ.Queue_Open,
                Options => Unsigned_32 (CuBit.Filesystems.OPEN_READ_ONLY),
                Length => Unsigned_64 (Path'Length), Arena_Offset => Path_At, others => <>),
               Handle) /= CuBit.Filesystems.REPLY_OK
      then
         debugPrint ("debug-target: " & Path & ": not opened (not there, or outside the scope)" & ASCII.LF);
         FS.Close (Session);
         return;
      end if;
      Data := new Byte_Buffer (0 .. Initial_File_Bytes - 1);
      Result := Ok;
      loop
         Answer := Call ((Operation => FQ.Queue_Read_At, Handle => Handle,
                          Position => Unsigned_64 (Filled), Length => Chunk_Bytes,
                          Arena_Offset => Data_At, others => <>), Count);
         if Answer /= CuBit.Filesystems.REPLY_OK or else Count > Chunk_Bytes then
            Result := (if Answer = CuBit.Messages.REPLY_TIMEOUT then Timed_Out else Malformed);
            exit;
         end if;
         exit when Count = 0;
         if Filled + Natural (Count) > File_Ceiling_Bytes then
            Result := Malformed;
            exit;
         end if;
         if Filled + Natural (Count) > Data'Length then
            declare
               Bigger : constant Buffer_Access :=
                 new Byte_Buffer (0 .. Natural'Min (2 * Data'Length, File_Ceiling_Bytes) - 1);
            begin
               Bigger (0 .. Filled - 1) := Data (0 .. Filled - 1);
               Free (Data);
               Data := Bigger;
            end;
         end if;
         declare
            Shared : constant Byte_Buffer (0 .. Natural (Count) - 1)
              with Import, Address => FS.Arena (Session) + Storage_Offset (Data_At);
         begin
            Data (Filled .. Filled + Natural (Count) - 1) := Shared;
         end;
         Filled := Filled + Natural (Count);
      end loop;
      if Call ((Operation => FQ.Queue_Close, Handle => Handle, others => <>), Ignore) /= 0 then
         null;
      end if;
      FS.Close (Session);
      if Result /= Ok then
         Free (Data);
         return;
      end if;
      --  Exactly the file's bytes.
      declare
         Exact : constant Buffer_Access := new Byte_Buffer (0 .. Filled - 1);
      begin
         Exact.all := Data (0 .. Filled - 1);
         Free (Data);
         Data := Exact;
      end;
   end Load_Path;

   procedure Load_Debug_File
     (C : Connection; Build_Id : String; Until_Time : Unsigned_64;
      Data : out Buffer_Access; Path : out String; Path_Length : out Natural; Result : out Status)
   is
      Name : constant String :=
        Debug_Root & ".build-id/" & Build_Id (Build_Id'First .. Build_Id'First + Hex_Split - 1)
        & "/" & Build_Id (Build_Id'First + Hex_Split .. Build_Id'Last) & ".debug";
   begin
      Path := [others => ' '];
      Path_Length := Natural'Min (Name'Length, Path'Length);
      Path (Path'First .. Path'First + Path_Length - 1) := Name (Name'First .. Name'First + Path_Length - 1);
      Load_Path (C.Handle.Filesystem, Name, Until_Time, Data, Result);
   end Load_Debug_File;

   procedure Load_Source
     (C : Connection; Build_Id : String; File : String; Until_Time : Unsigned_64;
      Data : out Buffer_Access; Result : out Status) is
   begin
      Load_Path (C.Handle.Filesystem, Debug_Root & "Source/" & Build_Id & "/" & File,
                 Until_Time, Data, Result);
   end Load_Source;

end CuBit.Debug_Target;
