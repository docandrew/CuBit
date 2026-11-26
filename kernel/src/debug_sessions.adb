pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Debug_ABI; use Debug_ABI;
with Debug_Admission;
with Debug_View_Walk;
with Capability_ABI;
with Process_Capabilities;
with Process_Identities;
with Process_Lifetime;
with Process.User_Memory;
with Stackframe;
with Spinlocks;
with BuddyAllocator;
with Image_Shares;
with TLB_Shootdown;
with Virtmem;

package body Debug_Sessions is
   package Caps renames Capabilities;
   use type Caps.CapabilityType;
   use type Caps.Generation;
   use type Process.ThreadID;
   use type Process.ProcessState;
   use type Process.ProcessMode;
   use type Debug_View_Walk.Verdict;

   ---------------------------------------------------------------------------
   -- The session table (Table_Lock)
   ---------------------------------------------------------------------------
   type Session_State is record
      Live            : Boolean := False;
      -- A slot whose generation reached its last value is never reused, so
      -- no stale capability can ever match it again.
      Exhausted       : Boolean := False;
      Generation      : Caps.Generation := Caps.INITIAL_GENERATION;
      Kind            : Debug_Kind := Observe;
      Target          : Process.ProcessID := Process.NO_PROCESS;
      Target_Identity : Unsigned_64 := 0;
      Viewer          : Process.ProcessID := Process.NO_PROCESS;
      Viewer_Identity : Unsigned_64 := 0;
      -- Pages of the slice that may hold a view (a bound for sweeps).
      View_High       : Natural range 0 .. View_Page_Ceiling := 0;
   end record;

   -- The frame tags are the kernel's (Stackframe) and the ABI's alike.
   pragma Compile_Time_Error
     (System_Call_Frame_Tag /= Stackframe.System_Call_Frame_Tag
      or else Initial_Frame_Tag /= Stackframe.Initial_Frame_Tag,
      "Debug_ABI and Stackframe disagree on the saved-frame tags");

   Sessions   : array (Session_Index) of Session_State;
   Table_Lock : Spinlocks.Spinlock;

   -- Views being removed in one batch: their frames, unpinned only after
   -- the TLB round (Table_Lock serializes every user of this buffer).
   Retiring : array (0 .. Map_Pages_Ceiling - 1) of Virtmem.PhysAddress :=
     [others => 0];

   function Cap_Type (K : Debug_Kind) return Caps.CapabilityType is
     (case K is
         when Observe    => Caps.CAP_DEBUG_OBSERVE,
         when Pause      => Caps.CAP_DEBUG_PAUSE,
         when Modify     => Caps.CAP_DEBUG_MODIFY,
         when Instrument => Caps.CAP_DEBUG_INSTRUMENT);

   function Kind_Of (T : Caps.CapabilityType) return Debug_Kind is
     (case T is
         when Caps.CAP_DEBUG_PAUSE      => Pause,
         when Caps.CAP_DEBUG_MODIFY     => Modify,
         when Caps.CAP_DEBUG_INSTRUMENT => Instrument,
         when others                    => Observe);

   function Identity_Word (Pid : Process.ProcessID) return Unsigned_64 is
     (Process_Identities.To_Word (Process.identityOf (Pid)));

   -- Pid is a live user process.
   function Live_User (Pid : Process.ProcessID) return Boolean is
     (Pid in Process.ProctabRange
      and then Process.threadOf (Pid).state /= Process.INVALID
      and then Process.threadOf (Pid).mode = Process.USER
      and then Process.proctab (Pid).pgTable /= Process.NO_PROCESS);

   procedure Answer
     (Request : Process.Message; Reply : out Process.Message; What : Outcome;
      W1, W2, W3 : Unsigned_64 := 0) is
   begin
      Reply := Process.NULL_MESSAGE;
      Reply.tag.label := Request.tag.label;
      Reply.tag.length := Reply_Words;
      Reply.words (Outcome_Word) := Outcome_Word_Of (What);
      Reply.words (1) := W1;
      Reply.words (2) := W2;
      Reply.words (3) := W3;
   end Answer;

   ---------------------------------------------------------------------------
   -- Page tables
   ---------------------------------------------------------------------------
   procedure Read_Table_Entry
     (Table_Frame : Unsigned_64; Index : Debug_View_Walk.Table_Index;
      Word : out Unsigned_64)
   is
      Entry_Word : Unsigned_64 with Import, Atomic,
        Address => Virtmem.P2Va (Virtmem.PhysAddress (Table_Frame)) +
                   Storage_Offset (Index * 8);
   begin
      Word := Entry_Word;
   end Read_Table_Entry;

   function Walk is new Debug_View_Walk.Walk (Read_Table_Entry);

   procedure Map_Page_Inst is new Virtmem.mapPage (BuddyAllocator.allocFrame);

   -- What Viewer's tables map at View, or 0.
   function Mapped_At (Viewer : Process.ProcessID; View : Unsigned_64)
     return Virtmem.PhysAddress is
     (Virtmem.tableWalk
        (Integer_Address (View), Process.addrtab (Process.proctab (Viewer).pgTable)));

   -- Map one page of Target at View in Viewer, read-only and never
   -- executable, holding a pin on the target's own frame.
   procedure Map_Page
     (Viewer, Target : Process.ProcessID; Target_Address, View : Unsigned_64;
      Refusal : out Map_Refusal; Occupied_Page : out Boolean)
   is
      R : Debug_View_Walk.Result;
      Pinned, Installed, Released : Boolean := False;
      Frame : Virtmem.PhysAddress := 0;
   begin
      Refusal := None;
      Occupied_Page := False;
      Process.lockAddressSpace (Target);
      R := Walk (Unsigned_64 (Process.rootPhysical (Process.proctab (Target).pgTable)),
                 Target_Address, Unsigned_64 (Virtmem.MAX_PHYS_USABLE));
      if R.Kind = Debug_View_Walk.Viewable then
         Frame := Virtmem.PhysAddress (R.Frame);
         -- Only a frame the target owns: never device memory, a kernel page
         -- or a page another process granted it.
         BuddyAllocator.pinOwnedFrame
           (Frame, BuddyAllocator.Frame_Owner (Target), Pinned);
      end if;
      Process.unlockAddressSpace (Target);
      case R.Kind is
         when Debug_View_Walk.Unmapped       => Refusal := Unmapped; return;
         when Debug_View_Walk.Not_Write_Back => Refusal := Not_Write_Back; return;
         when Debug_View_Walk.Viewable       => null;
      end case;
      -- KERN-006: or a frame of the shared image the target maps (its
      -- read-only segments), pinned as the image's, not the target's.
      if not Pinned and then R.Kind = Debug_View_Walk.Viewable then
         declare
            use type Image_Shares.Share;
            S : constant Image_Shares.Share := Process.proctab (Target).imageShare;
            Page : Image_Shares.Page_Index;
            Inside : Boolean;
         begin
            if S /= Image_Shares.No_Share then
               Image_Shares.Page_Of (S, Target_Address, Page, Inside);
               if Inside then
                  Image_Shares.Pin_View (S, Page, Frame, Pinned);
               end if;
            end if;
         end;
      end if;
      if not Pinned then
         Refusal := Not_Owned;
         return;
      end if;

      Process.lockAddressSpace (Viewer);
      if Mapped_At (Viewer, View) /= 0 then
         Occupied_Page := True;
      else
         Map_Page_Inst
           (Frame, Integer_Address (View), Virtmem.PG_USERDATARO,
            Process.addrtab (Process.proctab (Viewer).pgTable), Installed);
      end if;
      Process.unlockAddressSpace (Viewer);
      if not Installed then
         -- Nothing maps it: no round is needed before the pin goes.
         BuddyAllocator.unpinFrame (Frame, Released);
         if not Released then
            raise Program_Error with "Debug view pin lost before install";
         end if;
         if not Occupied_Page then
            Refusal := Install_Failed;
         end if;
      end if;
   end Map_Page;

   -- Remove Viewer's view pages in [First, First + Pages * Page_Bytes)
   -- (Pages <= Map_Pages_Ceiling): clear them, one TLB round, then unpin.
   procedure Remove_Batch
     (Viewer : Process.ProcessID; First : Unsigned_64; Pages : Natural;
      Removed : out Natural)
   is
      Physical : Virtmem.PhysAddress;
      OK : Boolean;
   begin
      Removed := 0;
      Process.lockAddressSpace (Viewer);
      for Page in 0 .. Pages - 1 loop
         declare
            View : constant Unsigned_64 := First + Unsigned_64 (Page) * Page_Bytes;
         begin
            Physical := Mapped_At (Viewer, View);
            if Physical /= 0 then
               Virtmem.unmapPage
                 (Integer_Address (View),
                  Process.addrtab (Process.proctab (Viewer).pgTable), OK);
               if not OK then
                  raise Program_Error with "Debug view could not be removed";
               end if;
               Retiring (Removed) := Physical;
               Removed := Removed + 1;
            end if;
         end;
      end loop;
      Process.unlockAddressSpace (Viewer);
      if Removed > 0 then
         -- No pin release (hence no allocator reuse) before every CPU,
         -- including the viewer's other threads, dropped the entries.
         TLB_Shootdown.Invalidate_All;
         for I in 0 .. Removed - 1 loop
            BuddyAllocator.unpinFrame (Retiring (I), OK);
            if not OK then
               raise Program_Error with "Debug view lost its pin";
            end if;
         end loop;
      end if;
   end Remove_Batch;

   -- End session S (Table_Lock held): remove its views while the viewer's
   -- tables exist, then retire the slot's generation.
   procedure End_Locked (S : Session_Index) is
      Session : Session_State renames Sessions (S);
      Removed : Natural;
   begin
      if not Session.Live then
         return;
      end if;
      if Session.Viewer /= Process.NO_PROCESS
        and then Live_User (Session.Viewer)
        and then Identity_Word (Session.Viewer) = Session.Viewer_Identity
      then
         declare
            Done_Pages : Natural := 0;
         begin
            while Done_Pages < Session.View_High loop
               declare
                  Batch : constant Natural :=
                    Natural'Min (Map_Pages_Ceiling, Session.View_High - Done_Pages);
               begin
                  Remove_Batch
                    (Session.Viewer,
                     Slice_Base (S) + Unsigned_64 (Done_Pages) * Page_Bytes,
                     Batch, Removed);
                  Done_Pages := Done_Pages + Batch;
               end;
            end loop;
         end;
      end if;
      Session.Live := False;
      Session.Target := Process.NO_PROCESS;
      Session.Target_Identity := 0;
      Session.Viewer := Process.NO_PROCESS;
      Session.Viewer_Identity := 0;
      Session.View_High := 0;
      if Session.Generation = Caps.Generation'Last then
         Session.Exhausted := True;
      else
         Session.Generation := Session.Generation + 1;
      end if;
   end End_Locked;

   ---------------------------------------------------------------------------
   -- Threads and registers (Table_Lock, then Process.lock)
   ---------------------------------------------------------------------------
   type Thread_Entry is record
      Slot       : Unsigned_16;
      Reason     : Unsigned_8;
      Reserved_1 : Unsigned_8;
      Generation : Unsigned_32;
      CPU        : Unsigned_32;
      Reserved_2 : Unsigned_32;
      Peer       : Unsigned_64;
      Since      : Unsigned_64;
      Deadline   : Unsigned_64;
      Sampled    : Unsigned_64;
   end record;
   for Thread_Entry use record
      Slot       at 0  range 0 .. 15;
      Reason     at 2  range 0 .. 7;
      Reserved_1 at 3  range 0 .. 7;
      Generation at 4  range 0 .. 31;
      CPU        at 8  range 0 .. 31;
      Reserved_2 at 12 range 0 .. 31;
      Peer       at 16 range 0 .. 63;
      Since      at 24 range 0 .. 63;
      Deadline   at 32 range 0 .. 63;
      Sampled    at 40 range 0 .. 63;
   end record;
   for Thread_Entry'Size use Thread_Entry_Bytes * 8;

   function Reason_Of (T : Process.ThreadID) return Wait_Reason is
      Thr : constant Process.Thread_Table.Element_Ref := Process.threadtab (T);
   begin
      if Process_Lifetime.Executing (Thr.lifetime) then
         return Running;
      end if;
      return
        (case Thr.state is
            when Process.RUNNING => Running,
            when Process.READY => Ready,
            when Process.SENDING | Process.WAITINGFORREPLY => In_Call,
            when Process.RECEIVING | Process.WAITINGFOREVENT => Receiving,
            when Process.WAITINGFORCOMPLETION => Completion_Wait,
            when Process.FUTEXWAITING => Futex,
            when Process.SLEEPING => Sleeping,
            when Process.SUSPENDED => Not_Started,
            when others => Other);
   end Reason_Of;

   function Entry_Of (T : Process.ThreadID) return Thread_Entry is
      Thr : constant Process.Thread_Table.Element_Ref := Process.threadtab (T);
      Reason : constant Wait_Reason := Reason_Of (T);
      In_Call_Now : constant Boolean :=
        Reason = In_Call and then Thr.callTarget /= Process.NO_PROCESS;
   begin
      return
        (Slot       => Unsigned_16 (Thr.countSlot),
         Reason     => Unsigned_8 (Wait_Reason'Enum_Rep (Reason)),
         Reserved_1 => 0,
         Generation => Unsigned_32 (Process.threadGenerationOf (T)),
         CPU        => Unsigned_32 (Thr.cpu),
         Reserved_2 => 0,
         Peer       => (if In_Call_Now then Identity_Word (Thr.callTarget) else 0),
         Since      => (if In_Call_Now then Thr.callStartMs else 0),
         Deadline   =>
           (case Reason is
               when In_Call   =>
                 (if Thr.callDeadlineActive then Thr.callDeadlineMs else No_Deadline),
               when Receiving =>
                 (if Thr.receiveDeadlineActive then Thr.receiveDeadlineMs else No_Deadline),
               when Futex     =>
                 (if Thr.futexDeadlineActive then Thr.futexDeadlineMs else No_Deadline),
               when others    => No_Deadline),
         Sampled    => Thr.sampledRIP);
   end Entry_Of;

   procedure Do_Threads
     (Caller, Target : Process.ProcessID; Destination, Capacity : Unsigned_64;
      What : out Outcome; Written, Live : out Unsigned_64)
   is
      T : Process.ThreadID;
      Maximum : constant Unsigned_64 := Capacity / Thread_Entry_Bytes;
      Copied : Boolean;
   begin
      What := Done;
      Written := 0;
      Live := 0;
      Spinlocks.enterCriticalSection (Process.lock);
      T := Process.mainThreadOf (Target);
      while T /= Process.NO_THREAD loop
         if Process.threadtab (T).state /= Process.INVALID then
            Live := Live + 1;
            if Written < Maximum and then What = Done then
               declare
                  Item : aliased constant Thread_Entry := Entry_Of (T);
               begin
                  Process.User_Memory.Copy_To_User
                    (Caller, Destination + Written * Thread_Entry_Bytes, Item'Address,
                     Thread_Entry_Bytes, Copied);
               end;
               if Copied then
                  Written := Written + 1;
               else
                  What := Destination_Unwritable;
               end if;
            end if;
         end if;
         T := Process.threadtab (T).nextSibling;
      end loop;
      Spinlocks.exitCriticalSection (Process.lock);
   end Do_Threads;

   -- The live user thread of Target with this slot and generation, or none
   -- (Process.lock held).
   function Find_Thread
     (Target : Process.ProcessID; Slot : Unsigned_16; Generation : Unsigned_32)
     return Process.ThreadID
   is
      T : Process.ThreadID := Process.mainThreadOf (Target);
   begin
      while T /= Process.NO_THREAD loop
         if Process.threadtab (T).state /= Process.INVALID
           and then Unsigned_16 (Process.threadtab (T).countSlot) = Slot
           and then Unsigned_32 (Process.threadGenerationOf (T)) = Generation
         then
            return T;
         end if;
         T := Process.threadtab (T).nextSibling;
      end loop;
      return Process.NO_THREAD;
   end Find_Thread;

   type General_Block is array (0 .. General_Words - 1) of Unsigned_64;

   function General_Of (T : Process.ThreadID) return General_Block is
      Thr : constant Process.Thread_Table.Element_Ref := Process.threadtab (T);
      -- The user registers saved at the thread's last entry to the kernel:
      -- SYSCALL entry and every interrupt from ring 3 build this frame at the
      -- top of its kernel stack, and a new thread starts from one.
      F : Stackframe.InterruptStackFrame renames Thr.kernelStack.interruptFrame;
      Block : General_Block := [others => 0];
      Kind : constant Frame_Kind :=
        (if F.errorCode = System_Call_Frame_Tag then System_Call
         elsif F.errorCode = Initial_Frame_Tag then Not_Yet_Run
         else Interrupt);
   begin
      Block (General_Register'Pos (RAX)) := F.rax;
      Block (General_Register'Pos (RBX)) := F.rbx;
      Block (General_Register'Pos (RCX)) := F.rcx;
      Block (General_Register'Pos (RDX)) := F.rdx;
      Block (General_Register'Pos (RSI)) := F.rsi;
      Block (General_Register'Pos (RDI)) := F.rdi;
      Block (General_Register'Pos (RBP)) := F.rbp;
      Block (General_Register'Pos (RSP)) := Unsigned_64 (To_Integer (F.rsp));
      Block (General_Register'Pos (R8))  := F.r8;
      Block (General_Register'Pos (R9))  := F.r9;
      Block (General_Register'Pos (R10)) := F.r10;
      Block (General_Register'Pos (R11)) := F.r11;
      Block (General_Register'Pos (R12)) := F.r12;
      Block (General_Register'Pos (R13)) := F.r13;
      Block (General_Register'Pos (R14)) := F.r14;
      Block (General_Register'Pos (R15)) := F.r15;
      Block (General_Register'Pos (RIP)) := Unsigned_64 (To_Integer (F.rip));
      Block (General_Register'Pos (RFLAGS)) := F.rflags;
      Block (General_Register'Pos (CS)) := F.cs;
      Block (General_Register'Pos (SS)) := F.ss;
      Block (General_Register'Pos (FS_Base)) := Thr.fsBase;
      -- User GS is not supported: the switch path loads zero (threads.md).
      Block (General_Register'Pos (GS_Base)) := 0;
      Block (General_Register'Pos (Saved_Frame)) := Unsigned_64 (Frame_Kind'Enum_Rep (Kind));
      Block (General_Register'Pos (Frame_Detail)) :=
        (if Kind = Not_Yet_Run then 0 else F.interruptNumber);
      Block (General_Register'Pos (Error_Code)) :=
        (if Kind = Interrupt then F.errorCode else 0);
      return Block;
   end General_Of;

   procedure Do_Registers
     (Caller, Target : Process.ProcessID; Selector, Destination, Capacity : Unsigned_64;
      What : out Outcome; Written : out Unsigned_64)
   is
      Set_Word : constant Unsigned_64 := Selector_Set_Word (Selector);
      T : Process.ThreadID;
      Copied : Boolean := False;
      Block : aliased General_Block;
   begin
      Written := 0;
      if Set_Word not in Register_Set'Enum_Rep (General) .. Register_Set'Enum_Rep (Vector)
        or else (Selector and 16#FFFF_FFFF_FF00_0000#) /=
                Shift_Left (Shift_Right (Selector, 32), 32)
      then
         What := Malformed;
         return;
      end if;
      if (Set_Word = Register_Set'Enum_Rep (General) and then Capacity < General_Bytes)
        or else (Set_Word = Register_Set'Enum_Rep (Vector)
                 and then Capacity < Process.XSAVE_AREA_BYTES)
      then
         What := Malformed;
         return;
      end if;
      Spinlocks.enterCriticalSection (Process.lock);
      T := Find_Thread (Target, Selector_Slot (Selector), Selector_Generation (Selector));
      if T = Process.NO_THREAD or else Process.threadtab (T).mode /= Process.USER
        or else Process_Lifetime.Closing (Process.threadtab (T).lifetime)
      then
         Spinlocks.exitCriticalSection (Process.lock);
         What := No_Such_Thread;
         return;
      end if;
      -- Off every CPU (stable while Process.lock is held): its frame and
      -- its XSAVE image are what it will resume with.
      if Process_Lifetime.Executing (Process.threadtab (T).lifetime) then
         Spinlocks.exitCriticalSection (Process.lock);
         What := Running;
         return;
      end if;
      if Set_Word = Register_Set'Enum_Rep (General) then
         Block := General_Of (T);
         Spinlocks.exitCriticalSection (Process.lock);
         Process.User_Memory.Copy_To_User
           (Caller, Destination, Block'Address, General_Bytes, Copied);
         Written := (if Copied then General_Bytes else 0);
      else
         Process.User_Memory.Copy_To_User
           (Caller, Destination, Process.threadtab (T).fpu,
            Process.XSAVE_AREA_BYTES, Copied);
         Spinlocks.exitCriticalSection (Process.lock);
         Written := (if Copied then Process.XSAVE_AREA_BYTES else 0);
      end if;
      What := (if Copied then Done else Destination_Unwritable);
   end Do_Registers;

   ---------------------------------------------------------------------------
   -- Calls
   ---------------------------------------------------------------------------
   procedure Control_Call
     (Caller   : Process.ProcessID;
      Control  : Caps.Capability;
      Request  : Process.Message;
      Reply    : out Process.Message;
      Answered : out Boolean)
   is
      Label  : constant Unsigned_64 := Unsigned_64 (Request.tag.label);
      Length : constant Unsigned_64 := Unsigned_64 (Request.tag.length);
      Wildcard : constant Boolean := Control.object.ref = 0;

      -- Control names Pid (in its current life), or is the wildcard.
      function Covers (Pid : Process.ProcessID) return Boolean is
        (Wildcard or else
           (Control.object.ref = Unsigned_64 (Pid)
            and then Control.gen = Process.generationOf (Pid)));
   begin
      Reply := Process.NULL_MESSAGE;
      Answered := Label in Mint_Debug_Label | Revoke_Debug_Label;
      if not Answered then
         return;
      end if;
      if not Control.rights (Caps.RIGHT_GRANT) then
         Answer (Request, Reply, Not_Permitted);
         return;
      end if;

      if Label = Mint_Debug_Label then
         if Length /= Mint_Request_Words or else not Is_Kind_Word (Request.words (0)) then
            Answer (Request, Reply, Malformed);
            return;
         end if;
         declare
            Kind : constant Debug_Kind := To_Kind (Request.words (0));
            Named : constant Unsigned_64 := Request.words (1);
            Target : Process.ProcessID := Process.NO_PROCESS;
            Slot : Natural := 0;
            Generation : Caps.Generation := 0;
            Identity : Unsigned_64;
            Handle : Unsigned_64;
         begin
            if Wildcard then
               -- The wildcard names the target by its identity (one life).
               Target := Process.processOfIdentity (Process_Identities.From_Word (Named));
            elsif Control.object.ref in 1 .. Unsigned_64 (Process.ProcessID'Last) then
               Target := Process.ProcessID (Control.object.ref);
            else
               Answer (Request, Reply, Malformed);
               return;
            end if;
            if Target = Process.NO_PROCESS or else not Covers (Target)
              or else Process.threadOf (Target).state = Process.INVALID
            then
               Answer (Request, Reply, Target_Gone);
               return;
            elsif not Wildcard and then Named not in 0 | Identity_Word (Target) then
               Answer (Request, Reply, Malformed);
               return;
            elsif Target = Caller or else not Live_User (Target) then
               Answer (Request, Reply, Not_A_Target);
               return;
            end if;
            Identity := Identity_Word (Target);

            Spinlocks.enterCriticalSection (Table_Lock);
            for S in Session_Index loop
               if not Sessions (S).Live and then not Sessions (S).Exhausted then
                  Sessions (S).Live := True;
                  Sessions (S).Kind := Kind;
                  Sessions (S).Target := Target;
                  Sessions (S).Target_Identity := Identity;
                  Sessions (S).Viewer := Process.NO_PROCESS;
                  Sessions (S).Viewer_Identity := 0;
                  Sessions (S).View_High := 0;
                  Slot := S;
                  Generation := Sessions (S).Generation;
                  exit;
               end if;
            end loop;
            Spinlocks.exitCriticalSection (Table_Lock);
            if Slot = 0 then
               Answer (Request, Reply, Capacity);
               return;
            end if;

            Spinlocks.enterCriticalSection (Process.mailtab (Caller).lock);
            Process_Capabilities.Insert
              (Process.proctab (Caller).caps,
               (capType      => Cap_Type (Kind),
                rights       => [Caps.RIGHT_READ | Caps.RIGHT_GRANT => True,
                                 others => False],
                authorityTag => Caps.NO_AUTHORITY_TAG,
                object       => (ref => Unsigned_64 (Slot), param => Identity),
                gen          => Generation), Handle);
            Spinlocks.exitCriticalSection (Process.mailtab (Caller).lock);
            if not Capability_ABI.Is_Handle_Word (Handle) then
               Spinlocks.enterCriticalSection (Table_Lock);
               End_Locked (Slot);
               Spinlocks.exitCriticalSection (Table_Lock);
               Answer (Request, Reply, Capacity, W1 => Handle);
               return;
            end if;
            Answer (Request, Reply, Done, W1 => Handle,
                    W2 => Unsigned_64 (Slot) or Shift_Left (Unsigned_64 (Generation), 32));
         end;
      else
         -- Revoke_Debug
         if Length /= Revoke_Request_Words then
            Answer (Request, Reply, Malformed);
            return;
         end if;
         declare
            Word : constant Unsigned_64 := Request.words (0);
            Slot_Word : constant Unsigned_64 := Word and 16#FFFF_FFFF#;
            Generation : constant Caps.Generation := Caps.Generation (Shift_Right (Word, 32));
            What : Outcome := Revoked;
         begin
            if Slot_Word not in 1 .. Session_Ceiling then
               Answer (Request, Reply, Malformed);
               return;
            end if;
            Spinlocks.enterCriticalSection (Table_Lock);
            declare
               S : constant Session_Index := Session_Index (Slot_Word);
            begin
               if Sessions (S).Live and then Sessions (S).Generation = Generation then
                  if Covers (Sessions (S).Target) then
                     End_Locked (S);
                     What := Done;
                  else
                     What := Not_Permitted;
                  end if;
               end if;
            end;
            Spinlocks.exitCriticalSection (Table_Lock);
            Answer (Request, Reply, What);
         end;
      end if;
   end Control_Call;

   procedure Call
     (Caller  : Process.ProcessID;
      Cap     : Caps.Capability;
      Request : Process.Message;
      Reply   : out Process.Message)
   is
      Label  : constant Unsigned_64 := Unsigned_64 (Request.tag.label);
      Length : constant Unsigned_64 := Unsigned_64 (Request.tag.length);
      What : Outcome;
      W1, W2, W3 : Unsigned_64 := 0;
   begin
      Reply := Process.NULL_MESSAGE;
      if Cap.object.ref not in 1 .. Session_Ceiling then
         Answer (Request, Reply, Revoked);
         return;
      end if;
      Spinlocks.enterCriticalSection (Table_Lock);
      declare
         S : constant Session_Index := Session_Index (Cap.object.ref);
         Session : Session_State renames Sessions (S);
         F : constant Debug_Admission.Facts :=
           (Live           => Session.Live,
            Same_Life      => Session.Generation = Cap.gen
                              and then Cap.object.param = Session.Target_Identity,
            Same_Kind      => Session.Kind = Kind_Of (Cap.capType),
            Target_Current => Session.Live and then Live_User (Session.Target)
                              and then Identity_Word (Session.Target) = Session.Target_Identity,
            Kind           => Session.Kind);
      begin
         What := Debug_Admission.Decide (F, Label, Length);
         if What = Done then
            case To_Operation (Label) is
               when Describe =>
                  W1 := Kind_Word (Session.Kind);
                  W2 := Session.Target_Identity;
                  W3 := Slice_Base (S);
               when Detach =>
                  End_Locked (S);
               when Threads =>
                  Do_Threads (Caller, Session.Target, Request.words (0), Request.words (1),
                           What, W1, W2);
               when Registers =>
                  Do_Registers (Caller, Session.Target, Request.words (0), Request.words (1),
                             Request.words (2), What, W1);
               when Map_View | Unmap_View =>
                  declare
                     View : constant Unsigned_64 := Request.words (1);
                     Pages : constant Unsigned_64 :=
                       (if To_Operation (Label) = Map_View then Request.words (2)
                        else Request.words (1));
                     First_View : constant Unsigned_64 :=
                       (if To_Operation (Label) = Map_View then View else Request.words (0));
                  begin
                     if Pages not in 1 .. Map_Pages_Ceiling then
                        What := Malformed;
                     elsif not Debug_Admission.View_Admitted (S, First_View, Pages) then
                        What := Outside_Slice;
                     elsif Session.Viewer /= Process.NO_PROCESS
                       and then (Session.Viewer /= Caller
                                 or else Session.Viewer_Identity /= Identity_Word (Caller))
                     then
                        What := Not_Viewer;
                     elsif To_Operation (Label) = Unmap_View then
                        if Session.Viewer = Process.NO_PROCESS then
                           W1 := 0;
                        else
                           declare
                              Removed : Natural;
                           begin
                              Remove_Batch (Caller, First_View, Natural (Pages), Removed);
                              W1 := Unsigned_64 (Removed);
                           end;
                        end if;
                     elsif not Debug_Admission.Target_Admitted (Request.words (0), Pages) then
                        What := Malformed;
                     else
                        if Session.Viewer = Process.NO_PROCESS then
                           Session.Viewer := Caller;
                           Session.Viewer_Identity := Identity_Word (Caller);
                        end if;
                        declare
                           Refusal : Map_Refusal := None;
                           Occupied_Page : Boolean := False;
                           Base_Page : constant Natural :=
                             Natural ((First_View - Slice_Base (S)) / Page_Bytes);
                        begin
                           for I in 0 .. Pages - 1 loop
                              -- Counted before the attempt: a sweep then
                              -- always covers a page that may be mapped.
                              Session.View_High :=
                                Natural'Max (Session.View_High, Base_Page + Natural (I) + 1);
                              Map_Page
                                (Caller, Session.Target, Request.words (0) + I * Page_Bytes,
                                 First_View + I * Page_Bytes, Refusal, Occupied_Page);
                              exit when Refusal /= None or else Occupied_Page;
                              W1 := W1 + 1;
                           end loop;
                           W2 := Unsigned_64 (Map_Refusal'Enum_Rep (Refusal));
                           if Occupied_Page then
                              What := Occupied;
                           end if;
                        end;
                     end if;
                  end;
            end case;
         end if;
      end;
      Spinlocks.exitCriticalSection (Table_Lock);
      Answer (Request, Reply, What, W1, W2, W3);
   end Call;

   procedure Process_Ended (Pid : Process.ProcessID) is
   begin
      if Pid = Process.NO_PROCESS then
         return;
      end if;
      Spinlocks.enterCriticalSection (Table_Lock);
      for S in Session_Index loop
         if Sessions (S).Live
           and then (Sessions (S).Target = Pid or else Sessions (S).Viewer = Pid)
         then
            End_Locked (S);
         end if;
      end loop;
      Spinlocks.exitCriticalSection (Table_Lock);
   end Process_Ended;
end Debug_Sessions;
