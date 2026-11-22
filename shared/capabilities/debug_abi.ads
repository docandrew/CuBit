------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Debug sessions (DBG-001, docs/debugging.md): four tightly scoped debug
--  capabilities, minted only from a target's Control, and the kernel
--  debug-session object they name. Shared by the kernel (Debug_Sessions)
--  and the runtime.
--
--  @description
--  Kinds. Debug_Observe, Debug_Pause, Debug_Modify and Debug_Instrument are
--  four capability kinds (Capability_ABI.Kind codes 19 .. 22), not one kind
--  with a rights mask. Each mint makes a session of its own, with its own
--  generation, so each is revoked on its own. Shielded memory is never
--  readable through any of them. In this slice only Debug_Observe's
--  operations run; the other kinds' operations answer Not_Yet.
--
--  Minting and revoking: a Call on a CAP_PROCESS handle that carries
--  RIGHT_GRANT, naming the target (a per-child Control) or carrying the
--  wildcard (procmgr's root) and naming the target's identity:
--    Mint_Debug   words (0) = Debug_Kind'Enum_Rep, words (1) = the target's
--                 identity word (Process_Identities; CuBit.Process_IDs) with
--                 the wildcard; 0 or that identity with a per-child Control.
--                 Reply words (0) = Outcome, (1) = the new handle word,
--                 (2) = the session word (for Revoke_Debug).
--    Revoke_Debug words (0) = a session word from Mint_Debug. Reply
--                 words (0) = Outcome. Every copy of that session's
--                 capability goes stale, and its views are removed.
--
--  Session operations: a Call on a debug handle, label = Operation:
--    Describe    reply (1) = Debug_Kind, (2) = the target's identity word,
--                (3) = this session's view slice in the viewer.
--    Threads     (0) = destination, (1) = its capacity in bytes; reply
--                (1) = Thread_Entry records written (Thread_Entry_Bytes
--                each), (2) = the target's live threads.
--    Registers   (0) = Thread_Selector (slot, generation, Register_Set),
--                (1) = destination, (2) = capacity; reply (1) = bytes
--                written. General: General_Bytes; Vector: the XSAVE image.
--    Map_View    (0) = target address, (1) = view address in this session's
--                slice, (2) = pages (1 .. Map_Pages_Ceiling); reply
--                (1) = pages mapped, (2) = Map_Refusal of the next page
--                when fewer were mapped than asked.
--    Unmap_View  (0) = view address, (1) = pages; reply (1) = pages removed.
--    Detach      ends this session (any kind): as Revoke_Debug.
--  A view maps the target's own frames read-only and never executable into
--  the viewer: the kernel never maps them itself (KERN-014). The first
--  process to map through a session is its viewer; no other may.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Debug_ABI with Pure, SPARK_Mode is

   type Debug_Kind is (Observe, Pause, Modify, Instrument);
   for Debug_Kind use (Observe => 1, Pause => 2, Modify => 3, Instrument => 4);

   function Kind_Word (Kind : Debug_Kind) return Unsigned_64 is
     (Unsigned_64 (Debug_Kind'Enum_Rep (Kind)));

   function Is_Kind_Word (Word : Unsigned_64) return Boolean is
     (Word in Kind_Word (Debug_Kind'First) .. Kind_Word (Debug_Kind'Last));

   function To_Kind (Word : Unsigned_64) return Debug_Kind is
     (case Word is
         when 1 => Observe, when 2 => Pause, when 3 => Modify, when others => Instrument)
   with Pre  => Is_Kind_Word (Word),
        Post => Kind_Word (To_Kind'Result) = Word;

   ---------------------------------------------------------------------------
   --  Labels. Control operations go on a CAP_PROCESS handle; session
   --  operations on a debug handle. Codes are fixed.
   ---------------------------------------------------------------------------
   Mint_Debug_Label   : constant := 16#0D01#;
   Revoke_Debug_Label : constant := 16#0D02#;

   type Operation is (Describe, Threads, Registers, Map_View, Unmap_View, Detach);
   for Operation use
     (Describe => 16#0D10#, Threads => 16#0D11#, Registers => 16#0D12#,
      Map_View => 16#0D13#, Unmap_View => 16#0D14#, Detach => 16#0D15#);

   function Label_Of (Op : Operation) return Unsigned_64 is
     (Unsigned_64 (Operation'Enum_Rep (Op)));

   function Is_Operation_Label (Label : Unsigned_64) return Boolean is
     (Label in Label_Of (Operation'First) .. Label_Of (Operation'Last));

   function To_Operation (Label : Unsigned_64) return Operation is
     (case Label is
         when 16#0D10# => Describe, when 16#0D11# => Threads,
         when 16#0D12# => Registers, when 16#0D13# => Map_View,
         when 16#0D14# => Unmap_View, when others => Detach)
   with Pre  => Is_Operation_Label (Label),
        Post => Label_Of (To_Operation'Result) = Label;

   --  Words each operation's request carries (MessageTag.length).
   function Request_Words (Op : Operation) return Unsigned_64 is
     (case Op is
         when Describe | Detach => 0,
         when Threads | Unmap_View => 2,
         when Registers | Map_View => 3);

   Mint_Request_Words   : constant := 2;
   Revoke_Request_Words : constant := 1;
   Reply_Words          : constant := 4;
   Outcome_Word         : constant := 0;

   ---------------------------------------------------------------------------
   --  Outcomes. Codes start at 1 and are fixed.
   ---------------------------------------------------------------------------
   type Outcome is
     (Done,                    --  the operation ran
      Malformed,               --  a word count or argument out of range
      Unknown_Operation,       --  the label is no operation
      Not_Permitted,           --  the handle lacks what the operation needs
      Not_Yet,                 --  this kind's operations are not built yet
      Target_Gone,             --  the target exited or was replaced
      Revoked,                 --  the session was revoked or detached
      Not_A_Target,            --  the caller itself, or a kernel thread
      Capacity,                --  the session table is at its ceiling
      Destination_Unwritable,  --  the buffer is not the caller's writable memory
      Running,                 --  the thread is on a CPU now: ask again
      No_Such_Thread,          --  no live thread has that slot and generation
      Not_Viewer,              --  another process maps through this session
      Outside_Slice,           --  the view address is not this session's slice
      Occupied,                --  a view page there is already mapped
      Not_Mapped);             --  a page to remove holds no view
   for Outcome use
     (Done => 1, Malformed => 2, Unknown_Operation => 3, Not_Permitted => 4,
      Not_Yet => 5, Target_Gone => 6, Revoked => 7, Not_A_Target => 8,
      Capacity => 9, Destination_Unwritable => 10, Running => 11,
      No_Such_Thread => 12, Not_Viewer => 13, Outside_Slice => 14,
      Occupied => 15, Not_Mapped => 16);

   function Outcome_Word_Of (What : Outcome) return Unsigned_64 is
     (Unsigned_64 (Outcome'Enum_Rep (What)));

   --  Why Map_View stopped before the page it names.
   type Map_Refusal is
     (None,            --  every page was mapped
      Unmapped,        --  the target maps nothing there (or not user-accessible)
      --  Not_Owned: not the target's own RAM (device memory, a kernel
      --  page, or a page another process granted it).
      Not_Owned,
      Not_Write_Back,  --  mapped uncached or write-combining in the target
      Pin_Limit,       --  the frame carries its most pins
      Install_Failed); --  no page table for the view (out of memory)
   for Map_Refusal use
     (None => 0, Unmapped => 1, Not_Owned => 2, Not_Write_Back => 3,
      Pin_Limit => 4, Install_Failed => 5);

   ---------------------------------------------------------------------------
   --  Sessions and the view aperture.
   ---------------------------------------------------------------------------
   Session_Ceiling   : constant := 256;
   subtype Session_Index is Natural range 1 .. Session_Ceiling;

   Page_Bytes        : constant := 4096;
   --  Pages one session may have mapped at once, and one Map_View maps.
   View_Page_Ceiling : constant := 16_384;
   Map_Pages_Ceiling : constant := 64;
   Slice_Bytes       : constant := View_Page_Ceiling * Page_Bytes;   --  64 MiB

   --  Every process's view aperture: one slice per session, below the
   --  device-mapping arena (Process_Layout.Device_Mapping_First) and above
   --  the publication pages. Kernel copies refuse it (User_Access_Ranges).
   Aperture_First    : constant := 16#0000_5C00_0000_0000#;
   Aperture_Limit    : constant := Aperture_First + Session_Ceiling * Slice_Bytes;

   function Slice_Base (S : Session_Index) return Unsigned_64 is
     (Aperture_First + Unsigned_64 (S - 1) * Slice_Bytes)
   with Post => Slice_Base'Result >= Aperture_First
                and then Slice_Base'Result <= Aperture_Limit - Slice_Bytes;

   ---------------------------------------------------------------------------
   --  Threads: one entry per live thread.
   ---------------------------------------------------------------------------
   Thread_Entry_Bytes : constant := 48;

   --  What a thread is doing (a snapshot).
   type Wait_Reason is
     (Running,          --  on a CPU
      Ready,            --  runnable, waiting for a CPU
      In_Call,          --  waiting for a server's reply (peer, since, deadline)
      Receiving,        --  waiting for a message or event (deadline)
      Completion_Wait,  --  waiting for an asynchronous completion
      Futex,            --  waiting on a futex word (deadline)
      Sleeping,         --  waiting for a time
      Not_Started,      --  created, not yet resumed
      Other);           --  any other kernel wait
   for Wait_Reason use
     (Running => 1, Ready => 2, In_Call => 3, Receiving => 4,
      Completion_Wait => 5, Futex => 6, Sleeping => 7, Not_Started => 8,
      Other => 9);

   --  Thread_Entry layout (little-endian):
   --    0  slot (16 bits)      2  Wait_Reason (8)   3  reserved (8)
   --    4  generation (32)     8  CPU (32)         12  reserved (32)
   --   16  peer identity (64): the server of a call, else 0
   --   24  since (ms since boot, 64): when the call began, else 0
   --   32  deadline (ms since boot, 64): Unsigned_64'Last when none
   --   40  sampled user RIP at its last timer interrupt (64)
   No_Deadline : constant Unsigned_64 := Unsigned_64'Last;

   ---------------------------------------------------------------------------
   --  Registers.
   ---------------------------------------------------------------------------
   type Register_Set is (General, Vector);
   for Register_Set use (General => 1, Vector => 2);

   --  Thread_Selector: slot in bits 0 .. 15, Register_Set in bits 16 .. 23,
   --  the thread's generation in bits 32 .. 63.
   function Selector (Slot : Unsigned_16; Set : Register_Set; Generation : Unsigned_32)
     return Unsigned_64 is
     (Unsigned_64 (Slot)
      or Shift_Left (Unsigned_64 (Register_Set'Enum_Rep (Set)), 16)
      or Shift_Left (Unsigned_64 (Generation), 32));

   function Selector_Slot (Word : Unsigned_64) return Unsigned_16 is
     (Unsigned_16 (Word and 16#FFFF#));
   function Selector_Generation (Word : Unsigned_64) return Unsigned_32 is
     (Unsigned_32 (Shift_Right (Word, 32)));
   function Selector_Set_Word (Word : Unsigned_64) return Unsigned_64 is
     (Shift_Right (Word, 16) and 16#FF#);

   --  General block: General_Words 64-bit words, in this order.
   type General_Register is
     (RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP, R8, R9, R10, R11, R12, R13, R14, R15,
      RIP, RFLAGS, CS, SS, FS_Base, GS_Base, Saved_Frame, Frame_Detail, Error_Code);
   General_Words : constant := 32;
   General_Bytes : constant := General_Words * 8;

   --  Where the registers were saved, in the General block's Saved_Frame word.
   --  A system call's RAX is its call number until the call returns; its
   --  RCX and R11 hold RIP and RFLAGS, as SYSCALL left them.
   type Frame_Kind is (System_Call, Interrupt, Not_Yet_Run);
   for Frame_Kind use (System_Call => 1, Interrupt => 2, Not_Yet_Run => 3);

   --  The error-code word of a saved frame that SYSCALL entry built, and of a
   --  thread's first frame (Stackframe.InterruptStackFrame; no CPU error
   --  code reaches 2 ** 31).
   System_Call_Frame_Tag : constant := 16#5359_5343#;   --  "SYSC"
   Initial_Frame_Tag     : constant := 16#494E_4954#;   --  "INIT"

end Debug_ABI;
