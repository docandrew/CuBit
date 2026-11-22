package Process.User_Memory is
   -- The currently executing caller's own memory, through its own mapping
   -- (KERN-014 slice 2): never through the kernel's linear map, and with no
   -- page walk or frame pin. The range must lie in the user half and outside
   -- the received-grant aperture (User_Access_Ranges); the hardware checks
   -- the rest: every page present and user-accessible, and writable for a
   -- store (CR0.WP). A page fault is recovered and refused (interrupt
   -- handlers' fault-resume table). MMIO and large pages the caller maps are
   -- accepted like its other pages: it can reach them anyway.
   --
   -- Reads Length bytes at Source into Destination (kernel memory). A
   -- failure can leave a copied prefix in Destination; callers must discard
   -- that output.
   procedure Copy
     (Caller : ProcessID; Source : Unsigned_64; Destination : System.Address;
      Length : Storage_Count; Success : out Boolean);

   -- Writes Length bytes from Source (kernel memory) to the caller's memory
   -- at Destination, as Copy. A failure can leave a written prefix (IPC
   -- syscalls, docs/ipc-fastpath.md, "User memory").
   procedure Copy_To_User
     (Caller : ProcessID; Destination : Unsigned_64; Source : System.Address;
      Length : Storage_Count; Success : out Boolean);

   -- Whether Destination .. Destination + Length - 1 is writable as
   -- Copy_To_User requires, now (a check before taking a message that the
   -- result could be delivered; nothing is written).
   function Writable_Range
     (Caller : ProcessID; Destination : Unsigned_64; Length : Storage_Count)
     return Boolean;

   -- A Message (or a CompletionEntry) to or from the caller's user memory,
   -- through the checks above: what every IPC system call uses for its
   -- user pointer.
   procedure Read_Message
     (Caller : ProcessID; Source : Unsigned_64; Value : out Message; Success : out Boolean);
   procedure Write_Message
     (Caller : ProcessID; Destination : Unsigned_64; Value : Message; Success : out Boolean);
   function Message_Writable (Caller : ProcessID; Destination : Unsigned_64) return Boolean;
   procedure Write_Completion
     (Caller : ProcessID; Destination : Unsigned_64; Value : CompletionEntry;
      Success : out Boolean);

   procedure Copy_Name
     (Caller : ProcessID; Source : Unsigned_64; Name : out ProcessName;
      Success : out Boolean);

   -- Atomic 32-bit access to one aligned word of the calling process's own
   -- memory (futex words, thread-exit words), as Copy: 4-byte aligned and
   -- outside the grant aperture; a store needs a user-writable mapping.
   procedure Load_Word32
     (Caller : ProcessID; Address : Unsigned_64; Value : out Unsigned_32;
      Success : out Boolean);
   procedure Store_Word32
     (Caller : ProcessID; Address : Unsigned_64; Value : Unsigned_32;
      Success : out Boolean);

   --  A shared futex word (IPC-010): the caller's 32-bit word at Address
   --  in a grant Owner owns, read through the caller's own mapping (its
   --  owned pages, or its received window when it is the grantee). The
   --  frame the caller's tables name must be Owner's (checked and pinned
   --  around the read); Process.IPC.resolveSharedWord checked the caller is
   --  a party of the live grant and Address is inside it.
   procedure Load_Shared_Word32
     (Caller : ProcessID; Owner : ProcessID; Address : Unsigned_64;
      Value : out Unsigned_32; Success : out Boolean);

   -- Kernel accesses to user memory that faulted and were refused since
   -- boot (the fault-resume path). Diagnostics and tests.
   function Recovered_Faults return Unsigned_64;
end Process.User_Memory;
