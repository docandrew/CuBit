package Process.User_Memory is
   -- Reads the currently executing caller's own normal user RAM, or its
   -- explicit readable mapping of permanently reserved initrd bytes. Received
   -- grants, large pages and MMIO are not accepted by this initial interface.
   -- A failure can leave a copied prefix in Destination; callers must discard
   -- that output. No user virtual address is directly dereferenced.
   procedure Copy
     (Caller : ProcessID; Source : Unsigned_64; Destination : System.Address;
      Length : Storage_Count; Success : out Boolean);

   -- Writes Length bytes from Source (kernel memory) to the currently
   -- executing caller's own normal user RAM at Destination, through its
   -- page tables: every page must be present, user-accessible, writable and
   -- owned by the caller (not a received grant, not MMIO, not the kernel).
   -- No user virtual address is directly dereferenced. A failure can leave
   -- a written prefix (IPC syscalls, docs/ipc-fastpath.md, "User memory").
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
   -- normal user RAM (futex words, thread-exit words). The address must be
   -- 4-byte aligned and outside the grant aperture; a store also requires a
   -- user-writable mapping. Nothing is dereferenced through user mappings.
   procedure Load_Word32
     (Caller : ProcessID; Address : Unsigned_64; Value : out Unsigned_32;
      Success : out Boolean);
   procedure Store_Word32
     (Caller : ProcessID; Address : Unsigned_64; Value : Unsigned_32;
      Success : out Boolean);
end Process.User_Memory;
