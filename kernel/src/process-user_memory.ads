package Process.User_Memory is
   -- Reads the currently executing caller's own normal user RAM, or its
   -- explicit readable mapping of permanently reserved initrd bytes. Received
   -- grants, large pages and MMIO are not accepted by this initial interface.
   -- A failure can leave a copied prefix in Destination; callers must discard
   -- that output. No user virtual address is directly dereferenced.
   procedure Copy
     (Caller : ProcessID; Source : Unsigned_64; Destination : System.Address;
      Length : Storage_Count; Success : out Boolean);

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
