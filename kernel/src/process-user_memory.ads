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
end Process.User_Memory;
