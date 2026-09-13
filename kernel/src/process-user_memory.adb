pragma Ada_2022;
with PerCPUData;
with User_Buffer_Copy;
with User_Page_Walk;
with Modules;

-- Physical overlays and frame-pin ownership are the trusted implementation
-- boundary. The shared traversal/chunking algorithms have host tests/proofs.
package body Process.User_Memory is
   procedure Copy
     (Caller : ProcessID; Source : Unsigned_64; Destination : System.Address;
      Length : Storage_Count; Success : out Boolean)
   is
      procedure Read_Entry
        (Table_Frame : Unsigned_64; Index : User_Page_Walk.Table_Index;
         Word : out Unsigned_64)
      is
         Entry_Word : Unsigned_64 with Import, Atomic,
           Address => Virtmem.P2Va (Virtmem.PhysAddress (Table_Frame)) +
                      Storage_Offset (Index * 8);
      begin
         Word := Entry_Word;
      end Read_Entry;
      function Readable_Frame is new User_Page_Walk.Readable_Frame (Read_Entry);

      procedure Read_Chunk
        (Page : Unsigned_64; Within_Page : Natural; Destination_Offset : Unsigned_64;
         Count : Positive; Success : out Boolean)
      is
         Frame : Unsigned_64;
         Pinned, Released : Boolean;
      begin
         Success := False;
         if Page >= Unsigned_64 (GRANT_REGION_BASE) and then
           Page < Unsigned_64 (GRANT_REGION_END) then return; end if;
         Frame := Readable_Frame
           (Unsigned_64 (Virtmem.K2P (addrtab(proctab(Caller).pgTable)'Address)),
            Page, Unsigned_64 (Virtmem.MAX_PHYS_USABLE));
         if Frame = 0 then return; end if;
         BuddyAllocator.pinOwnedFrame (Virtmem.PhysAddress (Frame), Unsigned_8 (Caller), Pinned);
         -- The initrd is permanently reserved boot RAM, explicitly mapped
         -- read-only into devmgr. It has no process owner to pin; unlike MMIO
         -- or arbitrary foreign frames it cannot disappear during this copy.
         if not Pinned and then not Modules.residentInitrdRange
           (Integer_Address (Frame) + Integer_Address (Within_Page), Storage_Count (Count))
         then return; end if;
         Util.memCopy (Destination + Storage_Offset (Destination_Offset),
                       Virtmem.P2Va (Virtmem.PhysAddress (Frame)) + Storage_Offset (Within_Page),
                       Storage_Count (Count));
         if Pinned then
            BuddyAllocator.unpinFrame (Virtmem.PhysAddress (Frame), Released);
            if not Released then
               raise ProcessException with "User-copy frame pin lost";
            end if;
         end if;
         Success := True;
      end Read_Chunk;
      procedure Copy_Chunks is new User_Buffer_Copy.Copy (Read_Chunk);
   begin
      Success := False;
      -- The caller's execution pin protects its page tables from retirement.
      -- Never use this interface to inspect an unpinned, remote address space.
      if Caller = NO_PROCESS or else Caller /= PerCPUData.getCurrentPID or else
        proctab(Caller).isThread or else proctab(Caller).pgTable = NO_PROCESS then
         return;
      end if;
      Copy_Chunks (Source, Unsigned_64 (Length), Success);
   end Copy;

   procedure Copy_Name
     (Caller : ProcessID; Source : Unsigned_64; Name : out ProcessName;
      Success : out Boolean)
   is
      procedure Read_Byte
        (Address : Unsigned_64; Value : out Character; Success : out Boolean)
      is
         Byte : aliased Character;
      begin
         Copy (Caller, Address, Byte'Address, 1, Success);
         Value := (if Success then Byte else ASCII.NUL);
      end Read_Byte;
      procedure Read_Name is new User_Buffer_Copy.Copy_Name (Read_Byte);
   begin
      Read_Name (Source, Name, Success);
   end Copy_Name;
end Process.User_Memory;
