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
           (Unsigned_64 (rootPhysical (proctab(Caller).pgTable)),
            Page, Unsigned_64 (Virtmem.MAX_PHYS_USABLE));
         if Frame = 0 then return; end if;
         BuddyAllocator.pinOwnedFrame (Virtmem.PhysAddress (Frame), BuddyAllocator.Frame_Owner (Caller), Pinned);
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
        proctab(Caller).pgTable = NO_PROCESS then
         return;
      end if;
      Copy_Chunks (Source, Unsigned_64 (Length), Success);
   end Copy;

   procedure Read_Table_Entry
     (Table_Frame : Unsigned_64; Index : User_Page_Walk.Table_Index;
      Word : out Unsigned_64);

   -- The caller's writable, owned frame for Page (0 if none), pinned; and
   -- its release. Shared by Copy_To_User and Writable_Range.
   procedure Pin_Writable_Page (Caller : ProcessID; Page : Unsigned_64; Frame : out Unsigned_64) is
      function Writable is new User_Page_Walk.Writable_Frame (Read_Table_Entry);
      Pinned : Boolean;
   begin
      Frame := 0;
      if Page >= Unsigned_64 (GRANT_REGION_BASE) and then
        Page < Unsigned_64 (GRANT_REGION_END) then return; end if;
      Frame := Writable
        (Unsigned_64 (rootPhysical (proctab(Caller).pgTable)),
         Page, Unsigned_64 (Virtmem.MAX_PHYS_USABLE));
      if Frame = 0 then return; end if;
      -- Only the caller's own frames: never a foreign or reserved one.
      BuddyAllocator.pinOwnedFrame (Virtmem.PhysAddress (Frame), BuddyAllocator.Frame_Owner (Caller), Pinned);
      if not Pinned then
         Frame := 0;
      end if;
   end Pin_Writable_Page;

   procedure Unpin_Page (Frame : Unsigned_64) is
      Released : Boolean;
   begin
      BuddyAllocator.unpinFrame (Virtmem.PhysAddress (Frame), Released);
      if not Released then
         raise ProcessException with "User-copy frame pin lost";
      end if;
   end Unpin_Page;

   function Caller_Ready (Caller : ProcessID) return Boolean is
     (Caller /= NO_PROCESS and then Caller = PerCPUData.getCurrentPID and then
      proctab(Caller).pgTable /= NO_PROCESS);

   procedure Copy_To_User
     (Caller : ProcessID; Destination : Unsigned_64; Source : System.Address;
      Length : Storage_Count; Success : out Boolean)
   is
      procedure Write_Chunk
        (Page : Unsigned_64; Within_Page : Natural; Source_Offset : Unsigned_64;
         Count : Positive; Success : out Boolean)
      is
         Frame : Unsigned_64;
      begin
         Pin_Writable_Page (Caller, Page, Frame);
         Success := Frame /= 0;
         if not Success then return; end if;
         Util.memCopy (Virtmem.P2Va (Virtmem.PhysAddress (Frame)) + Storage_Offset (Within_Page),
                       Source + Storage_Offset (Source_Offset),
                       Storage_Count (Count));
         Unpin_Page (Frame);
      end Write_Chunk;
      procedure Copy_Chunks is new User_Buffer_Copy.Copy (Write_Chunk);
   begin
      Success := False;
      if not Caller_Ready (Caller) then
         return;
      end if;
      Copy_Chunks (Destination, Unsigned_64 (Length), Success);
   end Copy_To_User;

   function Writable_Range
     (Caller : ProcessID; Destination : Unsigned_64; Length : Storage_Count)
     return Boolean
   is
      procedure Probe_Chunk
        (Page : Unsigned_64; Within_Page : Natural; Source_Offset : Unsigned_64;
         Count : Positive; Success : out Boolean)
      is
         pragma Unreferenced (Within_Page, Source_Offset, Count);
         Frame : Unsigned_64;
      begin
         Pin_Writable_Page (Caller, Page, Frame);
         Success := Frame /= 0;
         if Success then
            Unpin_Page (Frame);
         end if;
      end Probe_Chunk;
      procedure Probe_Chunks is new User_Buffer_Copy.Copy (Probe_Chunk);
      Success : Boolean;
   begin
      if not Caller_Ready (Caller) then
         return False;
      end if;
      Probe_Chunks (Destination, Unsigned_64 (Length), Success);
      return Success;
   end Writable_Range;

   procedure Read_Table_Entry
     (Table_Frame : Unsigned_64; Index : User_Page_Walk.Table_Index;
      Word : out Unsigned_64)
   is
      Entry_Word : Unsigned_64 with Import, Atomic,
        Address => Virtmem.P2Va (Virtmem.PhysAddress (Table_Frame)) +
                   Storage_Offset (Index * 8);
   begin
      Word := Entry_Word;
   end Read_Table_Entry;

   -- Resolve and pin the frame holding an aligned user word, or 0.
   generic
      with function Frame_Of (Root, Address, Physical_Last : Unsigned_64)
        return Unsigned_64;
   procedure Pin_Word_Frame
     (Caller : ProcessID; Address : Unsigned_64; Frame : out Unsigned_64);

   procedure Pin_Word_Frame
     (Caller : ProcessID; Address : Unsigned_64; Frame : out Unsigned_64)
   is
      Pinned : Boolean;
   begin
      Frame := 0;
      if Caller = NO_PROCESS or else Caller /= PerCPUData.getCurrentPID or else
        proctab(Caller).pgTable = NO_PROCESS or else Address mod 4 /= 0 or else
        Address >= User_Page_Walk.User_Limit or else
        (Address >= Unsigned_64 (GRANT_REGION_BASE) and then
         Address < Unsigned_64 (GRANT_REGION_END))
      then
         return;
      end if;
      Frame := Frame_Of
        (Unsigned_64 (rootPhysical (proctab(Caller).pgTable)),
         Address, Unsigned_64 (Virtmem.MAX_PHYS_USABLE));
      if Frame = 0 then
         return;
      end if;
      -- Only the caller's own frames: never MMIO or another owner's pages.
      BuddyAllocator.pinOwnedFrame
        (Virtmem.PhysAddress (Frame), BuddyAllocator.Frame_Owner (Caller), Pinned);
      if not Pinned then
         Frame := 0;
      end if;
   end Pin_Word_Frame;

   procedure Unpin_Word_Frame (Frame : Unsigned_64) is
      Released : Boolean;
   begin
      BuddyAllocator.unpinFrame (Virtmem.PhysAddress (Frame), Released);
      if not Released then
         raise ProcessException with "User-word frame pin lost";
      end if;
   end Unpin_Word_Frame;

   procedure Load_Word32
     (Caller : ProcessID; Address : Unsigned_64; Value : out Unsigned_32;
      Success : out Boolean)
   is
      function Readable is new User_Page_Walk.Readable_Frame (Read_Table_Entry);
      procedure Pin is new Pin_Word_Frame (Readable);
      Frame : Unsigned_64;
   begin
      Value := 0;
      Success := False;
      Pin (Caller, Address, Frame);
      if Frame = 0 then
         return;
      end if;
      declare
         Word : Unsigned_32 with Import, Atomic,
           Address => Virtmem.P2Va (Virtmem.PhysAddress (Frame)) +
                      Storage_Offset (Address mod User_Page_Walk.Page_Size);
      begin
         Value := Word;
      end;
      Unpin_Word_Frame (Frame);
      Success := True;
   end Load_Word32;

   procedure Store_Word32
     (Caller : ProcessID; Address : Unsigned_64; Value : Unsigned_32;
      Success : out Boolean)
   is
      function Writable is new User_Page_Walk.Writable_Frame (Read_Table_Entry);
      procedure Pin is new Pin_Word_Frame (Writable);
      Frame : Unsigned_64;
   begin
      Success := False;
      Pin (Caller, Address, Frame);
      if Frame = 0 then
         return;
      end if;
      declare
         Word : Unsigned_32 with Import, Atomic,
           Address => Virtmem.P2Va (Virtmem.PhysAddress (Frame)) +
                      Storage_Offset (Address mod User_Page_Walk.Page_Size);
      begin
         Word := Value;
      end;
      Unpin_Word_Frame (Frame);
      Success := True;
   end Store_Word32;

   Message_Bytes : constant Storage_Count := Message'Max_Size_In_Storage_Elements;

   procedure Read_Message
     (Caller : ProcessID; Source : Unsigned_64; Value : out Message; Success : out Boolean)
   is
      Local : aliased Message := NULL_MESSAGE;
   begin
      Copy (Caller, Source, Local'Address, Message_Bytes, Success);
      Value := (if Success then Local else NULL_MESSAGE);
   end Read_Message;

   procedure Write_Message
     (Caller : ProcessID; Destination : Unsigned_64; Value : Message; Success : out Boolean)
   is
      Local : aliased constant Message := Value;
   begin
      Copy_To_User (Caller, Destination, Local'Address, Message_Bytes, Success);
   end Write_Message;

   function Message_Writable (Caller : ProcessID; Destination : Unsigned_64) return Boolean is
     (Writable_Range (Caller, Destination, Message_Bytes));

   procedure Write_Completion
     (Caller : ProcessID; Destination : Unsigned_64; Value : CompletionEntry;
      Success : out Boolean)
   is
      Local : aliased constant CompletionEntry := Value;
   begin
      Copy_To_User (Caller, Destination, Local'Address,
                    CompletionEntry'Max_Size_In_Storage_Elements, Success);
   end Write_Completion;

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
