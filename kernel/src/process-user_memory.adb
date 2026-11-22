pragma Ada_2022;
with PerCPUData;
with User_Page_Walk;
with User_Buffer_Copy;
with User_Access_Ranges;
with Debug_ABI;
with Address_Space_Tags;
with TextIO;
with x86;

-- KERN-014 slice 2: the current process's memory is read and written
-- through its own mapping (cubitCopyFromUser and friends,
-- interrupt_handlers.asm), never through the kernel's linear map. The
-- hardware checks the access (present, user, writable under CR0.WP); a fault
-- is recovered and refused. The range arithmetic is proved
-- (User_Access_Ranges); the assembly clamps again against speculation.
package body Process.User_Memory is
   Faults : Unsigned_64 := 0
     with Atomic, Export, Convention => C, External_Name => "cubitUserAccessFaults";

   function Copy_From_User (Destination, Source : System.Address; Bytes : Storage_Count)
     return Unsigned_32
     with Import, Convention => C, External_Name => "cubitCopyFromUser";
   function Copy_To_User_Raw (Destination, Source : System.Address; Bytes : Storage_Count)
     return Unsigned_32
     with Import, Convention => C, External_Name => "cubitCopyToUser";
   function User_Load32 (Address : System.Address; Value : System.Address)
     return Unsigned_32
     with Import, Convention => C, External_Name => "cubitUserLoad32";
   function User_Store32 (Address : System.Address; Value : Unsigned_32)
     return Unsigned_32
     with Import, Convention => C, External_Name => "cubitUserStore32";

   function Recovered_Faults return Unsigned_64 is (Faults);

   Space_Mismatch_Reported : Boolean := False with Atomic;

   -- The caller is the process running here, with its own tables loaded:
   -- its user addresses mean its memory and nothing else.
   function Caller_Ready (Caller : ProcessID) return Boolean is
      Root : Unsigned_64;
   begin
      if Caller = NO_PROCESS or else Caller /= PerCPUData.getCurrentPID or else
        proctab(Caller).pgTable = NO_PROCESS
      then
         return False;
      end if;
      Root := Unsigned_64 (rootPhysical (proctab(Caller).pgTable));
      if Address_Space_Tags.Root_Of (Unsigned_64 (x86.getCR3)) /= Root then
         if not Space_Mismatch_Reported then
            Space_Mismatch_Reported := True;
            TextIO.println ("KERN-014: user access refused: caller's tables not loaded");
         end if;
         return False;
      end if;
      return True;
   end Caller_Ready;

   -- Outside the received-grant aperture and the debug-view aperture
   -- (Debug_ABI, DBG-001): another process's frames mapped here are never
   -- the caller's own memory, so the kernel never reaches them, even
   -- through the caller's mapping.
   function Admitted (Address : Unsigned_64; Bytes : Storage_Count) return Boolean is
     (User_Access_Ranges.Admitted
        (Address, Unsigned_64 (Bytes),
         Unsigned_64 (GRANT_REGION_BASE), Unsigned_64 (GRANT_REGION_END))
      and then User_Access_Ranges.Admitted
        (Address, Unsigned_64 (Bytes),
         Debug_ABI.Aperture_First, Debug_ABI.Aperture_Limit));

   procedure Copy
     (Caller : ProcessID; Source : Unsigned_64; Destination : System.Address;
      Length : Storage_Count; Success : out Boolean) is
   begin
      Success := False;
      if not Caller_Ready (Caller) then
         return;
      elsif Length = 0 then
         Success := True;
         return;
      elsif not Admitted (Source, Length) then
         return;
      end if;
      Success :=
        Copy_From_User (Destination, To_Address (Integer_Address (Source)), Length) = 0;
   end Copy;

   procedure Copy_To_User
     (Caller : ProcessID; Destination : Unsigned_64; Source : System.Address;
      Length : Storage_Count; Success : out Boolean) is
   begin
      Success := False;
      if not Caller_Ready (Caller) then
         return;
      elsif Length = 0 then
         Success := True;
         return;
      elsif not Admitted (Destination, Length) then
         return;
      end if;
      Success :=
        Copy_To_User_Raw (To_Address (Integer_Address (Destination)), Source, Length) = 0;
   end Copy_To_User;

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

   -- A check before taking a message that the result could be delivered
   -- now: every page is mapped user-writable (and RAM) in the caller's
   -- tables. The page tables are kernel-owned; nothing of the process's
   -- memory is touched. Copy_To_User can still fail later, and callers
   -- handle that.
   function Writable_Range
     (Caller : ProcessID; Destination : Unsigned_64; Length : Storage_Count)
     return Boolean
   is
      function Writable is new User_Page_Walk.Writable_Frame (Read_Table_Entry);
      Page : Unsigned_64;
      Last : Unsigned_64;
   begin
      if not Caller_Ready (Caller) or else not Admitted (Destination, Length) then
         return False;
      end if;
      if Length = 0 then
         return True;
      end if;
      Page := Destination - Destination mod User_Page_Walk.Page_Size;
      Last := Destination + Unsigned_64 (Length) - 1;
      loop
         if Writable (Unsigned_64 (rootPhysical (proctab(Caller).pgTable)),
                      Page, Unsigned_64 (Virtmem.MAX_PHYS_USABLE)) = 0
         then
            return False;
         end if;
         exit when Last - Page < User_Page_Walk.Page_Size;
         Page := Page + User_Page_Walk.Page_Size;
      end loop;
      return True;
   end Writable_Range;

   -- An aligned word of the caller's own memory, outside the grant aperture.
   function Word_Admitted (Caller : ProcessID; Address : Unsigned_64) return Boolean is
     (Address mod 4 = 0 and then Caller_Ready (Caller) and then Admitted (Address, 4));

   procedure Load_Word32
     (Caller : ProcessID; Address : Unsigned_64; Value : out Unsigned_32;
      Success : out Boolean)
   is
      Word : aliased Unsigned_32 := 0;
   begin
      Value := 0;
      Success := False;
      if not Word_Admitted (Caller, Address) then
         return;
      end if;
      Success := User_Load32 (To_Address (Integer_Address (Address)), Word'Address) = 0;
      Value := Word;
   end Load_Word32;

   procedure Load_Shared_Word32
     (Caller : ProcessID; Owner : ProcessID; Address : Unsigned_64;
      Value : out Unsigned_32; Success : out Boolean)
   is
      function Readable is new User_Page_Walk.Readable_Frame (Read_Table_Entry);
      Frame : Unsigned_64;
      Pinned, Released : Boolean;
      Word : aliased Unsigned_32 := 0;
   begin
      Value := 0;
      Success := False;
      -- The word may lie in the grant aperture (the caller is the grantee).
      if Owner = NO_PROCESS or else Address mod 4 /= 0 or else
        not Caller_Ready (Caller) or else
        not User_Access_Ranges.Within_User (Address, 4) or else
        not User_Access_Ranges.Admitted
          (Address, 4, Debug_ABI.Aperture_First, Debug_ABI.Aperture_Limit)
      then
         return;
      end if;
      Frame := Readable
        (Unsigned_64 (rootPhysical (proctab(Caller).pgTable)),
         Address, Unsigned_64 (Virtmem.MAX_PHYS_USABLE));
      if Frame = 0 then
         return;
      end if;
      -- The grant owner's frame, never MMIO or a third party's page; the
      -- word itself is read through the caller's mapping.
      BuddyAllocator.pinOwnedFrame
        (Virtmem.PhysAddress (Frame), BuddyAllocator.Frame_Owner (Owner), Pinned);
      if not Pinned then
         return;
      end if;
      Success := User_Load32 (To_Address (Integer_Address (Address)), Word'Address) = 0;
      Value := Word;
      BuddyAllocator.unpinFrame (Virtmem.PhysAddress (Frame), Released);
      if not Released then
         raise ProcessException with "User-word frame pin lost";
      end if;
   end Load_Shared_Word32;

   procedure Store_Word32
     (Caller : ProcessID; Address : Unsigned_64; Value : Unsigned_32;
      Success : out Boolean) is
   begin
      Success := Word_Admitted (Caller, Address) and then
        User_Store32 (To_Address (Integer_Address (Address)), Value) = 0;
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
