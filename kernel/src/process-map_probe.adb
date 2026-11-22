pragma Ada_2022;
with TextIO; use TextIO;
with User_Page_Walk;
with PerCPUData;
with Process.User_Memory;

package body Process.Map_Probe is
   function Read_Word (Address : System.Address; Value : System.Address)
     return Unsigned_32
     with Import, Convention => C, External_Name => "cubitKernelReadProbe";

   function Hex (Value : Unsigned_64) return String is
      Digit_Text : constant String := "0123456789abcdef";
      Result : String (1 .. Hex_Digits);
      V : Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Digit_Text (Natural (V and 15) + 1);
         V := Shift_Right (V, 4);
      end loop;
      return Result;
   end Hex;

   -- Text is Prefix and Hex_Digits lowercase hex digits: Value.
   procedure Parse
     (Text, Prefix : String; Value : out Unsigned_64; Valid : out Boolean)
   is
      First : constant Natural := Text'First + Prefix'Length;
      Nibble : Unsigned_64;
   begin
      Value := 0;
      Valid := False;
      if Text'Length /= Prefix'Length + Hex_Digits
        or else Text (Text'First .. First - 1) /= Prefix
      then
         return;
      end if;
      for C of Text (First .. Text'Last) loop
         case C is
            when '0' .. '9' => Nibble := Character'Pos (C) - Character'Pos ('0');
            when 'a' .. 'f' => Nibble := Character'Pos (C) - Character'Pos ('a') + 10;
            when others => return;
         end case;
         Value := Shift_Left (Value, 4) or Nibble;
      end loop;
      Valid := True;
   end Parse;

   procedure Handle_Request
     (Caller : ProcessID; Text : String; Handled : out Boolean)
   is
      Value : Unsigned_64;
   begin
      Parse (Text, Request_Prefix, Value, Handled);
      if Handled then
         Report_Process_Page (Caller, Value);
         return;
      end if;
      Parse (Text, Write_Prefix, Value, Handled);
      if Handled then
         declare
            Word : aliased constant Unsigned_64 := Written_Word;
            Stored : Boolean;
         begin
            User_Memory.Copy_To_User (Caller, Value, Word'Address, 8, Stored);
            println ("KERN014-PROBE: user write " & (if Stored then "stored" else "refused"));
         end;
         return;
      end if;
      if Text = Faults_Request then
         Handled := True;
         println ("KERN014-PROBE: user-access faults recovered" &
                  Unsigned_64'Image (User_Memory.Recovered_Faults));
      end if;
   end Handle_Request;

   procedure Report_Process_Page (Caller : ProcessID; Address : Unsigned_64) is
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
      function Readable is new User_Page_Walk.Readable_Frame (Read_Entry);
      Frame : Unsigned_64;
      Word : aliased Unsigned_64 := 0;
   begin
      if Caller = NO_PROCESS or else Caller /= PerCPUData.getCurrentPID
        or else proctab (Caller).pgTable = NO_PROCESS or else Address mod 8 /= 0
      then
         println ("KERN014-PROBE: request refused");
         return;
      end if;
      Frame := Readable (Unsigned_64 (rootPhysical (proctab (Caller).pgTable)),
                         Address, Unsigned_64 (Virtmem.MAX_PHYS_USABLE));
      if Frame = 0 then
         println ("KERN014-PROBE: address not mapped in the process");
         return;
      end if;
      -- The read a kernel bug or a speculative gadget would make: the
      -- process's frame at its linear-map address.
      if Read_Word (Virtmem.P2Va (Virtmem.PhysAddress (Frame)) +
                      Storage_Offset (Address mod User_Page_Walk.Page_Size),
                    Word'Address) = 0
      then
         println ("KERN014-PROBE: process page through the kernel map: readable 0x" &
                  Hex (Word));
      else
         println ("KERN014-PROBE: process page through the kernel map: unmapped");
      end if;
   end Report_Process_Page;

   procedure Check_Free_Links is
      Tries : constant := 64;
      Pattern : constant Unsigned_64 := 16#5A5A_5A5A_5A5A_5A5A#;
      Frames : array (1 .. Tries) of Virtmem.PhysAddress := [others => 0];
      Chosen : Natural := 0;
      First, Second : aliased Unsigned_64 := 0;
      Ignore : Unsigned_32;
   begin
      -- Two buddies held at once: freeing one cannot coalesce, so the freed
      -- frame becomes an order-0 free block of its own.
      for I in Frames'Range loop
         BuddyAllocator.allocFrame (Frames (I));
         exit when Frames (I) = 0;
         for J in Frames'First .. I - 1 loop
            if (Frames (J) xor Frames (I)) = Virtmem.PhysAddress (Virtmem.FRAME_SIZE) then
               Chosen := J;
            end if;
         end loop;
         exit when Chosen /= 0;
      end loop;
      if Chosen = 0 then
         println ("KERN014-PROBE: free-list check: no buddy pair");
      else
         declare
            Page : array (0 .. Virtmem.FRAME_SIZE / 8 - 1) of Unsigned_64
              with Import, Volatile, Address => Virtmem.P2Va (Frames (Chosen));
         begin
            Page := [others => Pattern];
         end;
         BuddyAllocator.freeFrame (Frames (Chosen));
         Ignore := Read_Word (Virtmem.P2Va (Frames (Chosen)), First'Address);
         Ignore := Read_Word (Virtmem.P2Va (Frames (Chosen)) + 8, Second'Address);
         -- The allocator's links are kernel addresses (linear map or the
         -- list heads in the kernel image); the frame's own content is the
         -- pattern.
         println ("KERN014-PROBE: freed frame holds allocator links: " &
                  (if First >= Unsigned_64 (Virtmem.LINEAR_BASE)
                     and then Second >= Unsigned_64 (Virtmem.LINEAR_BASE)
                   then "yes" else "no"));
         Frames (Chosen) := 0;
      end if;
      for F of Frames loop
         if F /= 0 then
            BuddyAllocator.freeFrame (F);
         end if;
      end loop;
   end Check_Free_Links;
end Process.Map_Probe;
