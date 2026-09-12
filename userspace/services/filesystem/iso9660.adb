with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Memory_Grants;
with CuBit.String;

package body ISO9660 is
   use type System.Address;
   Optical_Endpoint : constant CapabilitySlot := 12;
   Buffer : System.Address := System.Null_Address;
   Grant : CuBit.Memory_Grants.Grant_Reference;
   Granted, Mounted : Boolean := False;
   Failed : Boolean := False;
   function Media_Failed return Boolean is (Failed);
   Volume_Blocks : Unsigned_64 := 0;
   Root, Apps : ISO_Records.File_Record;

   function Fetch (First, Blocks : Unsigned_64) return Boolean is
      Request : Message :=
        (tag => (label => OP_READ_BLOCKS, length => 4, flags => 0, reserved => 0),
         authorityTag => 0,
         words => [First, Unsigned_64 (Grant.slot), Blocks,
                   Unsigned_64 (Grant.generation)]);
      Tag : MessageTag;
   begin
      if Failed or else not Granted or else Blocks not in 1 .. 16 or else
        First >= Volume_Blocks or else Blocks > Volume_Blocks - First
      then return False; end if;
      Tag := capCall (Optical_Endpoint, Request);
      if Tag.label /= REPLY_OK or else Tag.length /= 1 or else
        Request.words (0) /= Blocks * 2048
      then
         Failed := True;
         return False;
      end if;
      return True;
   end Fetch;

   procedure Child
     (Parent : ISO_Records.File_Record; Name : String;
      Item : out ISO_Records.File_Record; Found : out Boolean)
   is
      Position : Unsigned_64 := 0;
      Entry_Offset, Consumed : Natural;
      Valid : Boolean;
      Candidate : ISO_Records.File_Record;
   begin
      Item := (others => <>); Found := False;
      --  Bound lookup work even on hostile media. No symlink, relocation,
      --  multi-extent, interleave or UNIX mode interpretation in this profile.
      --  A caller can append a component to a regular filename; that is a
      --  failed lookup, not evidence of damaged media.
      if not Parent.Directory then return; end if;
      if Parent.Bytes = 0 or else
        Parent.Bytes > 1024 * 1024 or else Parent.Bytes mod 2048 /= 0
      then Failed := True; return; end if;
      while Position < Unsigned_64 (Parent.Bytes) loop
         if not Fetch (Unsigned_64 (Parent.Extent) + Position / 2048, 1) then
            return;
         end if;
         declare
            Data : ISO_Records.Sector with Import, Address => Buffer;
         begin
            Entry_Offset := 0;
            while Entry_Offset < 2048 loop
               exit when Data (Entry_Offset) = 0;
               ISO_Records.Decode_Record
                 (Data, Entry_Offset, Volume_Blocks, Candidate, Consumed, Valid);
               if not Valid then Failed := True; return; end if;
               if ISO_Records.Matches (Candidate, Name) then
                  Item := Candidate; Found := True; return;
               end if;
               Entry_Offset := Entry_Offset + Consumed;
            end loop;
         end;
         Position := Position + 2048;
      end loop;
   end Child;

   function Mount return Boolean is
      Request : Message :=
        (tag => (label => OP_DESCRIBE_DEVICE, length => 0, flags => 0, reserved => 0),
         authorityTag => 0, words => [others => 0]);
      Tag : MessageTag;
      Description : Device_Description;
      Raw : Unsigned_64;
      Valid : Boolean;
   begin
      if Failed then return False; end if;
      if Mounted then return True; end if;
      Tag := capCall (Optical_Endpoint, Request);
      if Tag.label /= REPLY_OK or else Tag.length /= 4 or else
        not Decode_Description (Request.words (0), Request.words (1),
                                Request.words (2), Request.words (3), Description)
        or else Description.logicalBlockSize /= 2048 or else
        Description.media /= Optical_Media or else not Is_Read_Only (Description)
        or else Description.maxTransferBlocks < 16
      then return False; end if;
      Volume_Blocks := Description.blockCount;
      if Buffer = System.Null_Address then
         Raw := syscall (SYSCALL_SBRK, 9 * 4096);
         if Raw = Unsigned_64'Last then return False; end if;
         Buffer := To_Address (Integer_Address ((Raw + 4095) and not 4095));
      end if;
      if not Granted then
         CuBit.Memory_Grants.Create_Via_Capability
           (Optical_Endpoint, Buffer, 8, True, Grant, Granted);
         if not Granted then return False; end if;
      end if;
      --  Bounded descriptor set; boot records may precede the PVD.
      for Block in Unsigned_64 range 16 .. 47 loop
         if not Fetch (Block, 1) then return False; end if;
         declare
            Data : ISO_Records.Sector with Import, Address => Buffer;
            Blocks : Unsigned_64;
         begin
            if not ISO_Records.Header_Valid (Data) or else Data (0) = 255 then
               Failed := True; return False;
            end if;
            if Data (0) = 1 then
               ISO_Records.Decode_Volume
                 (Data, Description.blockCount, Blocks, Root, Valid);
               if not Valid then Failed := True; return False; end if;
               Volume_Blocks := Blocks;
               Child (Root, "apps", Apps, Valid);
               if not Valid or else not Apps.Directory then
                  Failed := True; return False;
               end if;
               Mounted := True;
               debugPrint ("FS: native USB ISO9660 apps mounted" & ASCII.LF);
               return True;
            end if;
         end;
      end loop;
      Failed := True;
      return False;
   end Mount;

   procedure Find (Path : String; Item : out ISO_Records.File_Record;
                   Found : out Boolean)
   is
      Parent, Next_Item : ISO_Records.File_Record;
      First, Last : Natural;
      Depth : Natural := 0;
   begin
      Item := (others => <>); Found := False;
      if Path'Length not in 1 .. 256 or else not Mount then return; end if;
      Parent := Apps;
      First := Path'First;
      if Path (First) = '/' then First := First + 1; end if;
      while First <= Path'Last loop
         Depth := Depth + 1;
         if Depth > 16 then return; end if;
         Last := First;
         while Last < Path'Last and then Path (Last + 1) /= '/' loop
            Last := Last + 1;
         end loop;
         if Path (First .. Last) = "." or else Path (First .. Last) = ".." or else
           Path (First) = '/'
         then return; end if;
         Child (Parent, Path (First .. Last), Next_Item, Found);
         if not Found then return; end if;
         Parent := Next_Item;
         exit when Last = Path'Last;
         Found := False;
         First := Last + 2;
      end loop;
      if Found then Item := Parent; end if;
   end Find;

   procedure Read
     (Item : ISO_Records.File_Record; Offset, Count : Unsigned_64;
      Destination : System.Address; Bytes : out Unsigned_64;
      Success : out Boolean)
   is
      Position : Unsigned_64 := Offset;
      Remaining, Skip, Blocks, Amount : Unsigned_64;
      Copied : System.Address;
   begin
      Bytes := 0; Success := False;
      if Failed or else not Mounted or else Item.Directory then return; end if;
      if Offset >= Unsigned_64 (Item.Bytes) then Success := True; return; end if;
      Remaining := Unsigned_64'Min (Count, Unsigned_64 (Item.Bytes) - Offset);
      while Remaining > 0 loop
         Skip := Position mod 2048;
         Amount := Unsigned_64'Min (Remaining, 32768 - Skip);
         Blocks := (Skip + Amount + 2047) / 2048;
         if not Fetch (Unsigned_64 (Item.Extent) + Position / 2048, Blocks) then
            return;
         end if;
         Copied := CuBit.String.memcpy
           (Destination + Storage_Offset (Bytes), Buffer + Storage_Offset (Skip),
            Storage_Count (Amount));
         Bytes := Bytes + Amount;
         Position := Position + Amount;
         Remaining := Remaining - Amount;
      end loop;
      Success := True;
   end Read;
end ISO9660;
