with System.Machine_Code;
with Fs_Server.Handles; use Fs_Server.Handles;
with Fs_Server.Replies; use Fs_Server.Replies;
with Fs_Server.Watches; use Fs_Server.Watches;

package body Fs_Server.Delegations is

   use type Ext2.Write_Status;
   use type Codes.Dirty_Tag;
   use ASCII;

   --  Write-back failures of released handles (errseq-like): reported by
   --  the owner's next flush or close of the same file, never dropped. A
   --  full table falls back to the owner's next flush or close of any file.
   type Write_Error is record
      used  : Boolean := False;
      owner : Process_ID := No_Process;
      key   : Inode_Identity := (Volume_Index'First, 0);
   end record;
   MAX_WRITE_ERRORS : constant := 64;
   writeErrors : array (0 .. MAX_WRITE_ERRORS - 1) of Write_Error;
   MAX_ERROR_OWNERS : constant := 16;
   overflowOwners : array (0 .. MAX_ERROR_OWNERS - 1) of Process_ID :=
     [others => No_Process];


   procedure keepWriteError (slot : Natural) is
      kept : Boolean := False;
   begin
      if not writebackFailed (slot) then
         return;
      end if;
      writebackFailed (slot) := False;
      for e of writeErrors loop
         if not kept and then not e.used then
            e := (used => True, owner => files (slot).ownerPID,
                  key => (files (slot).volume, files (slot).inodeNum));
            kept := True;
         end if;
      end loop;
      for o of overflowOwners loop
         if not kept and then (o = No_Process or else o = files (slot).ownerPID) then
            o := files (slot).ownerPID;
            kept := True;
         end if;
      end loop;
      if not kept then
         --  Nowhere left to keep it: say so rather than lose it silently.
         debugPrint ("FS: write-back failure not kept for its owner" & LF);
      end if;
   end keepWriteError;

   --  Take (and clear) a kept failure of owner for key.
   procedure takeWriteError (owner : Process_ID; key : Inode_Identity; found : out Boolean) is
   begin
      found := False;
      for e of writeErrors loop
         if e.used and then e.owner = owner and then e.key = key then
            e := (others => <>);
            found := True;
         end if;
      end loop;
      for o of overflowOwners loop
         if o = owner then
            o := No_Process;
            found := True;
         end if;
      end loop;
   end takeWriteError;

   --  An exited owner's kept failures have no one left to report to.
   procedure forgetWriteErrors (owner : Process_ID) is
   begin
      for e of writeErrors loop
         if e.used and then e.owner = owner then
            e := (others => <>);
         end if;
      end loop;
      for o of overflowOwners loop
         if o = owner then
            o := No_Process;
         end if;
      end loop;
   end forgetWriteErrors;


   procedure delegate (slot : Handle_Slot; grant : Boolean; write : Boolean := False) is
      q : constant Client_Queue_Count := queueOf (files (slot).ownerPID);
   begin
      delegated (slot) := grant and then q /= No_Client_Queue;
      writeDelegated (slot) := delegated (slot) and then write and then
        clientQueues (q).dirty /= System.Null_Address;
      if q = No_Client_Queue then
         return;
      end if;
      declare
         entryAt : constant Natural := FQ.Delegations_At + slot * FQ.Delegation_Bytes;
         valid : Unsigned_32 with Volatile, Import,
           Address => serverWord (q, entryAt + FQ.Delegation_Valid_At);
         mode : Unsigned_32 with Volatile, Import,
           Address => serverWord (q, entryAt + FQ.Delegation_Mode_At);
         inode : Unsigned_64 with Volatile, Import,
           Address => serverWord (q, entryAt + FQ.Delegation_Inode_At);
         version : Unsigned_64 with Volatile, Import,
           Address => serverWord (q, entryAt + FQ.Delegation_Version_At);
         size : Unsigned_64 with Volatile, Import,
           Address => serverWord (q, entryAt + FQ.Delegation_Size_At);
         key : constant Inode_Identity := (files (slot).volume, files (slot).inodeNum);
      begin
         valid := 0;
         System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
         if grant then
            --  Writes buffered under a write delegation reach the file
            --  without fileChanging: the version moves on now, so pages
            --  another client cached for the file are not taken again.
            if writeDelegated (slot) then
               bumpVersion (key);
            end if;
            inode := objectOf (key);
            version := versionOf (key);
            size := Ext2.fileSize
              (Open_Inodes.Value (inodeObjects, Open_Inodes.Owner_Index (slot)));
            mode := (if writeDelegated (slot) then FQ.Write_Delegation
                     else FQ.Read_Delegation);
            System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
            valid := 1;
         end if;
      end;
   end delegate;

   ---------------------------------------------------------------------------
   --  Harvesting a write delegation's dirty pages (FQ.Dirty_*): copy each
   --  stable entry of the handle out of the client's dirty arena, write
   --  contiguous runs to the file, and free the entries. The table is the
   --  client's: every field is checked, and an entry is used only if its
   --  sequence word is even and unchanged across the copy.
   ---------------------------------------------------------------------------
   --  Yields one harvest may spend waiting out entries the client is
   --  writing (odd sequence words), in total: an entry still odd after the
   --  budget is left for a later harvest. A client cannot hold the service.
   HARVEST_WAIT_BUDGET  : constant := 64;
   pragma Compile_Time_Error
     (Dirty_Runs.Maximum_Items /= FQ.Dirty_Entries or else
      Dirty_Runs.Page_Bytes /= FQ.Dirty_Page_Bytes,
      "Dirty_Runs must match the dirty arena");
   subtype Dirty_Index is Dirty_Runs.Item_Index;
   harvestItems : Dirty_Runs.Item_Array;

   --  Atomically replace expected at addr with desired; True if it was so.
   function compareAndSwap
     (addr : System.Address; expected, desired : Unsigned_32) return Boolean
   is
      previous : Unsigned_32;
   begin
      System.Machine_Code.Asm
        ("lock cmpxchgl %2, (%1)",
         Outputs  => Unsigned_32'Asm_Output ("=a", previous),
         Inputs   => [System.Address'Asm_Input ("r", addr),
                      Unsigned_32'Asm_Input ("r", desired),
                      Unsigned_32'Asm_Input ("a", expected)],
         Clobber  => "memory",
         Volatile => True);
      return previous = expected;
   end compareAndSwap;

   --  Entries found in one scan of the arena, listed per handle slot
   --  (links are entry index + 1; 0 ends a list).
   type Entry_Links is array (Dirty_Index) of Natural;
   type Slot_Links is array (Handle_Slot) of Natural;
   type Slot_List is array (0 .. MAX_OPEN_FILES - 1) of Handle_Slot;
   nextOfEntry : Entry_Links := [others => 0];
   firstOfSlot : Slot_Links := [others => 0];
   scannedSlots : Slot_List := [others => 0];


   --  slot's dirty-entry tag (FQ.Dirty_Slot_At) for its current handle.
   function tagOf (slot : Handle_Slot) return Codes.Dirty_Tag is
     (Codes.Tag_Of (Codes.Handle_Slot (slot), files (slot).generation));

   --  The live handle of owner that tag names, or -1: an entry with any
   --  other tag is stale (its handle released) or forged.
   function taggedHandle (word : Unsigned_32; owner : Process_ID) return Integer is
      tag : constant Codes.Dirty_Tag := Codes.Dirty_Tag (word);
      slot : constant Handle_Slot := Handle_Slot (Codes.Tag_Slot (tag));
   begin
      if files (slot).active and then files (slot).ownerPID = owner and then
        tagOf (slot) = tag
      then
         return slot;
      end if;
      return -1;
   end taggedHandle;

   --  Harvest the entries of queue q's dirty arena that belong to slot
   --  single, or (single < 0) to any of the queue owner's write-delegated
   --  handles: one scan of the entries either way.
   procedure harvestEntries (q : Client_Queue_Index; single : Integer) is
      owner : constant Process_ID := clientQueues (q).owner;
      base : constant System.Address := clientQueues (q).dirty;
      slotCount : Natural := 0;
      waitBudget : Natural := HARVEST_WAIT_BUDGET;

      function entryWord (i : Dirty_Index; field : Natural) return System.Address is
        (base + Storage_Offset (i * FQ.Dirty_Entry_Bytes + field));

      --  A live handle's entry this harvest takes.
      function wanted (handle : Integer) return Boolean is
        (handle >= 0 and then
         (if single >= 0 then handle = single
          else writeDelegated (handle) and then
               files (handle).filesystemKind = EXT2_FILESYSTEM));

      --  Write harvestBuffer (0 .. length - 1) at offset through slot; then
      --  free the items first .. last whose words did not move (none if
      --  last < first).
      procedure writeRun (slot : Handle_Slot; offset : Unsigned_64; length : Natural;
                          first, last : Integer) is
         currentInode : Ext2.Inode := Open_Inodes.Value
           (inodeObjects, Open_Inodes.Owner_Index (slot));
         written : Unsigned_64;
         status : Ext2.Write_Status;
      begin
         if length = 0 or else first < 0 or else last < first then
            return;
         end if;
         Ext2.writeData
           (Contexts (files (slot).volume).Fs, files (slot).inodeNum,
            currentInode, offset, harvestBuffer'Address, Unsigned_64 (length),
            written, status);
         Open_Inodes.Replace
           (inodeObjects, Open_Inodes.Owner_Index (slot), currentInode);
         noteModified (slot);
         if status /= Ext2.Write_Complete or else written /= Unsigned_64 (length) then
            writebackFailed (slot) := True;
         end if;
         for k in first .. last loop
            if compareAndSwap (entryWord (harvestItems (k).Entry_Index, FQ.Dirty_Sequence_At),
                               harvestItems (k).Sequence, 0)
            then
               null;   --  freed; a word that moved holds newer data, taken later
            end if;
         end loop;
      end writeRun;

      --  2. slot's items in file order, then 3. contiguous runs within the
      --  buffer, each copied, checked against the client's words, and written.
      procedure writeSlot (slot : Handle_Slot) is
         count : Natural := 0;
         link  : Natural := firstOfSlot (slot);
         first : Natural := 0;
         last  : Dirty_Index;
         bytes : Natural;
      begin
         while link /= 0 loop
            declare
               i : constant Dirty_Index := link - 1;
               startWord : Unsigned_16 with Volatile, Import,
                 Address => entryWord (i, FQ.Dirty_Start_At);
               stopWord : Unsigned_16 with Volatile, Import,
                 Address => entryWord (i, FQ.Dirty_Stop_At);
               seqWord : Unsigned_32 with Volatile, Import,
                 Address => entryWord (i, FQ.Dirty_Sequence_At);
               pageWord : Unsigned_32 with Volatile, Import,
                 Address => entryWord (i, FQ.Dirty_Page_At);
               sequence : constant Unsigned_32 := seqWord;
               page : constant Unsigned_32 := pageWord;
               start : constant Unsigned_16 := startWord;
               stop  : constant Unsigned_16 := stopWord;
            begin
               if sequence /= 0 and then sequence mod 2 = 0 and then
                 start < stop and then Natural (stop) <= FQ.Dirty_Page_Bytes
               then
                  harvestItems (count) :=
                    (Entry_Index => i, Sequence => sequence, Page => page,
                     Start => Natural (start), Stop => Natural (stop));
                  count := count + 1;
               end if;
               link := nextOfEntry (i);
            end;
         end loop;
         firstOfSlot (slot) := 0;
         Dirty_Runs.Sort (harvestItems, count);
         while first < count loop
            Dirty_Runs.Next_Run (harvestItems, count, first, last, bytes);
            declare
               length : Natural := 0;
               kept   : Integer := first - 1;   --  items copied intact
            begin
               for k in first .. last loop
                  declare
                     item : constant Dirty_Runs.Item := harvestItems (k);
                     n : constant Natural := Dirty_Runs.Length (item);
                     seqWord : Unsigned_32 with Volatile, Import,
                       Address => entryWord (item.Entry_Index, FQ.Dirty_Sequence_At);
                     source : Byte_Buffer with Import,
                       Address => base + Storage_Offset
                         (FQ.Dirty_Pages_At + item.Entry_Index * FQ.Dirty_Page_Bytes);
                  begin
                     harvestBuffer (length .. length + n - 1) :=
                       source (item.Start .. item.Stop - 1);
                     System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
                     --  Rewritten while copied: stop here; it is taken later.
                     exit when seqWord /= item.Sequence;
                     length := length + n;
                     kept := k;
                  end;
               end loop;
               writeRun (slot, Dirty_Runs.Offset (harvestItems (first)), length, first, kept);
            end;
            first := last + 1;
         end loop;
      end writeSlot;
   begin
      if base = System.Null_Address then
         return;
      end if;
      --  1. The wanted entries, listed per slot. Every field is the
      --  client's: copied, then checked against the service's own table.
      --  An entry being written (odd) is waited for within one bounded
      --  budget per harvest, else left for a later one. A stable entry
      --  whose tag names no live handle of this client is freed unwritten.
      for i in reverse Dirty_Index loop
         declare
            seqWord : Unsigned_32 with Volatile, Import,
              Address => entryWord (i, FQ.Dirty_Sequence_At);
            slotWord : Unsigned_32 with Volatile, Import,
              Address => entryWord (i, FQ.Dirty_Slot_At);
            sequence : Unsigned_32 := seqWord;
            ignore : Unsigned_64;
         begin
            if sequence /= 0 then
               while sequence mod 2 = 1 and then waitBudget > 0 and then
                 wanted (taggedHandle (slotWord, owner))
               loop
                  ignore := syscall (SYSCALL_YIELD);
                  waitBudget := waitBudget - 1;
                  sequence := seqWord;
               end loop;
               System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
               declare
                  tag : constant Unsigned_32 := slotWord;
                  handle : constant Integer := taggedHandle (tag, owner);
               begin
                  if sequence = 0 or else sequence mod 2 = 1 then
                     null;   --  free, or still being written: a later harvest
                  elsif handle < 0 then
                     if compareAndSwap (seqWord'Address, sequence, 0) then
                        null;   --  stale: its handle is gone
                     end if;
                  elsif wanted (handle) then
                     if firstOfSlot (handle) = 0 then
                        scannedSlots (slotCount) := handle;
                        slotCount := slotCount + 1;
                     end if;
                     nextOfEntry (i) := firstOfSlot (handle);
                     firstOfSlot (handle) := i + 1;
                  end if;
               end;
            end if;
         end;
      end loop;
      for k in 0 .. slotCount - 1 loop
         writeSlot (scannedSlots (k));
      end loop;
   end harvestEntries;

   procedure harvest (slot : Handle_Slot) is
      q : constant Client_Queue_Count := queueOf (files (slot).ownerPID);
   begin
      if q /= No_Client_Queue and then clientQueues (q).dirty /= System.Null_Address and then
        files (slot).filesystemKind = EXT2_FILESYSTEM
      then
         harvestEntries (q, slot);
      end if;
   end harvest;

   --  Every write-delegated handle of queue q's owner, in one scan.
   procedure harvestOwner (q : Client_Queue_Index) is
   begin
      harvestEntries (q, -1);
   end harvestOwner;

   --  Take back slot's write delegation: clear its valid word, then harvest.
   procedure recallWrite (slot : Handle_Slot) is
   begin
      if writeDelegated (slot) then
         delegate (slot, False);
         System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
         harvest (slot);
      end if;
   end recallWrite;

   --  The file key is about to change through slot except: every other
   --  handle's delegation goes first, and the version moves on.
   procedure fileChanging (key : Inode_Identity; except : Integer) is
      object : constant Open_Inodes.Link := Open_Inodes.Object_Of (inodeObjects, key);
   begin
      --  The file's handles are its object's holders (no scan of the table).
      if object /= 0 then
         for i in 0 .. Open_Inodes.Holding (inodeObjects, object) - 1 loop
            declare
               j : constant Handle_Slot := Open_Inodes.Holder (inodeObjects, object, i);
            begin
               if j /= except and then delegated (j) then
                  if writeDelegated (j) then
                     recallWrite (j);
                  else
                     delegate (j, False);
                  end if;
               end if;
            end;
         end loop;
      end if;
      bumpVersion (key);
      noteModified (except);
   end fileChanging;

   procedure dropDelegation (slot : Natural) is
   begin
      if slot <= Handle_Slot'Last and then delegated (slot) then
         if writeDelegated (slot) then
            recallWrite (slot);
         else
            delegate (slot, False);
         end if;
      end if;
   end dropDelegation;

   --  After slot's own change: its delegation, if held, carries the new
   --  version and size.
   procedure refreshDelegation (slot : Handle_Slot) is
   begin
      if delegated (slot) then
         --  Kept in its mode: a write delegation stays one (its client
         --  goes on buffering, and its entries stay the service's to take).
         delegate (slot, True, write => writeDelegated (slot));
      end if;
   end refreshDelegation;

   --  A handle was opened: a writable open revokes the others; the handle
   --  is delegated if no other handle can write the file.
   procedure handleOpened (slot : Handle_Slot) is
      object : constant Open_Inodes.Link :=
        Open_Inodes.Object_Of_Owner (inodeObjects, Open_Inodes.Owner_Index (slot));
      holders : constant Natural :=
        (if object = 0 then 0 else Open_Inodes.Holding (inodeObjects, object));
      otherWriter : Boolean := False;
      sole : Boolean := True;
   begin
      --  The file's other handles are its object's holders.
      for i in 0 .. holders - 1 loop
         declare
            j : constant Handle_Slot := Open_Inodes.Holder (inodeObjects, object, i);
         begin
            if j /= slot then
               sole := False;
               --  Its buffered writes reach the file before this open answers.
               if writeDelegated (j) then
                  recallWrite (j);
               elsif (files (slot).openRights and ACL_WRITE) /= 0 and then delegated (j) then
                  delegate (j, False);
               end if;
               if (files (j).openRights and ACL_WRITE) /= 0 then
                  otherWriter := True;
               end if;
            end if;
         end;
      end loop;
      if not otherWriter then
         delegate (slot, True,
                   write => sole and then (files (slot).openRights and ACL_WRITE) /= 0);
      end if;
   end handleOpened;

end Fs_Server.Delegations;
