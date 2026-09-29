with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with CuBit.Messages; use CuBit.Messages;
with Volume_Admission; use Volume_Admission;
with Ext2_Support;

--  Linux-hosted: the actual CuBit Ext2 driver, with only its block IPC replaced
--  by an adapter operating on a Linux-created image. Not a simulated filesystem.
procedure Main is
   Fs : Filesystem;
   Admission : Admission_Result;
   Root, Item : Inode;
   Number, Created : Unsigned_32;
   Read_Result : Read_Status;
   Write_Result : Write_Status;
   Lookup : Directory_Lookup_Status;
   Rename : Rename_Status;
   Truncate : Truncate_Status;
   Flushed : Flush_Status;
   Completed : Unsigned_64;
   Payload : constant String := "CuBit roundtrip";
   Output : String (1 .. 15);

   procedure Write_At (Offset : Unsigned_64) is
   begin
      writeData (Fs, Number, Item, Offset, Payload'Address, Payload'Length,
                 Completed, Write_Result);
      pragma Assert (Write_Result = Write_Complete and Completed = Payload'Length);
      readData (Fs, Item, Offset, Output'Address, Output'Length, Completed, Read_Result);
      pragma Assert (Read_Result = Read_Complete and Output = Payload);
   end Write_At;
begin
   Open_Image (Ada.Command_Line.Argument (1));
   initBlockDevice (Fs, 1, (slot => 1, generation => 1),
                    Grant_Buffer'Address, Grant_Buffer'Length, Admission);
   pragma Assert (Admission = Admitted);
   readInode (Fs, ROOT_INODE, Root, Read_Result);
   pragma Assert (Read_Result = Read_Complete);
   createFile (Fs, ROOT_INODE, "created", Created, Write_Result);
   pragma Assert (Write_Result = Write_Complete);
   pragma Assert (Created = Unsigned_32'Value (Ada.Command_Line.Argument (2)));
   Number := Created;
   readInode (Fs, Number, Item, Read_Result);
   pragma Assert (Read_Result = Read_Complete);
   Write_At (0);
   Write_At (Unsigned_64 (14 * Fs.blkSize + 7)); -- sparse, with an indirect root
   Write_At (Unsigned_64 (2 * Fs.blkSize + 3)); -- fill a hole below EOF
   Write_At (Unsigned_64 ((12 + Fs.blkSize / 4) * Fs.blkSize) + 7);
   Write_At (Unsigned_64 ((12 + 2 * (Fs.blkSize / 4)) * Fs.blkSize) + 7);
   resizeFile (Fs, Number, Unsigned_64 ((12 + Fs.blkSize / 4) * Fs.blkSize) + 17,
               Item, Truncate);
   pragma Assert (Truncate = Truncate_Complete);
   --  Reallocate the removed leaf, zeroing the retained tail on the way.
   Write_At (Unsigned_64 ((12 + 2 * (Fs.blkSize / 4)) * Fs.blkSize) + 7);
   renamePath (Fs, "created", "renamed", Rename);
   pragma Assert (Rename = Rename_Complete);

   resolvePath (Fs, "existing", Number, Lookup);
   pragma Assert (Lookup = Lookup_Found);
   readInode (Fs, Number, Item, Read_Result);
   pragma Assert (Read_Result = Read_Complete);
   Write_At (0); -- must preserve this Linux inode's extended metadata/xattrs

   resolvePath (Fs, "truncate-me", Number, Lookup);
   pragma Assert (Lookup = Lookup_Found);
   truncateToEmpty (Fs, Number, Item, Truncate);
   pragma Assert (Truncate = Truncate_Complete);
   Write_At (0);
   --  Shrink while retaining a partial indirect block, then grow sparsely.
   resolvePath (Fs, "resize-me", Number, Lookup);
   pragma Assert (Lookup = Lookup_Found);
   resizeFile (Fs, Number, Unsigned_64 (13 * Fs.blkSize + 17), Item, Truncate);
   pragma Assert (Truncate = Truncate_Complete);
   resizeFile (Fs, Number, Unsigned_64 (15 * Fs.blkSize + 11), Item, Truncate);
   pragma Assert (Truncate = Truncate_Complete);

   --  Repeat across the indirect/direct boundary and then grow via a write
   --  into the retained partial block: bytes in the gap must never reappear.
   resolvePath (Fs, "resize-gap", Number, Lookup);
   pragma Assert (Lookup = Lookup_Found);
   resizeFile (Fs, Number, 17, Item, Truncate);
   pragma Assert (Truncate = Truncate_Complete);
   Write_At (513);

   --  Linux-created double-indirect mappings: in-leaf and cross-leaf writes
   --  may change payload only, never inode size, mappings or accounting.
   resolvePath (Fs, "double-existing", Number, Lookup);
   pragma Assert (Lookup = Lookup_Found);
   readInode (Fs, Number, Item, Read_Result);
   pragma Assert (Read_Result = Read_Complete and Item.doubleIndirectBlock /= 0);
   Write_At (Unsigned_64 ((12 + Fs.blkSize / 4) * Fs.blkSize) + 7);
   Write_At (Unsigned_64 ((12 + 2 * (Fs.blkSize / 4)) * Fs.blkSize) - 7);
   resolvePath (Fs, "double-resize", Number, Lookup);
   pragma Assert (Lookup = Lookup_Found);
   resizeFile (Fs, Number, Unsigned_64 ((12 + Fs.blkSize / 4) * Fs.blkSize) + 17,
               Item, Truncate);
   pragma Assert (Truncate = Truncate_Complete);
   resizeFile (Fs, Number, Unsigned_64 ((12 + 2 * (Fs.blkSize / 4)) * Fs.blkSize) + 64,
               Item, Truncate);
   pragma Assert (Truncate = Truncate_Complete);
   --  Triple-indirect mappings. Without LARGE_FILE, a 4 KiB volume cannot
   --  represent the first triple block, and growth there must be rejected.
   declare
      P : constant Unsigned_64 := Unsigned_64 (Fs.blkSize / 4);
      Bytes : constant Unsigned_64 := Unsigned_64 (Fs.blkSize);
      First_Triple : constant Unsigned_64 := (12 + P + P * P) * Bytes;
      Middle_Bytes : constant Unsigned_64 := P * P * Bytes;
   begin
      if not Ext2_Support.Size_Admitted
        (First_Triple + 2 * Middle_Bytes, Fs.sb.readOnlyFeatures)
      then
         resolvePath (Fs, "renamed", Number, Lookup);
         pragma Assert (Lookup = Lookup_Found);
         readInode (Fs, Number, Item, Read_Result);
         pragma Assert (Read_Result = Read_Complete);
         writeData (Fs, Number, Item, First_Triple, Payload'Address,
                    Payload'Length, Completed, Write_Result);
         pragma Assert (Write_Result = Write_File_Range_Unsupported and Completed = 0);
      else
         --  Linux-created triple tree: in-place overwrites, one crossing the
         --  double/triple boundary, must not change the inode.
         resolvePath (Fs, "triple-existing", Number, Lookup);
         pragma Assert (Lookup = Lookup_Found);
         readInode (Fs, Number, Item, Read_Result);
         pragma Assert (Read_Result = Read_Complete and Item.tripleIndirectBlock /= 0);
         Write_At (First_Triple - 7);
         Write_At (First_Triple + Middle_Bytes + 20);

         --  CuBit growth: new root/middle/leaf, a second leaf, a second
         --  middle; shrink retiring both, sparse regrowth zeroing the retained
         --  partial block's tail, then reallocation of a middle subtree.
         resolvePath (Fs, "triple-grow", Number, Lookup);
         pragma Assert (Lookup = Lookup_Found);
         readInode (Fs, Number, Item, Read_Result);
         pragma Assert (Read_Result = Read_Complete and Item.tripleIndirectBlock = 0);
         Write_At (First_Triple + 7);
         Write_At (First_Triple + P * Bytes + 3);
         Write_At (First_Triple + Middle_Bytes + (P + 1) * Bytes + 5);
         resizeFile (Fs, Number, First_Triple + 17, Item, Truncate);
         pragma Assert (Truncate = Truncate_Complete);
         resizeFile (Fs, Number, First_Triple + Middle_Bytes + 100, Item, Truncate);
         pragma Assert (Truncate = Truncate_Complete);
         Write_At (First_Triple + Middle_Bytes + 3 * Bytes + 1);

         --  Linux-created tree across two middles: shrink into the first
         --  leaf, then sparse regrowth, then truncate a second file to empty.
         resolvePath (Fs, "triple-resize", Number, Lookup);
         pragma Assert (Lookup = Lookup_Found);
         resizeFile (Fs, Number, First_Triple + Bytes + 5, Item, Truncate);
         pragma Assert (Truncate = Truncate_Complete);
         resizeFile (Fs, Number, First_Triple + Middle_Bytes, Item, Truncate);
         pragma Assert (Truncate = Truncate_Complete);
         resolvePath (Fs, "triple-empty", Number, Lookup);
         pragma Assert (Lookup = Lookup_Found);
         truncateToEmpty (Fs, Number, Item, Truncate);
         pragma Assert (Truncate = Truncate_Complete and Item.tripleIndirectBlock = 0);
      end if;
   end;
   Flush (Fs, Flushed);
   pragma Assert (Flushed = Flush_Complete);
   --  End the session as an unmount does: an ext3 journal is left empty
   --  and needs_recovery cleared.
   Detach (Fs, Flushed);
   pragma Assert (Flushed = Flush_Complete);
   Close_Image;
   Ada.Text_IO.Put_Line ("EXT2-HOSTED-ROUNDTRIP: PASS");
end Main;
