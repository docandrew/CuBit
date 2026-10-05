with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with CuBit.Messages; use CuBit.Messages;
with Volume_Admission; use Volume_Admission;

--  Linux-hosted, on a DISPOSABLE image (run-directories.py): the production
--  Ext2 driver grows a directory past its direct and single-indirect blocks,
--  removes names across it, reuses the space, and changes a Linux htree
--  directory. Linux's e2fsck and debugfs then judge the result.
procedure Directory_Growth is
   Fs : Filesystem;
   Admission : Admission_Result;
   Count : constant Positive := Positive'Value (Ada.Command_Line.Argument (2));
   Big, Number : Unsigned_32;
   Write_Result : Write_Status;
   Remove_Result : Remove_Status;
   Rename_Result : Rename_Status;
   Lookup : Directory_Lookup_Status;
   Unlinked : Inode;
   Flushed : Flush_Status;
   Replaced_Number : Unsigned_32;
   Replaced : Inode;

   function Name (Index : Natural) return String is
      Digits_Text : String := "00000";
      Rest : Natural := Index;
   begin
      for I in reverse Digits_Text'Range loop
         Digits_Text (I) := Character'Val (Character'Pos ('0') + Rest mod 10);
         Rest := Rest / 10;
      end loop;
      return "entry-" & Digits_Text & "-" & (1 .. 40 => 'x');
   end Name;
begin
   Open_Image (Ada.Command_Line.Argument (1));
   initBlockDevice (Fs, 1, (slot => 1, generation => 1),
                    Grant_Buffer'Address, Grant_Buffer'Length, Admission);
   pragma Assert (Admission = Admitted);

   resolvePath (Fs, "big", Big, Lookup);
   pragma Assert (Lookup = Lookup_Found);
   for I in 0 .. Count - 1 loop
      createFile (Fs, Big, Name (I), Number, Write_Result);
      if Write_Result /= Write_Complete then
         Put_Line ("create " & Name (I) & ": " & Write_Result'Image);
         raise Program_Error;
      end if;
   end loop;
   --  Every third name goes, from blocks all over the directory.
   for I in 0 .. Count - 1 loop
      if I mod 3 = 0 then
         unlinkPath (Fs, "big/" & Name (I), False, Number, Unlinked, Remove_Result);
         if Remove_Result /= Remove_Complete then
            Put_Line ("unlink " & Name (I) & ": " & Remove_Result'Image);
            raise Program_Error;
         end if;
      end if;
   end loop;
   --  New names reuse the freed space.
   for I in Count .. Count + 9 loop
      createFile (Fs, Big, Name (I), Number, Write_Result);
      pragma Assert (Write_Result = Write_Complete);
   end loop;
   makeDirectoryPath (Fs, "big/subdirectory", Number, Lookup, Write_Result);
   pragma Assert (Lookup = Lookup_Found and Write_Result = Write_Complete);
   removeDirectoryPath (Fs, "big/subdirectory", Number, Remove_Result);
   pragma Assert (Remove_Result = Remove_Complete);
   renamePath (Fs, "big/" & Name (Count - 1), "big/" & Name (Count + 20), False, Replaced_Number, Replaced, Rename_Result);
   pragma Assert (Rename_Result = Rename_Complete);

   --  A Linux htree directory: each change clears its index first.
   resolvePath (Fs, "indexed", Number, Lookup);
   pragma Assert (Lookup = Lookup_Found);
   createFile (Fs, Number, "added-by-cubit", Big, Write_Result);
   pragma Assert (Write_Result = Write_Complete);
   unlinkPath (Fs, "indexed/remove-me", False, Number, Unlinked, Remove_Result);
   pragma Assert (Remove_Result = Remove_Complete);
   renamePath (Fs, "indexed/rename-me", "indexed/renamed-by-cubit", False, Replaced_Number, Replaced, Rename_Result);
   pragma Assert (Rename_Result = Rename_Complete);

   --  POSIX rename (run-directories.py seeds moves/: a/, b/ and files).
   --  Across directories, to a new name:
   renamePath (Fs, "moves/a/moved", "moves/b/arrived", False, Replaced_Number,
               Replaced, Rename_Result);
   pragma Assert (Rename_Result = Rename_Complete and Replaced_Number = 0);
   --  Replacing an existing file (its old inode is released):
   renamePath (Fs, "moves/a/new-version", "moves/b/target", False, Replaced_Number,
               Replaced, Rename_Result);
   pragma Assert (Rename_Result = Rename_Complete and Replaced_Number /= 0);
   --  Replacing within one directory, as index.lock -> index:
   renamePath (Fs, "moves/b/index.lock", "moves/b/index", False, Replaced_Number,
               Replaced, Rename_Result);
   pragma Assert (Rename_Result = Rename_Complete and Replaced_Number /= 0);
   --  A directory with a child moves; its ".." follows:
   renamePath (Fs, "moves/a/sub", "moves/b/sub", False, Replaced_Number,
               Replaced, Rename_Result);
   pragma Assert (Rename_Result = Rename_Complete);
   --  Onto an empty directory, which is released:
   renamePath (Fs, "moves/b/sub", "moves/b/empty", False, Replaced_Number,
               Replaced, Rename_Result);
   pragma Assert (Rename_Result = Rename_Complete);
   --  Refusals, nothing changed:
   renamePath (Fs, "moves/b", "moves/b/empty/inner", False, Replaced_Number,
               Replaced, Rename_Result);
   pragma Assert (Rename_Result = Rename_Invalid_Move);
   renamePath (Fs, "moves/b/arrived", "moves/b/full", False, Replaced_Number,
               Replaced, Rename_Result);
   pragma Assert (Rename_Result = Rename_Is_Directory);
   renamePath (Fs, "moves/b/full", "moves/b/arrived", False, Replaced_Number,
               Replaced, Rename_Result);
   pragma Assert (Rename_Result = Rename_Not_Directory);
   renamePath (Fs, "moves/b/empty", "moves/b/full", False, Replaced_Number,
               Replaced, Rename_Result);
   pragma Assert (Rename_Result = Rename_Not_Empty);
   --  Within one crowded directory to a longer name: no room in the
   --  source block, so the general path moves it (the old in-place limit).
   renamePath (Fs, "big/" & Name (Count + 20),
               "big/" & Name (Count + 20) & "-a-much-longer-name-than-before",
               False, Replaced_Number, Replaced, Rename_Result);
   pragma Assert (Rename_Result = Rename_Complete);

   Detach (Fs, Flushed);
   pragma Assert (Flushed = Flush_Complete);
   Put_Line ("DIRECTORY-GROWTH: driver done" & Count'Image & " names");
end Directory_Growth;
