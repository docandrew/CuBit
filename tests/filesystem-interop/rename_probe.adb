with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with CuBit.Messages; use CuBit.Messages;
with Volume_Admission; use Volume_Admission;
with Directory_Blocks;
with CuBit.Block_Devices;

--  Diagnostic against a DISPOSABLE copy only: creates one file, checks the
--  actual directory-block preparation, then invokes the production rename.
procedure Rename_Probe is
   Fs : Filesystem;
   Admission : Admission_Result;
   Root : Inode;
   Created, Found, Parent : Unsigned_32;
   R : Read_Status;
   W : Write_Status;
   L : Directory_Lookup_Status;
   Result : Rename_Status;
   Before : constant String := "cubit-new.dat";
   After : constant String := "cubit-renamed-longer.dat";
   Original, Candidate : Directory_Blocks.Block_Data;
   P : Directory_Blocks.Prepare_Result;
   use type Directory_Blocks.Prepare_Result;
   use type Directory_Blocks.Block_Data;
   Can_Rename : Boolean := False;
   M : Message;
begin
   Open_Image (Ada.Command_Line.Argument (1));
   initBlockDevice (Fs, 1, (slot => 1, generation => 1),
                    Grant_Buffer'Address, Grant_Buffer'Length, Admission);
   pragma Assert (Admission = Admitted);
   createFile (Fs, ROOT_INODE, Before, Created, W);
   pragma Assert (W = Write_Complete);
   readInode (Fs, ROOT_INODE, Root, R);
   pragma Assert (R = Read_Complete);
   Put_Line ("Root bytes:" & Root.sizeLo'Image & " flags:" & Root.flags'Image);
   for I in 0 .. Natural (Root.sizeLo / Fs.blkSize) - 1 loop
      M := ((CuBit.Block_Devices.OP_READ_BLOCKS, 4, 0, 0), 0,
            [Unsigned_64 (Root.directBlocks (I)) * Unsigned_64 (Fs.blkSize / 512),
             1, Unsigned_64 (Fs.blkSize / 512), 1]);
      M.tag := capCall (1, M);
      pragma Assert (M.tag.label = CuBit.Block_Devices.REPLY_OK);
      Original := [others => 0];
      for J in 1 .. Natural (Fs.blkSize) loop Original (J) := Grant_Buffer (J - 1); end loop;
      Candidate := Original;
      Directory_Blocks.Prepare_Rename
        (Candidate, Directory_Blocks.Block_Length (Fs.blkSize),
         Fs.sb.inodeCount, Before, After, P);
      Put_Line ("Block" & I'Image & ": " & P'Image);
      pragma Assert (P in Directory_Blocks.Prepared | Directory_Blocks.Insufficient_Space |
                          Directory_Blocks.Source_Not_Found);
      if P /= Directory_Blocks.Prepared then pragma Assert (Candidate = Original); end if;
      Can_Rename := Can_Rename or P = Directory_Blocks.Prepared;
   end loop;
   renamePath (Fs, Before, After, Result);
   Put_Line ("Production rename: " & Result'Image);
   pragma Assert (Result = (if Can_Rename then Rename_Complete else Rename_Range_Unsupported));
   resolvePath (Fs, (if Can_Rename then After else Before), Found, L);
   pragma Assert (L = Lookup_Found and Found = Created);
   resolvePath (Fs, (if Can_Rename then Before else After), Found, L);
   pragma Assert (L = Lookup_Not_Found);
   -- The native test must not assume shared root fixtures leave spare bytes.
   -- Same-length root rename and longer nested rename exercise both paths
   -- without depending on which other apps were installed in the base image.
   renamePath (Fs, (if Can_Rename then After else Before), "cubit-alt.dat", Result);
   pragma Assert (Result = Rename_Complete);
   resolvePath (Fs, "cubit-alt.dat", Found, L);
   pragma Assert (L = Lookup_Found and Found = Created);
   resolvePath (Fs, "lost+found", Parent, L);
   pragma Assert (L = Lookup_Found);
   createFile (Fs, Parent, "rename-before.dat", Created, W);
   pragma Assert (W = Write_Complete);
   renamePath (Fs, "lost+found/rename-before.dat",
               "lost+found/rename-after-much-longer.dat", Result);
   pragma Assert (Result = Rename_Complete);
   resolvePath (Fs, "lost+found/rename-after-much-longer.dat", Found, L);
   pragma Assert (L = Lookup_Found and Found = Created);
   resolvePath (Fs, "lost+found/rename-before.dat", Found, L);
   pragma Assert (L = Lookup_Not_Found);
   Close_Image;
   Put_Line ("Rename probe: preserved inode/name on the supported or rejected path PASS");
   Put_Line ("Same-length root and longer nested rename: PASS");
end Rename_Probe;
