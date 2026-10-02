with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with CuBit.Messages; use CuBit.Messages;
with Volume_Admission; use Volume_Admission;

--  Journal operations under cache pressure (pressure.py): one large write
--  whose dirty blocks fill cache sets, so operations meet full sets and
--  must neither split (splitCommits stays 0) nor lose ordering. "SYNCED k"
--  is printed after step k's flush completed.
procedure Pressure is
   Fs : Filesystem;
   Admission : Admission_Result;
   Flushed : Flush_Status;
   Written : Write_Status;
   Lookup : Directory_Lookup_Status;
   Read_Result : Read_Status;
   Number : Unsigned_32;
   Item : Inode;
   Completed : Unsigned_64;
   Mib : constant := 1024 * 1024;
   Big : constant := 150 * Mib;
   type Payload is array (1 .. Big) of Character;
   type Payload_Access is access Payload;
   Data : constant Payload_Access := new Payload'(others => 'P');
begin
   Open_Image (Ada.Command_Line.Argument (1));
   initBlockDevice (Fs, 1, (slot => 1, generation => 1), Grant_Buffer'Address,
                    Grant_Buffer'Length, Admission);
   pragma Assert (Admission = Admitted);
   createFile (Fs, ROOT_INODE, "big", Number, Written);
   pragma Assert (Written = Write_Complete);
   Flush (Fs, Flushed);
   pragma Assert (Flushed = Flush_Complete);
   Put_Line ("SYNCED 1");
   readInode (Fs, Number, Item, Read_Result);
   pragma Assert (Read_Result = Read_Complete);
   writeData (Fs, Number, Item, 0, Data.all'Address, Big, Completed, Written);
   pragma Assert (Written = Write_Complete and Completed = Big);
   Flush (Fs, Flushed);
   pragma Assert (Flushed = Flush_Complete);
   Put_Line ("SYNCED 2");
   resolvePath (Fs, "big", Number, Lookup);
   pragma Assert (Lookup = Lookup_Found);
   Detach (Fs, Flushed);
   pragma Assert (Flushed = Flush_Complete);
   Put_Line ("SPLIT COMMITS" & splitCommits'Image & " PRESSURE BLOCKS" & pressureBlocks'Image);
   Close_Image;
end Pressure;
