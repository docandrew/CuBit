with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with Volume_Admission; use Volume_Admission;
with Jbd2_Format; use Jbd2_Format;
with Jbd2_Recovery;

--  Linux-hosted: production journal replay against a Linux-created image.
--    replay admit IMAGE
--       Ext2 volume admission, which replays a dirty ext3 journal and
--       clears needs_recovery. Prints the admission result.
--    replay raw IMAGE BLOCK_BYTES FS_BLOCKS MAP
--       Jbd2_Recovery alone, for journals of volumes Ext2 does not admit
--       (metadata_csum, 64bit). MAP lists the journal's physical blocks in
--       logical order. Marks the journal clean as admission does.
procedure Replay is
   Sector_Bytes : constant := 512;

   procedure Transfer
     (Op : Unsigned_32; Block_Number : Unsigned_64; Size : Positive; Ok : out Boolean)
   is
      Msg : Message := NULL_MESSAGE;
      Tag : MessageTag;
   begin
      Msg.tag := (label => Op, length => 4, flags => 0, reserved => 0);
      Msg.words := [0 => Block_Number * Unsigned_64 (Size / Sector_Bytes), 1 => 1,
                    2 => Unsigned_64 (Size / Sector_Bytes), 3 => 1];
      Tag := capCall (1, Msg);
      Ok := Tag.label = REPLY_OK;
   end Transfer;
begin
   Open_Image (Argument (2));
   if Argument (1) = "admit" then
      declare
         Fs : Ext2.Filesystem;
         Result : Admission_Result;
         Detached : Ext2.Flush_Status;
      begin
         Ext2.initBlockDevice
           (Fs, 1, (slot => 1, generation => 1), Grant_Buffer'Address,
            Grant_Buffer'Length, Result);
         Put_Line ("ADMISSION " & Result'Image);
         if Result = Admitted then
            --  Admission opened the journal (needs_recovery set while in
            --  use); detaching closes it cleanly again.
            Ext2.Detach (Fs, Detached);
            Put_Line ("DETACH " & Detached'Image);
         end if;
         Close_Image;
         Set_Exit_Status (if Result = Admitted then Success else Failure);
      end;
      return;
   end if;

   declare
      Size : constant Block_Bytes := Block_Bytes'Value (Argument (3));
      Fs_Blocks : constant Unsigned_32 := Unsigned_32'Value (Argument (4));
      Map_File : File_Type;
      Map : array (0 .. 65_535) of Unsigned_64 := [others => 0];
      Mapped : Natural := 0;
      Data : Block := [others => 0];
      Super : Journal_Superblock;
      Valid, Ok : Boolean;

      procedure Read_Log (Log_Block : Unsigned_32; Result : out Block; Done : out Boolean) is
      begin
         Result := [others => 0];
         Done := Natural (Log_Block) < Mapped;
         if Done then
            Transfer (OP_READ_BLOCKS, Map (Natural (Log_Block)), Size, Done);
            for I in 0 .. Size - 1 loop
               Result (I) := Grant_Buffer (I);
            end loop;
         end if;
      end Read_Log;

      procedure Write_Home (Home : Unsigned_64; Source : Block; Done : out Boolean) is
      begin
         for I in 0 .. Size - 1 loop
            Grant_Buffer (I) := Source (I);
         end loop;
         Transfer (OP_WRITE_BLOCKS, Home, Size, Done);
      end Write_Home;

      package Recovery is new Jbd2_Recovery (Read_Log, Write_Home);
      use type Recovery.Outcome;
   begin
      Open (Map_File, In_File, Argument (5));
      while not End_Of_File (Map_File) loop
         Map (Mapped) := Unsigned_64'Value (Get_Line (Map_File));
         Mapped := Mapped + 1;
      end loop;
      Close (Map_File);
      Read_Log (0, Data, Ok);
      Decode_Superblock
        (Data, Size, Unsigned_32 (Mapped), Recovery.Superblock_Checksum_Matches (Data),
         Super, Valid);
      if not Ok or else not Valid then
         Put_Line ("JOURNAL INVALID");
         Set_Exit_Status (Failure);
         return;
      elsif Super.Start = 0 then
         Put_Line ("JOURNAL CLEAN");
         return;
      end if;
      declare
         Outcome : Recovery.Outcome;
         Next : Unsigned_32;
         Transactions, Written : Natural;
         Start_Offset : constant := 16#1C#;
         Sequence_Offset : constant := 16#18#;

         procedure Put_Be32 (Offset : Natural; Value : Unsigned_32) is
         begin
            Data (Offset) := Unsigned_8 (Shift_Right (Value, 24));
            Data (Offset + 1) := Unsigned_8 (Shift_Right (Value, 16) and 16#FF#);
            Data (Offset + 2) := Unsigned_8 (Shift_Right (Value, 8) and 16#FF#);
            Data (Offset + 3) := Unsigned_8 (Value and 16#FF#);
         end Put_Be32;
      begin
         Recovery.Recover (Super, Size, Fs_Blocks, Outcome, Next, Transactions, Written);
         Put_Line ("RECOVERY " & Outcome'Image & Transactions'Image &
                   " transactions" & Written'Image & " blocks next" & Next'Image);
         if Outcome /= Recovery.Recovered then
            Set_Exit_Status (Failure);
            return;
         end if;
         Put_Be32 (Start_Offset, 0);
         Put_Be32 (Sequence_Offset, Next);
         if Checksummed (Super.Incompat) then
            Put_Be32 (Superblock_Checksum_Offset, 0);
            Put_Be32 (Superblock_Checksum_Offset,
                      Crc32c (16#FFFF_FFFF#, Data, 0, Superblock_Bytes));
         end if;
         Write_Home (Map (0), Data, Ok);
      end;
      Close_Image;
   end;
end Replay;
