------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with Partition_Tables; use Partition_Tables;
with CuBit.Shared_Copies;

package body Fs_Partitions is
   package Copies is new CuBit.Shared_Copies (Positive, Unsigned_8, Bytes);

   use type Partition_Tables.MBR_Kind, Partition_Tables.Header_Result, Partition_Tables.GUID;

   Describe_Words : constant := 4;
   Transfer_Words : constant := 4;
   GPT_Header_LBA : constant := 1;

   procedure Find_Root
     (Endpoint : CuBit.Handles.Endpoint; Grant : CuBit.Memory_Grants.Grant_Reference;
      Buffer : System.Address; Buffer_Bytes : Unsigned_32;
      Window : out Ext2.Volume_Window; Result : out Window_Result)
   is
      Description : Device_Description;
      Msg : Message;
      Ignore : MessageTag;

      --  Count blocks at First into Buffer. False: refused or short.
      function Read (First, Count : Unsigned_64) return Boolean is
      begin
         Msg := (tag => (label => OP_READ_BLOCKS, length => Transfer_Words, protocol => 0,
                         ipc => <>, reserved => 0),
                 authorityTag => 0, words => [First, Grant.slot, Count, Grant.generation]);
         Ignore := capCall (Endpoint, Msg, Wait_Forever);
         return Msg.tag.label = REPLY_OK
           and then Msg.words (0) = Count * Unsigned_64 (Description.logicalBlockSize);
      end Read;
   begin
      Window := Ext2.Whole_Device;
      Result := Device_Error;
      Msg := (tag => (label => OP_DESCRIBE_DEVICE, length => 0, protocol => 0, ipc => <>,
                      reserved => 0),
              authorityTag => 0, words => [others => 0]);
      Ignore := capCall (Endpoint, Msg, Wait_Forever);
      if Msg.tag.label /= REPLY_OK or else Msg.tag.length /= Describe_Words
        or else not Decode_Description (Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3),
                                        Description)
        or else Buffer_Bytes < Unsigned_32 (Description.logicalBlockSize)
      then
         return;
      end if;
      declare
         Block : constant Natural := Natural (Description.logicalBlockSize);
         Header : GPT_Header;
         Outcome : Header_Result;
      begin
         if not Read (0, 1) then
            return;
         end if;
         --  Every block is decoded from a private copy: the buffer stays
         --  lent to the block driver (SEC-025).
         declare
            Zero_Block : Sector;
         begin
            Copies.Copy_At (Buffer, Zero_Block);
            if Classify_MBR (Zero_Block) /= Protective_GPT then
               Result := Whole_Device;
               return;
            end if;
         end;
         if not Read (GPT_Header_LBA, 1) then
            return;
         end if;
         declare
            Header_Block : Bytes (1 .. Block);
         begin
            Copies.Copy_At (Buffer, Header_Block);
            Decode_Header (Header_Block, GPT_Header_LBA, Description.blockCount, Header, Outcome);
         end;
         if Outcome /= Header_Valid then
            Result := No_Root_Partition;
            return;
         end if;
         declare
            Array_Bytes : constant Unsigned_64 :=
              Unsigned_64 (Header.Entry_Count) * Unsigned_64 (Header.Entry_Bytes);
            Array_Blocks : constant Unsigned_64 :=
              (Array_Bytes + Unsigned_64 (Block) - 1) / Unsigned_64 (Block);
         begin
            if Array_Blocks * Unsigned_64 (Block) > Unsigned_64 (Buffer_Bytes)
              or else Array_Blocks > Unsigned_64 (Description.maxTransferBlocks)
              or else not Read (Header.Entries_LBA, Array_Blocks)
            then
               Result := (if Array_Blocks * Unsigned_64 (Block) > Unsigned_64 (Buffer_Bytes)
                          then No_Root_Partition else Device_Error);
               return;
            end if;
            declare
               --  One copy for the checksum and the decode: what is checked
               --  is what is decoded (at most the transfer buffer, on the
               --  16 MiB stack).
               Entries : Bytes (1 .. Natural (Array_Bytes));
            begin
               Copies.Copy_At (Buffer, Entries);
               if CRC32 (Entries) /= Header.Entries_CRC then
                  Result := No_Root_Partition;
                  return;
               end if;
               for I in 0 .. Natural (Header.Entry_Count) - 1 loop
                  declare
                     Item : constant Partition_Entry :=
                       Decode_Entry (Entries, I * Natural (Header.Entry_Bytes));
                  begin
                     if Item.Kind = Linux_Filesystem and then Entry_In_Range (Header, Item) then
                        Window := (First => Item.First, Blocks => Item.Last - Item.First + 1);
                        Result := Partition;
                        return;
                     end if;
                  end;
               end loop;
            end;
         end;
         Result := No_Root_Partition;
      end;
   end Find_Root;

end Fs_Partitions;
