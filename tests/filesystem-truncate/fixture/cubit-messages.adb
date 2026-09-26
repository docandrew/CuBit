with CuBit.Block_Devices; use CuBit.Block_Devices;
with Ext2;
package body CuBit.Messages is
   use type Ext2.DirectBlockArray;
   procedure Reset is
   begin
      Disk := [others => 0];
      Durable := [others => 0];
      Grant_Buffer := [others => 0];
      Sector_Bytes := 512;
      Fail_At := 0;
      Reply_Style := Error_Label;
      Calls := 0;
      Writes := 0;
      Barriers := 0;
      Reclaims := 0;
      Publication_Attempted := False;
      Failed := False;
      Check_Reclamation := True;
      Check_Resize_Reclamation := False;
      Reclamation_Block_Bytes := 1024;
      Reclamation_First_Block := 1;
      Inode_Write_Call := 0;
      Pointer_Write_Call := 0;
      Check_Creation := False;
      Override_Description := False;
      Description_Reply := NULL_MESSAGE;
   end Reset;

   function capCall
     (slot : Unsigned_64; msg : in out Message) return MessageTag
   is
      pragma Unreferenced (slot);
      op : constant Unsigned_32 := msg.tag.label;
      offset : constant Natural := Natural (msg.words (0) * Unsigned_64 (Sector_Bytes));
      count : Natural := Natural (msg.words (2) * Unsigned_64 (Sector_Bytes));
      fail : Boolean;
      durableInode : Ext2.Inode
        with Import, Address => Durable (5 * Reclamation_Block_Bytes)'Address;
      createdInode : Ext2.Inode
        with Import, Address => Disk (5 * 1024 + 256)'Address;
      function Referenced (Block : Unsigned_32) return Boolean is
         function Leaf_References (Number : Unsigned_32) return Boolean is
         begin
            if Number = 0 then return False; end if;
            if Number = Block then return True; end if;
            declare
               Values : array (0 .. Reclamation_Block_Bytes / 4 - 1) of Unsigned_32
                 with Import, Address => Durable (Natural (Number) * Reclamation_Block_Bytes)'Address;
            begin
               return (for some Value of Values => Value = Block);
            end;
         end Leaf_References;
      begin
         if (for some Value of durableInode.directBlocks => Value = Block) or else
           Leaf_References (durableInode.singleIndirectBlock)
         then return True; end if;
         if durableInode.doubleIndirectBlock /= 0 then
            if durableInode.doubleIndirectBlock = Block then return True; end if;
            declare
               Root : array (0 .. Reclamation_Block_Bytes / 4 - 1) of Unsigned_32
                 with Import, Address => Durable
                   (Natural (durableInode.doubleIndirectBlock) * Reclamation_Block_Bytes)'Address;
            begin
               for Leaf of Root loop
                  if Leaf_References (Leaf) then return True; end if;
               end loop;
            end;
         end if;
         return False;
      end Referenced;
   begin
      --  Quarantine must stop subsequent transport calls in this operation.
      pragma Assert (not Failed);
      Calls := Calls + 1;
      if op = OP_DESCRIBE_DEVICE and then Override_Description then
         msg := Description_Reply;
         return msg.tag;
      end if;
      fail := Calls = Fail_At;
      if op = OP_WRITE_BLOCKS then
         Writes := Writes + 1;
         if (Check_Resize_Reclamation or Check_Reclamation) and then
           offset = 3 * Reclamation_Block_Bytes
         then
            Reclaims := Reclaims + 1;
            --  At every attempted bitmap clear, inspect the last completed
            --  flush, not merely the driver's in-memory inode candidate.
            for Bit in 0 .. Disk'Length / Reclamation_Block_Bytes - Reclamation_First_Block - 1 loop
               declare
                  Mask : constant Unsigned_8 := Shift_Left (Unsigned_8 (1), Bit mod 8);
                  Block : constant Unsigned_32 := Unsigned_32 (Bit + Reclamation_First_Block);
               begin
                  if (Disk (offset + Bit / 8) and Mask) /= 0 and then
                    (Grant_Buffer (Bit / 8) and Mask) = 0
                  then
                     pragma Assert (Barriers > 0);
                     pragma Assert (not Referenced (Block));
                  end if;
               end;
            end loop;
         end if;
         if Check_Creation and then offset = Creation_Block * 1024 then
            --  The name may only be published after inode initialization.
            pragma Assert ((Disk (4096) and 4) /= 0);
            pragma Assert
              (Ext2.inodeType (createdInode) = Ext2.INODE_REGULAR_FILE);
            pragma Assert (createdInode.numHardLinks = 1);
            pragma Assert (Ext2.fileSize (createdInode) = 0);
         end if;
         if offset = 5 * 1024 then
            Publication_Attempted := True;
            Inode_Write_Call := Calls;
         elsif offset = 21 * 1024 then
            Pointer_Write_Call := Calls;
         end if;
      end if;
      if not fail or else Mode /= Before_IO then
         case op is
            when OP_READ_BLOCKS =>
               if fail and then Mode = Partial_Transfer then
                  count := Natural'Min (count / 2, 64);
               end if;
               Grant_Buffer (0 .. count - 1) :=
                 Disk (offset .. offset + count - 1);
            when OP_WRITE_BLOCKS =>
               if fail and then Mode = Partial_Transfer then
                  count := Natural'Min (count / 2, 64);
               end if;
               Disk (offset .. offset + count - 1) :=
                 Grant_Buffer (0 .. count - 1);
            when OP_FLUSH_DEVICE =>
               Durable := Disk;
               Barriers := Barriers + 1;
            when OP_DESCRIBE_DEVICE => null;
            when others => raise Program_Error;
         end case;
      end if;
      Failed := fail;
      msg.tag := (if fail and Reply_Style = Error_Label then (REPLY_ERROR, 1, 0, 0)
                  else (REPLY_OK, 1, 0, 0));
      msg.words := [0 => (if op = OP_FLUSH_DEVICE then 0
                         else Unsigned_64 (count)), others => 0];
      if op = OP_DESCRIBE_DEVICE and not fail then
         msg.tag.length := 4;
         msg.words :=
           [0 => Unsigned_64 (Disk'Length / Sector_Bytes),
            1 => Pack_Sizes (Logical_Block_Size (Sector_Bytes),
                             Logical_Block_Size (Sector_Bytes)),
            2 => 8, 3 => Pack_Properties (FEATURE_FLUSH, Fixed_Media)];
      elsif fail then
         case Reply_Style is
            when Error_Label => null;
            when Short_Transfer => msg.words (0) := 0;
            when Bad_Length => msg.tag.length := 0;
            when Bad_Flags => msg.tag.flags := 1;
            when Bad_Reserved => msg.tag.reserved := 1;
         end case;
      end if;
      return msg.tag;
   end capCall;
end CuBit.Messages;
