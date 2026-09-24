with CuBit.Block_Devices; use CuBit.Block_Devices;
with Ext2;
package body CuBit.Messages is
   use type Ext2.DirectBlockArray;
   procedure Reset is
   begin
      Disk := [others => 0];
      Durable := [others => 0];
      Grant_Buffer := [others => 0];
      Fail_At := 0;
      Reply_Style := Error_Label;
      Calls := 0;
      Writes := 0;
      Barriers := 0;
      Reclaims := 0;
      Publication_Attempted := False;
      Failed := False;
      Check_Reclamation := True;
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
      offset : constant Natural := Natural (msg.words (0) * 512);
      count : Natural := Natural (msg.words (2) * 512);
      fail : Boolean;
      durableInode : Ext2.Inode
        with Import, Address => Durable (5 * 1024)'Address;
      createdInode : Ext2.Inode
        with Import, Address => Disk (5 * 1024 + 256)'Address;
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
         elsif offset = 3 * 1024 and then Check_Reclamation then
            Reclaims := Reclaims + 1;
            --  Test the actual on-device ordering, not just final state.
            pragma Assert (Barriers > 0);
            pragma Assert (Ext2.fileSize (durableInode) = 0);
            pragma Assert
              (durableInode.directBlocks = Ext2.DirectBlockArray'[others => 0] and then
               durableInode.singleIndirectBlock = 0);
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
           [0 => Disk'Length / 512, 1 => Pack_Sizes (512, 512),
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
