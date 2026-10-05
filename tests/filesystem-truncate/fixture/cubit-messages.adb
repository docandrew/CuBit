with Ada.Environment_Variables;
with System.Storage_Elements;
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
      function Covers (Position : Natural) return Boolean is
        (offset <= Position and then Position < offset + count);
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
         if durableInode.tripleIndirectBlock /= 0 then
            if durableInode.tripleIndirectBlock = Block then return True; end if;
            declare
               Root : array (0 .. Reclamation_Block_Bytes / 4 - 1) of Unsigned_32
                 with Import, Address => Durable
                   (Natural (durableInode.tripleIndirectBlock) * Reclamation_Block_Bytes)'Address;
            begin
               for Middle of Root loop
                  if Middle /= 0 then
                     if Middle = Block then return True; end if;
                     declare
                        Leaves : array (Root'Range) of Unsigned_32
                          with Import, Address => Durable
                            (Natural (Middle) * Reclamation_Block_Bytes)'Address;
                     begin
                        for Leaf of Leaves loop
                           if Leaf_References (Leaf) then return True; end if;
                        end loop;
                     end;
                  end if;
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
         --  Write-back coalesces adjacent blocks: match any write whose
         --  range covers a block of interest, not only one starting there.
         if (Check_Resize_Reclamation or Check_Reclamation) and then
           Covers (3 * Reclamation_Block_Bytes)
         then
            Reclaims := Reclaims + 1;
            --  At every attempted bitmap clear, inspect the last completed
            --  flush, not merely the driver's in-memory inode candidate.
            for Bit in 0 .. Disk'Length / Reclamation_Block_Bytes - Reclamation_First_Block - 1 loop
               declare
                  Mask : constant Unsigned_8 := Shift_Left (Unsigned_8 (1), Bit mod 8);
                  Block : constant Unsigned_32 := Unsigned_32 (Bit + Reclamation_First_Block);
                  Bitmap : constant Natural := 3 * Reclamation_Block_Bytes;
               begin
                  if (Disk (Bitmap + Bit / 8) and Mask) /= 0 and then
                    (Grant_Buffer (Bitmap - offset + Bit / 8) and Mask) = 0
                  then
                     pragma Assert (Barriers > 0);
                     pragma Assert (not Referenced (Block));
                  end if;
               end;
            end loop;
         end if;
         if Check_Creation and then Covers (Creation_Block * 1024) and then
           --  A grown directory's empty block (one unused record spanning
           --  it) carries no name and may precede the inode.
           not (for all I in 0 .. 3 =>
                  Grant_Buffer (Creation_Block * 1024 - offset + I) = 0)
         then
            --  The name may only be published after inode initialization.
            pragma Assert ((Disk (4096) and 4) /= 0);
            pragma Assert
              (Ext2.inodeType (createdInode) = Ext2.INODE_REGULAR_FILE);
            pragma Assert (createdInode.numHardLinks = 1);
            pragma Assert (Ext2.fileSize (createdInode) = 0);
         end if;
         if Covers (5 * 1024) then
            Publication_Attempted := True;
            Inode_Write_Call := Calls;
         end if;
         if Covers (21 * 1024) then
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
            2 => 8, 3 => Pack_Properties (FEATURE_FLUSH or FEATURE_VOLATILE_CACHE, Fixed_Media)];
      elsif fail then
         case Reply_Style is
            when Error_Label => null;
            --  A flush's valid reply count is zero: make it malformed.
            when Short_Transfer =>
               msg.words (0) := (if op = OP_FLUSH_DEVICE then 1 else 0);
            when Bad_Length => msg.tag.length := 0;
            when Bad_Flags => msg.tag.flags := 1;
            when Bad_Reserved => msg.tag.reserved := 1;
         end case;
      end if;
      return msg.tag;
   end capCall;
   function syscall
     (call : Unsigned_64; arg0 : Unsigned_64 := 0; arg1 : Unsigned_64 := 0;
      arg2 : Unsigned_64 := 0; arg3 : Unsigned_64 := 0;
      arg4 : Unsigned_64 := 0; arg5 : Unsigned_64 := 0) return Unsigned_64
   is
      pragma Unreferenced (arg1, arg2, arg3, arg4, arg5);
      Page : constant := 4096;
      type Region is array (Natural range <>) of Unsigned_8;
      type Region_Access is access Region;
   begin
      if call = SYSCALL_GETTIME then
         return 0;
      elsif call = SYSCALL_INFO and then arg0 = SYSINFO_WALL_CLOCK_OFFSET then
         declare
            Clock : constant String :=
              Ada.Environment_Variables.Value ("CUBIT_TEST_WALL_CLOCK", "");
         begin
            return (if Clock = "" then Unsigned_64'Last
                    else Unsigned_64'Value (Clock) * 1_000);
         end;
      end if;
      if call /= SYSCALL_ALLOCATE_OWNED_MEMORY or else arg0 = 0 or else
        arg0 > 16 * 1024 * 1024
      then
         return Unsigned_64'Last;
      end if;
      declare
         Area : constant Region_Access :=
           new Region'(0 .. Natural (arg0) + Page - 1 => 0);
         Base : constant Unsigned_64 := Unsigned_64
           (System.Storage_Elements.To_Integer (Area.all (0)'Address));
      begin
         return (Base + Page - 1) and not (Page - 1);
      end;
   end syscall;
end CuBit.Messages;
