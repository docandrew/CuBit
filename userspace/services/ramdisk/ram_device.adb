with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Memory_Grants; use CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;

package body Ram_Device is
   use type System.Address;
   Storage : System.Address := System.Null_Address;
   Blocks : Unsigned_64 := 0;

   procedure Initialize
     (Base : System.Address; Bytes : Unsigned_64; Success : out Boolean)
   is
   begin
      Success := False;
      --  Storage cannot be rebound while clients may retain a device session.
      if Blocks /= 0 or else Base = System.Null_Address or else
        Bytes = 0 or else Bytes mod Block_Size /= 0 or else
        Bytes > Unsigned_64 (Storage_Offset'Last) or else
        Bytes > Unsigned_64 (Integer_Address'Last - To_Integer (Base))
      then
         return;
      end if;
      Storage := Base;
      Blocks := Bytes / Block_Size;
      Success := True;
   end Initialize;

   procedure Handle
     (Sender : ProcessID; Request : Message; Response : out Message)
   is
      LBA : constant Unsigned_64 := Request.words (0);
      Count : constant Unsigned_64 := Request.words (2);
      Bytes : Unsigned_64;
      Reference : Grant_Reference;
      Buffer : System.Address;
      Acquired, Returned : Boolean;
   begin
      Response := NULL_MESSAGE;
      Response.tag := (label => REPLY_ERROR, length => 1,
                       flags => 0, reserved => 0);
      if Blocks = 0 or else Request.tag.flags /= 0 or else
        Request.tag.reserved /= 0
      then
         return;
      end if;
      if Request.tag.label = OP_DESCRIBE_DEVICE then
         if Request.tag.length /= 0 then
            return;
         end if;
         Response.tag := (label => REPLY_OK, length => 4,
                          flags => 0, reserved => 0);
         Response.words :=
           [0 => Blocks, 1 => Pack_Sizes (Block_Size, Block_Size),
            2 => Maximum_Transfer_Blocks,
            3 => Pack_Properties (FEATURE_VOLATILE, Memory_Media)];
         return;
      end if;

      --  FLUSH is deliberately unsupported. RAM is not durable storage.
      if Request.tag.label not in OP_READ_BLOCKS | OP_WRITE_BLOCKS or else
        Request.tag.length /= 4 or else
        Count = 0 or else Count > Maximum_Transfer_Blocks or else
        LBA >= Blocks or else Count > Blocks - LBA or else
        Request.words (1) > MAXIMUM_GLOBAL_SLOT or else
        Request.words (3) = 0 or else Request.words (3) > MAXIMUM_GENERATION
      then
         return;
      end if;
      Bytes := Count * Block_Size;
      Reference :=
        (slot => Global_Grant_Slot (Request.words (1)),
         generation => Grant_Generation (Request.words (3)));
      Acquire
        (reference => Reference, expectedOwner => Sender,
         byteOffset => 0, byteLength => Bytes,
         requiredAccess =>
           (if Request.tag.label = OP_READ_BLOCKS then Write_Access
            else Read_Access),
         mappedAddress => Buffer, success => Acquired);
      if not Acquired then
         return;
      end if;
      declare
         Device_Bytes : String (1 .. Natural (Bytes))
           with Import, Address => Storage + Storage_Offset (LBA * Block_Size);
         Grant_Bytes : String (1 .. Natural (Bytes))
           with Import, Address => Buffer;
      begin
         if Request.tag.label = OP_READ_BLOCKS then
            Grant_Bytes := Device_Bytes;
         else
            Device_Bytes := Grant_Bytes;
         end if;
      end;
      Return_Acquisition (Reference, Returned);
      if Returned then
         Response.tag.label := REPLY_OK;
         Response.words (0) := Bytes;
      end if;
   end Handle;
end Ram_Device;
