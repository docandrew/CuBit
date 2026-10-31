with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Block_Requests; use CuBit.Block_Requests;
with CuBit.Block_Work; use CuBit.Block_Work;
with CuBit.Block_Sync_Driver;
with CuBit.Device_Identities;

package body Ram_Device is
   use type System.Address;
   Storage : System.Address := System.Null_Address;
   Blocks : Unsigned_64 := 0;

   procedure Transfer
     (Item : Transfer_Request; Buffer : System.Address; Moved : out Unsigned_64)
   is
      Device_Bytes : String (1 .. Natural (Bytes (Item)))
        with Import,
             Address => Storage + Storage_Offset (Item.First_Block * Block_Size);
      Grant_Bytes : String (1 .. Natural (Bytes (Item)))
        with Import, Address => Buffer;
   begin
      if Item.Direction = Device_Read then
         Grant_Bytes := Device_Bytes;
      else
         Device_Bytes := Grant_Bytes;
      end if;
      Moved := Bytes (Item);
   end Transfer;

   --  Never called: the device offers no FEATURE_FLUSH.
   procedure Flush (Durable : out Boolean) is
   begin
      Durable := False;
   end Flush;

   package Driver is new CuBit.Block_Sync_Driver
     (Writes => Any_Client_Writes, Transfer => Transfer, Flush => Flush);

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
      Driver.Start
        (State =>
           (Presence      => Available,
            Blocks        => Blocks,
            Logical_Size  => Block_Size,
            Physical_Size => Block_Size,
            Max_Transfer  => Maximum_Transfer_Blocks,
            --  No flush and no FUA: RAM is not durable storage.
            Features      => FEATURE_VOLATILE,
            Media         => Memory_Media),
         Identity =>
           (Model  => CuBit.Device_Identities.To_Text ("CuBit RAM disk"),
            Serial => CuBit.Device_Identities.To_Text ("")));
      Success := True;
   end Initialize;

   procedure Serve_One renames Driver.Serve_One;
end Ram_Device;
