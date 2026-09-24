with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with System;
with Ram_Device;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants; use CuBit.Memory_Grants;
with CuBit.Block_Devices; use CuBit.Block_Devices;

procedure Main is
   Disk : String (1 .. 8192) := [others => 'D'];
   Before : String (Disk'Range);
   Request, Response : Message := NULL_MESSAGE;
   Description : Device_Description;
   OK : Boolean;
   Failures : Natural := 0;

   procedure Transfer (Op : Unsigned_32; LBA, Count : Unsigned_64) is
   begin
      Request := (tag => (Op, 4, 0, 0), authorityTag => 0,
                  words => [LBA, 1, Count, 7]);
   end Transfer;

   procedure Reject (Owner : ProcessID := 42) is
      Prior_Buffer : constant String := Buffer;
   begin
      Before := Disk;
      Ram_Device.Handle (Owner, Request, Response);
      pragma Assert (Response.tag.label = REPLY_ERROR and Response.words (0) = 0);
      pragma Assert (Disk = Before and Buffer = Prior_Buffer and Active = 0);
      Failures := Failures + 1;
   end Reject;
begin
   Transfer (OP_READ_BLOCKS, 0, 1);
   Reject; -- no initialized medium
   Ram_Device.Initialize (System.Null_Address, Disk'Length, OK);
   pragma Assert (not OK);
   Ram_Device.Initialize (Disk'Address, 0, OK);
   pragma Assert (not OK);
   Ram_Device.Initialize (Disk'Address, 513, OK);
   pragma Assert (not OK);
   Ram_Device.Initialize (Disk'Address, Unsigned_64'Last - 511, OK);
   pragma Assert (not OK);
   Ram_Device.Initialize (Disk'Address, Disk'Length, OK);
   pragma Assert (OK);
   Ram_Device.Initialize (Buffer'Address, Buffer'Length, OK);
   pragma Assert (not OK); -- never rebind a live device session

   Request := NULL_MESSAGE;
   Request.tag.label := OP_DESCRIBE_DEVICE;
   Ram_Device.Handle (42, Request, Response);
   pragma Assert (Response.tag.label = REPLY_OK and Response.tag.length = 4);
   OK := Decode_Description
     (Response.words (0), Response.words (1), Response.words (2),
      Response.words (3), Description);
   pragma Assert (OK and Description.blockCount = 16 and
                  Description.media = Memory_Media and Is_Volatile (Description));
   pragma Assert ((Description.features and FEATURE_FLUSH) = 0 and Acquires = 0);
   OK := Decode_Description
     (Response.words (0), Response.words (1), Response.words (2),
      Pack_Properties (FEATURE_VOLATILE or FEATURE_FLUSH, Memory_Media),
      Description);
   pragma Assert (not OK);
   Request.tag.length := 1;
   Reject;
   Request := NULL_MESSAGE;
   Request.tag.label := OP_FLUSH_DEVICE;
   Reject;

   for Op of MessageWords'[Unsigned_64 (OP_READ_BLOCKS), Unsigned_64 (OP_WRITE_BLOCKS), 0, 0] loop
      if Op /= 0 then
         Transfer (Unsigned_32 (Op), 16, 1); Reject;
         Transfer (Unsigned_32 (Op), 15, 2); Reject;
         Transfer (Unsigned_32 (Op), Unsigned_64'Last, 1); Reject;
         Transfer (Unsigned_32 (Op), 0, 0); Reject;
         Transfer (Unsigned_32 (Op), 0, Unsigned_64'Last); Reject;
         Transfer (Unsigned_32 (Op), 0, 1025); Reject;
         Transfer (Unsigned_32 (Op), 0, 1);
         Request.words (1) := MAXIMUM_GLOBAL_SLOT + 1; Reject;
         Transfer (Unsigned_32 (Op), 0, 1);
         Request.words (3) := 0; Reject;
         Request.words (3) := MAXIMUM_GENERATION + 1; Reject;
         Request.words (3) := 6; Reject; -- stale but well-formed
         Request.words (3) := 7; Reject (Owner => 43);
         Request.words (1) := 2; Reject; -- foreign grant
         Transfer (Unsigned_32 (Op), 0, 1);
         Request.tag.length := 3; Reject;
         Request.tag.length := 4; Request.tag.flags := 1; Reject;
         Request.tag.flags := 0; Request.tag.reserved := 1; Reject;
      end if;
   end loop;
   Transfer (OP_READ_BLOCKS, 0, 1);
   Allow_Write := False; Reject; Allow_Write := True;
   Transfer (OP_WRITE_BLOCKS, 0, 1);
   Allow_Read := False; Reject; Allow_Read := True;

   Buffer := [others => 'W'];
   Transfer (OP_WRITE_BLOCKS, 15, 1);
   Allow_Write := False; -- writes only need to READ the caller's grant
   Ram_Device.Handle (42, Request, Response);
   pragma Assert (Response.tag.label = REPLY_OK and Response.words (0) = 512);
   pragma Assert (Disk (1 .. 7680) = [1 .. 7680 => 'D']);
   pragma Assert (Disk (7681 .. 8192) = [7681 .. 8192 => 'W'] and Active = 0);
   Allow_Write := True; Allow_Read := False;
   Buffer := [others => '?'];
   Transfer (OP_READ_BLOCKS, 15, 1);
   Ram_Device.Handle (42, Request, Response);
   pragma Assert (Response.tag.label = REPLY_OK and Response.words (0) = 512);
   pragma Assert (Buffer (1 .. 512) = [1 .. 512 => 'W']);
   pragma Assert (Buffer (513 .. 4096) = [513 .. 4096 => '?'] and Active = 0);
   Allow_Read := True;
   Transfer (OP_READ_BLOCKS, 0, 9); Reject; -- grant smaller than transfer
   Transfer (OP_WRITE_BLOCKS, 0, 8);
   Buffer := [others => 'M'];
   Ram_Device.Handle (42, Request, Response);
   pragma Assert (Response.tag.label = REPLY_OK and Response.words (0) = 4096);
   pragma Assert (Disk (1 .. 4096) = [1 .. 4096 => 'M']);

   Allow_Return := False;
   Transfer (OP_WRITE_BLOCKS, 0, 1);
   Buffer := [others => 'X'];
   Ram_Device.Handle (42, Request, Response);
   pragma Assert (Response.tag.label = REPLY_ERROR and Response.words (0) = 0);
   pragma Assert (Disk (1 .. 512) = [1 .. 512 => 'X'] and Active = 0);
   --  A failed return reports uncertainty, never a falsely successful write.
   Put_Line ("RAM-BLOCK-CHECK: PASS (" & Natural'Image (Failures) & " rejected requests)");
end Main;
