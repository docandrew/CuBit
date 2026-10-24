--  Hosted regression tests for the shared Block.Device.V1 front end in the
--  configurations its drivers use: NVMe (flush, FUA, volatile cache, 4 KiB
--  blocks), USB optical (read only, 2 KiB, 16 blocks), ATA, an absent
--  device and a faulted one. The kernel grant API is the ram-block fixture.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants; use CuBit.Memory_Grants;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Block_Requests; use CuBit.Block_Requests;
with Proof_Device; use Proof_Device;

procedure Main is
   Owner : constant Process_ID := 42;
   Checks : Natural := 0;

   NVMe : constant Device_State :=
     (Presence => Available, Blocks => 64, Logical_Size => 4096,
      Physical_Size => 4096, Max_Transfer => 32,
      Features => FEATURE_FLUSH or FEATURE_FUA or FEATURE_VOLATILE_CACHE,
      Media => Fixed_Media);
   Optical : constant Device_State :=
     (Presence => Available, Blocks => 100, Logical_Size => 2048,
      Physical_Size => 2048, Max_Transfer => 16,
      Features => FEATURE_READ_ONLY or FEATURE_REMOVABLE,
      Media => Optical_Media);
   ATA : constant Device_State :=
     (Presence => Available, Blocks => 16#1000_0000#, Logical_Size => 512,
      Physical_Size => 512, Max_Transfer => 255,
      Features => FEATURE_FLUSH, Media => Fixed_Media);

   function Request
     (Label : Unsigned_32; LBA, Count : Unsigned_64;
      Flags : Protocol_Flags := 0) return Message is
     ((tag => (label => Label, length => 4, protocol => Flags, ipc => <>,
               reserved => 0),
       authorityTag => 0, words => [LBA, 1, Count, 7]));

   function Bare (Label : Unsigned_32) return Message is
     ((tag => (label => Label, length => 0, protocol => 0, ipc => <>,
               reserved => 0),
       authorityTag => 0, words => [others => 0]));

   Response : Message;

   procedure Expect_Reject (State : Device_State; Item : Message) is
      Before_Transfers : constant Natural := Transfers;
      Before_Flushes : constant Natural := Flush_Calls;
      Before_Acquires : constant Natural := Acquires;
   begin
      Server.Handle (State, Owner, Item, Response);
      pragma Assert (Response.tag.label = REPLY_ERROR and
                     Response.tag.length = 1 and Response.words (0) = 0);
      pragma Assert (Transfers = Before_Transfers and
                     Flush_Calls = Before_Flushes and Acquires = Before_Acquires);
      pragma Assert (Decode (Item, State).Kind = Rejected or
                     Item.tag.label = OP_FLUSH_DEVICE);
      Checks := Checks + 1;
   end Expect_Reject;

   procedure Expect_Transfer
     (State : Device_State; Item : Message; Direction : Transfer_Direction;
      FUA : Boolean := False)
   is
      Before : constant Natural := Transfers;
   begin
      Server.Handle (State, Owner, Item, Response);
      pragma Assert (Transfers = Before + 1 and Active = 0);
      pragma Assert (Last_Item.Direction = Direction and
                     Last_Item.Force_Unit_Access = FUA and
                     Last_Item.First_Block = Item.words (0) and
                     Last_Item.Count = Item.words (2));
      pragma Assert (Response.tag.label = REPLY_OK and
                     Response.tag.length = 1 and
                     Response.words (0) = Bytes (Last_Item));
      Checks := Checks + 1;
   end Expect_Transfer;

   Description : Device_Description;
   OK : Boolean;
begin
   --  Describe: each presence answers as the protocol says.
   Server.Handle (NVMe, Owner, Bare (OP_DESCRIBE_DEVICE), Response);
   pragma Assert (Response.tag.label = REPLY_OK and Response.tag.length = 4);
   OK := Decode_Description
     (Response.words (0), Response.words (1), Response.words (2),
      Response.words (3), Description);
   pragma Assert (OK and Description.blockCount = 64 and
                  Description.logicalBlockSize = 4096 and
                  Description.maxTransferBlocks = 32 and
                  Has_Volatile_Cache (Description) and
                  Supports_FUA (Description) and Can_Persist (Description));
   Server.Handle (Optical, Owner, Bare (OP_DESCRIBE_DEVICE), Response);
   OK := Decode_Description
     (Response.words (0), Response.words (1), Response.words (2),
      Response.words (3), Description);
   pragma Assert (OK and Is_Read_Only (Description) and
                  Description.media = Optical_Media);
   Server.Handle ((Presence => No_Device), Owner, Bare (OP_DESCRIBE_DEVICE),
                  Response);
   pragma Assert (Response.tag.label = REPLY_NO_DEVICE and
                  Response.tag.length = 1 and Response.words (0) = 0);
   Server.Handle ((Presence => Faulted), Owner, Bare (OP_DESCRIBE_DEVICE),
                  Response);
   pragma Assert (Response.tag.label = REPLY_ERROR);
   pragma Assert (Acquires = 0 and Transfers = 0);

   --  Malformed describes and unknown operations.
   declare
      Item : Message := Bare (OP_DESCRIBE_DEVICE);
   begin
      Item.tag.length := 1; Expect_Reject (NVMe, Item);
      Item.tag.length := 0; Item.tag.protocol := 1; Expect_Reject (NVMe, Item);
      Item.tag.protocol := 0; Item.tag.reserved := 1; Expect_Reject (NVMe, Item);
      Expect_Reject (NVMe, Bare (16#0214#));
      Expect_Reject (NVMe, Request (16#0214#, 0, 1));
   end;

   --  NVMe: reads, writes, FUA only on writes, short transfers.
   Expect_Transfer (NVMe, Request (OP_READ_BLOCKS, 63, 1), Device_Read);
   Expect_Transfer (NVMe, Request (OP_WRITE_BLOCKS, 0, 1), Device_Write);
   Expect_Transfer
     (NVMe, Request (OP_WRITE_BLOCKS, 5, 1, WRITE_FLAG_FUA), Device_Write,
      FUA => True);
   Expect_Reject (NVMe, Request (OP_READ_BLOCKS, 5, 1, WRITE_FLAG_FUA));
   Expect_Reject (NVMe, Request (OP_WRITE_BLOCKS, 5, 1, 2));
   Expect_Reject (NVMe, Request (OP_READ_BLOCKS, 64, 1));
   Expect_Reject (NVMe, Request (OP_READ_BLOCKS, 63, 2));
   Expect_Reject (NVMe, Request (OP_READ_BLOCKS, 0, 33));
   Expect_Reject (NVMe, Request (OP_READ_BLOCKS, 0, 0));
   Expect_Reject (NVMe, Request (OP_READ_BLOCKS, Unsigned_64'Last, 2));
   Expect_Reject (NVMe, Request (OP_READ_BLOCKS, 1, Unsigned_64'Last));
   declare
      Item : Message := Request (OP_READ_BLOCKS, 0, 1);
   begin
      Item.words (1) := MAXIMUM_GLOBAL_SLOT + 1; Expect_Reject (NVMe, Item);
      Item.words (1) := 1; Item.words (3) := 0; Expect_Reject (NVMe, Item);
      Item.words (3) := MAXIMUM_GENERATION + 1; Expect_Reject (NVMe, Item);
      Item.words (3) := 7; Item.tag.length := 3; Expect_Reject (NVMe, Item);
      Item.tag.length := 4; Item.tag.reserved := 1; Expect_Reject (NVMe, Item);
   end;

   --  A short transfer is an error that keeps the completed prefix.
   Short_By := 512;
   Server.Handle (NVMe, Owner, Request (OP_READ_BLOCKS, 0, 1), Response);
   pragma Assert (Response.tag.label = REPLY_ERROR and
                  Response.words (0) = 4096 - 512 and Active = 0);
   Short_By := 0;
   --  A grant the kernel refuses (here: larger than the loan) moves nothing.
   declare
      Before : constant Natural := Transfers;
   begin
      Server.Handle (NVMe, Owner, Request (OP_READ_BLOCKS, 0, 2), Response);
      pragma Assert (Response.tag.label = REPLY_ERROR and
                     Response.words (0) = 0 and Transfers = Before);
      Server.Handle (NVMe, 43, Request (OP_READ_BLOCKS, 0, 1), Response);
      pragma Assert (Response.tag.label = REPLY_ERROR and Transfers = Before);
   end;
   --  A failed grant return makes the outcome uncertain: zero bytes.
   Allow_Return := False;
   Server.Handle (NVMe, Owner, Request (OP_WRITE_BLOCKS, 0, 1), Response);
   pragma Assert (Response.tag.label = REPLY_ERROR and Response.words (0) = 0);
   Allow_Return := True;

   --  Flush goes to the driver only when advertised, and reports its result.
   Server.Handle (NVMe, Owner, Bare (OP_FLUSH_DEVICE), Response);
   pragma Assert (Response.tag.label = REPLY_OK and Flush_Calls = 1);
   Flush_Result := False;
   Server.Handle (NVMe, Owner, Bare (OP_FLUSH_DEVICE), Response);
   pragma Assert (Response.tag.label = REPLY_ERROR and Flush_Calls = 2);
   Flush_Result := True;
   declare
      Item : Message := Bare (OP_FLUSH_DEVICE);
   begin
      Item.tag.length := 1; Expect_Reject (NVMe, Item);
   end;
   Expect_Reject (Optical, Bare (OP_FLUSH_DEVICE));
   pragma Assert (Flush_Calls = 2);

   --  Optical: read only, 16-block transfers.
   Expect_Transfer (Optical, Request (OP_READ_BLOCKS, 99, 1), Device_Read);
   Expect_Transfer (Optical, Request (OP_READ_BLOCKS, 0, 2), Device_Read);
   Expect_Reject (Optical, Request (OP_WRITE_BLOCKS, 0, 1));
   Expect_Reject (Optical, Request (OP_WRITE_BLOCKS, 0, 1, WRITE_FLAG_FUA));
   Expect_Reject (Optical, Request (OP_READ_BLOCKS, 0, 17));
   Expect_Reject (Optical, Request (OP_READ_BLOCKS, 99, 2));

   --  ATA: 255-block limit, no FUA.
   Expect_Transfer (ATA, Request (OP_READ_BLOCKS, 16#0FFF_FFF0#, 8), Device_Read);
   Expect_Reject (ATA, Request (OP_READ_BLOCKS, 0, 256));
   Expect_Reject (ATA, Request (OP_WRITE_BLOCKS, 0, 1, WRITE_FLAG_FUA));

   --  Nothing is served without an available device.
   Expect_Reject ((Presence => No_Device), Request (OP_READ_BLOCKS, 0, 1));
   Expect_Reject ((Presence => Faulted), Request (OP_WRITE_BLOCKS, 0, 1));
   Expect_Reject ((Presence => Faulted), Bare (OP_FLUSH_DEVICE));
   pragma Assert (Active = 0);

   Put_Line ("BLOCK-REQUESTS-CHECK: PASS (" & Natural'Image (Checks) &
             " request checks)");
end Main;
