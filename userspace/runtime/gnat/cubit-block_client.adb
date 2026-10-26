------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;

package body CuBit.Block_Client is

   Describe_Words : constant := 4;
   Transfer_Words : constant := 4;
   Status_Words : constant := 1;

   function Request
     (Label : Unsigned_32; Length : Unsigned_8; Flags : Protocol_Flags;
      Words : MessageWords) return Message is
     ((tag => (label => Label, length => Length, protocol => Flags, ipc => <>,
               reserved => 0),
       authorityTag => 0, words => Words));

   procedure Open
     (Endpoint : CuBit.Handles.Endpoint;
      Pages    : Page_Count;
      Item     : out Session;
      Result   : out Open_Result)
   is
      Msg : Message := Request (OP_DESCRIBE_DEVICE, 0, 0, [others => 0]);
      Ignore : MessageTag;
      Raw : Unsigned_64;
      Granted : Boolean;
   begin
      Item := (others => <>);
      Ignore := capCall (Endpoint, Msg, Wait_Forever);
      if Msg.tag.label /= REPLY_OK or else Msg.tag.length /= Describe_Words
        or else not Decode_Description
          (Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3),
           Item.Device.description)
      then
         Result := Describe_Failed;
         return;
      end if;
      --  One page more than the grant, so the grant can start page aligned.
      Raw := syscall (SYSCALL_SBRK, Unsigned_64 (Pages + 1) * Page_Bytes);
      if Raw = 0 or else Raw = Unsigned_64'Last then
         Result := No_Memory;
         return;
      end if;
      Item.Device.grantBuffer :=
        To_Address (Integer_Address ((Raw + Page_Bytes - 1) and not (Page_Bytes - 1)));
      CuBit.Memory_Grants.Create_Via_Capability
        (Endpoint, Item.Device.grantBuffer, Pages, True, Item.Device.grant, Granted);
      if not Granted then
         Result := Grant_Refused;
         return;
      end if;
      Item.Device.endpoint := Endpoint;
      Item.Device.grantBytes := Unsigned_32 (Pages * Page_Bytes);
      Item.Pages := Pages;
      Item.Open := True;
      Result := Opened;
   end Open;

   function Transfer_Limit (Item : Session) return Unsigned_64 is
     (Unsigned_64'Min
        (Unsigned_64 (Item.Device.description.maxTransferBlocks),
         Unsigned_64 (Item.Device.grantBytes) /
           Unsigned_64 (Item.Device.description.logicalBlockSize)));

   function Buffer (Item : Session) return System.Address is
     (Item.Device.grantBuffer);

   --  One OP_READ_BLOCKS or OP_WRITE_BLOCKS call through the grant.
   procedure Move
     (Item : Session; Label : Unsigned_32; Flags : Protocol_Flags;
      First, Count : Unsigned_64;
      Moved : out Unsigned_64; Result : out Transfer_Result);

   procedure Move
     (Item : Session; Label : Unsigned_32; Flags : Protocol_Flags;
      First, Count : Unsigned_64;
      Moved : out Unsigned_64; Result : out Transfer_Result)
   is
      Expected : constant Unsigned_64 :=
        Count * Unsigned_64 (Item.Device.description.logicalBlockSize);
      Msg : Message := Request
        (Label, Transfer_Words, Flags,
         [First, Item.Device.grant.slot, Count, Item.Device.grant.generation]);
      Ignore : MessageTag;
   begin
      Ignore := capCall (Item.Device.endpoint, Msg, Wait_Forever);
      Moved := (if Msg.tag.length = Status_Words then Msg.words (0) else 0);
      if Msg.tag.length /= Status_Words then
         Result := Refused;
      elsif Msg.tag.label = REPLY_OK and then Moved = Expected then
         Result := Transferred;
      elsif Moved = 0 then
         Result := Refused;
      else
         Result := Short_Transfer;
      end if;
   end Move;

   procedure Read
     (Item : Session; First : Unsigned_64; Count : Unsigned_64;
      Moved : out Unsigned_64; Result : out Transfer_Result) is
   begin
      Move (Item, OP_READ_BLOCKS, 0, First, Count, Moved, Result);
   end Read;

   procedure Write
     (Item : Session; First : Unsigned_64; Count : Unsigned_64;
      Force_Unit_Access : Boolean;
      Moved : out Unsigned_64; Result : out Transfer_Result) is
   begin
      Move (Item, OP_WRITE_BLOCKS,
            (if Force_Unit_Access then WRITE_FLAG_FUA else 0),
            First, Count, Moved, Result);
   end Write;

   procedure Flush (Item : Session; Durable : out Boolean) is
      Msg : Message := Request (OP_FLUSH_DEVICE, 0, 0, [others => 0]);
      Ignore : MessageTag;
   begin
      Ignore := capCall (Item.Device.endpoint, Msg, Wait_Forever);
      Durable := Msg.tag.label = REPLY_OK and then Msg.tag.length = Status_Words;
   end Flush;

end CuBit.Block_Client;
