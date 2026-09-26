with Interfaces; use Interfaces;
with Config_Worker_Protocol;
with CuBit.Grant_References;
with CCL.Objects.Schemas;
with Config_Schema_Protocol;

package body Config_Worker_Messages is
   function Type_Request (Grant : CuBit.Memory_Grants.Grant_Reference) return CuBit.Messages.Message is
      Item : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      Item.tag := (Operation'Enum_Rep (Exchange_Type), 4, 0, 0);
      Item.words := [Grant.slot, Grant.generation, Config_Schema_Protocol.Frame_Bytes,
                     Unsigned_64 (Config_Schema_Protocol.Version)];
      return Item;
   end Type_Request;
   function Valid_Type_Request (Item : CuBit.Messages.Message) return Boolean is
     (Item.tag.label = Operation'Enum_Rep (Exchange_Type) and then
      Item.tag.length = 4 and then Item.tag.flags = 0 and then Item.tag.reserved = 0 and then
      Item.words (0) <= CuBit.Grant_References.Maximum_Slot and then
      Item.words (1) in CuBit.Grant_References.Generation and then
      Item.words (2) = Config_Schema_Protocol.Frame_Bytes and then
      Item.words (3) = Unsigned_64 (Config_Schema_Protocol.Version));
   function Type_Acknowledgment return CuBit.Messages.Message is
      Item : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      Item.tag := (Status'Enum_Rep (Type_Ready), 1, 0, 0);
      Item.words (0) := Config_Schema_Protocol.Frame_Bytes;
      return Item;
   end Type_Acknowledgment;
   function Valid_Type_Acknowledgment (Item : CuBit.Messages.Message) return Boolean is
     (Item.tag.label = Status'Enum_Rep (Type_Ready) and then Item.tag.length = 1 and then
      Item.tag.flags = 0 and then Item.tag.reserved = 0 and then
      Item.words (0) = Config_Schema_Protocol.Frame_Bytes and then
      (for all I in 1 .. 3 => Item.words (I) = 0));
   function Request (Grant : CuBit.Memory_Grants.Grant_Reference)
      return CuBit.Messages.Message
   is
      Item : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      Item.tag := (Operation'Enum_Rep (Exchange_Frame), 4, 0, 0);
      Item.words := [Grant.slot, Grant.generation,
                     Config_Worker_Protocol.Frame_Bytes,
                     Unsigned_64 (Config_Worker_Protocol.Version)];
      return Item;
   end Request;

   function Valid_Request (Item : CuBit.Messages.Message) return Boolean is
     (Item.tag.label = Operation'Enum_Rep (Exchange_Frame) and then
      Item.tag.length = 4 and then Item.tag.flags = 0 and then Item.tag.reserved = 0 and then
      Item.words (0) <= CuBit.Grant_References.Maximum_Slot and then
      Item.words (1) in CuBit.Grant_References.Generation and then
      Item.words (2) = Config_Worker_Protocol.Frame_Bytes and then
      Item.words (3) = Unsigned_64 (Config_Worker_Protocol.Version));

   function Error (Code : Status) return CuBit.Messages.Message is
      Item : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      Item.tag := (Status'Enum_Rep (Code), 0, 0, 0);
      return Item;
   end Error;

   function Acknowledgment return CuBit.Messages.Message is
      Item : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      Item.tag := (Status'Enum_Rep (Frame_Ready), 1, 0, 0);
      Item.words (0) := Config_Worker_Protocol.Frame_Bytes;
      return Item;
   end Acknowledgment;

   function Valid_Acknowledgment (Item : CuBit.Messages.Message) return Boolean is
     (Item.tag.label = Status'Enum_Rep (Frame_Ready) and then
      Item.tag.length = 1 and then Item.tag.flags = 0 and then
      Item.tag.reserved = 0 and then
      Item.words (0) = Config_Worker_Protocol.Frame_Bytes and then
      (for all I in 1 .. 3 => Item.words (I) = 0));

   function Schema_Request (Grant : CuBit.Memory_Grants.Grant_Reference)
      return CuBit.Messages.Message
   is
      Item : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      Item.tag := (Operation'Enum_Rep (Provision_Schema), 4, 0, 0);
      Item.words := [Grant.slot, Grant.generation, CCL.Objects.Schemas.Native_Schema_Bytes,
                     Unsigned_64 (CCL.Objects.Schemas.Version)];
      return Item;
   end Schema_Request;

   function Valid_Schema_Request (Item : CuBit.Messages.Message) return Boolean is
     (Item.tag.label = Operation'Enum_Rep (Provision_Schema) and then
      Item.tag.length = 4 and then Item.tag.flags = 0 and then Item.tag.reserved = 0 and then
      Item.words (0) <= CuBit.Grant_References.Maximum_Slot and then
      Item.words (1) in CuBit.Grant_References.Generation and then
      Item.words (2) = CCL.Objects.Schemas.Native_Schema_Bytes and then
      Item.words (3) = Unsigned_64 (CCL.Objects.Schemas.Version));

   function Schema_Acknowledgment return CuBit.Messages.Message is
      Item : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      Item.tag := (Status'Enum_Rep (Schema_Ready), 1, 0, 0);
      Item.words (0) := CCL.Objects.Schemas.Native_Schema_Bytes;
      return Item;
   end Schema_Acknowledgment;

   function Valid_Schema_Acknowledgment (Item : CuBit.Messages.Message) return Boolean is
     (Item.tag.label = Status'Enum_Rep (Schema_Ready) and then
      Item.tag.length = 1 and then Item.tag.flags = 0 and then Item.tag.reserved = 0 and then
      Item.words (0) = CCL.Objects.Schemas.Native_Schema_Bytes and then
      (for all I in 1 .. 3 => Item.words (I) = 0));
end Config_Worker_Messages;
