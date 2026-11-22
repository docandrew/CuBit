pragma Ada_2022;
with Interfaces;
with ACPI_Inventory_Protocol;
package body ACPI_Backend_Catalog_Endpoint is
   package Proto renames ACPI_Inventory_Protocol;
   package Wire renames Proto.Wire;
   use type Interfaces.Unsigned_32;
   procedure Dispatch
     (State : in out Catalog.Context;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message)
   is
      Item : constant Wire.Packet :=
        (Label => Request.tag.label, Length => Request.tag.length,
         Flags => CuBit.Messages.Flag_Byte (Request.tag), Reserved => Request.tag.reserved,
         Data => Wire.Words (Request.words));
      Result : Wire.Packet;
   begin
      if Item.Label = Proto.Header_Operation or else Item.Label = Proto.Part_Operation then
         Inventory.Dispatch (State, Request.authorityTag, Item, Result);
      else
         Catalog.Dispatch (State, Request.authorityTag, Item, Result);
      end if;
      Reply := CuBit.Messages.NULL_MESSAGE;
      Reply.tag := (label => Result.Label, length => Result.Length,
                    protocol => CuBit.Messages.Protocol_Flags (Result.Flags), ipc => <>, reserved => Result.Reserved);
      Reply.words := CuBit.Messages.MessageWords (Result.Data);
   end Dispatch;
end ACPI_Backend_Catalog_Endpoint;
