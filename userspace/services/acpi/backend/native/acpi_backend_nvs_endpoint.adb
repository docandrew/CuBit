pragma Ada_2022;
with ACPI_NVS_Protocol;
package body ACPI_Backend_NVS_Endpoint is
   package P renames ACPI_NVS_Protocol;
   procedure Dispatch
     (State : in out Records.Context; Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message) is
      Item : constant P.Packet :=
        (Label => Request.tag.label, Length => Request.tag.length,
         Flags => CuBit.Messages.Flag_Byte (Request.tag), Reserved => Request.tag.reserved,
         Data => P.Wire.Words (Request.words));
      Result : P.Packet;
   begin
      Driver.Dispatch (State, Request.authorityTag, Item, Result);
      Reply := CuBit.Messages.NULL_MESSAGE;
      Reply.tag := (label => Result.Label, length => Result.Length,
        protocol => CuBit.Messages.Protocol_Flags (Result.Flags), ipc => <>, reserved => Result.Reserved);
      Reply.words := CuBit.Messages.MessageWords (Result.Data);
   end Dispatch;
end ACPI_Backend_NVS_Endpoint;
