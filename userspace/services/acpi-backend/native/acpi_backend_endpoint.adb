with ACPI_Region_Protocol;
package body ACPI_Backend_Endpoint is
   procedure Dispatch
     (Region : in out ACPI_Region_Policy.State;
      Hardware : in out Hardware_State;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message;
      Pending_Ticket : out Interfaces.Unsigned_64) is
      Result : ACPI_Region_Protocol.Packet;
   begin
      Core.Dispatch
        (Region, Hardware, Request.authorityTag,
         (Label => Request.tag.label, Length => Request.tag.length,
          Flags => Request.tag.flags, Reserved => Request.tag.reserved,
          Data => ACPI_Region_Protocol.Words (Request.words)), Result, Pending_Ticket);
      Reply := CuBit.Messages.NULL_MESSAGE;
      Reply.tag := (label => Result.Label, length => Result.Length,
                    flags => Result.Flags, reserved => Result.Reserved);
      Reply.words := CuBit.Messages.MessageWords (Result.Data);
   end Dispatch;
end ACPI_Backend_Endpoint;
