package body ACPI_Native_Endpoint is
   procedure Dispatch
     (Adapter : in out ACPI_Native_Blocks.State;
      Server : in out ACPI_Requests.State;
      Config : ACPI_Endpoint.Configuration;
      Provider_Slot : CuBit.Messages.CapabilitySlot;
      Request : CuBit.Messages.Message;
      Reply : out CuBit.Messages.Message)
   is
      Result : ACPI_Requests.Packet;
   begin
      ACPI_Native_Blocks.Dispatch
        (Adapter, Server, Config, Request.authorityTag, Provider_Slot,
         (Label => Request.tag.label, Length => Request.tag.length,
          Flags => Request.tag.flags, Reserved => Request.tag.reserved,
          Data => ACPI_Requests.Words (Request.words)), Result);
      Reply := CuBit.Messages.NULL_MESSAGE;
      Reply.tag := (label => Result.Label, length => Result.Length,
                    flags => Result.Flags, reserved => Result.Reserved);
      Reply.words := CuBit.Messages.MessageWords (Result.Data);
   end Dispatch;
end ACPI_Native_Endpoint;
