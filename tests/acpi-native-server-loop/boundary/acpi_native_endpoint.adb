with ACPI_Observer_Endpoint;
with ACPI_Protocol;
with CuBit.Refusal_Notes;
package body ACPI_Native_Endpoint is
   procedure Dispatch
     (Server : in out ACPI_Requests.State;
      From : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message) is
      Result : ACPI_Requests.Packet;
      Stamp : constant ACPI_Protocol.Role_Tag :=
        ACPI_Protocol.Role_Tag (Request.authorityTag);
   begin
      if not ACPI_Protocol.Is_Observer (Stamp) then
         CuBit.Refusal_Notes.Warn_Once
           ("acpi", From'Image & " observer",
            "process" & From'Image & " undeclared observer");
      end if;
      ACPI_Observer_Endpoint.Dispatch (Server, Stamp,
        (Label => Request.tag.label, Length => Request.tag.length,
         Flags => CuBit.Messages.Flag_Byte (Request.tag),
         Reserved => Request.tag.reserved,
         Data => ACPI_Requests.Words (Request.words)), Result);
      Reply := CuBit.Messages.NULL_MESSAGE;
      Reply.tag := (label => Result.Label, length => Result.Length,
                    protocol => 0, ipc => <>, reserved => Result.Reserved);
      Reply.words := CuBit.Messages.MessageWords (Result.Data);
   end Dispatch;
end ACPI_Native_Endpoint;
