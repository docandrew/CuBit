pragma Ada_2022;
with ACPI_Protocol;
with ACPI_Runtime_Query_Polling;
package body ACPI_Native_Query_Input with SPARK_Mode => Off is
   function Classify
     (Sender : CuBit.Messages.Process_ID; Packet : CuBit.Messages.Message;
      Request_Capacity, Response_Capacity : Wire.Word) return Classification is
      Selected, Decoded : Boolean;
      Reference_Word, Declared_Length : Wire.Word;
   begin
      Decode (Packet, Selected, Decoded, Reference_Word, Declared_Length);
      if not Selected then return (Selected => False, Result => (Status => Admission.Malformed)); end if;
      return (Selected => True,
        Result => Admission.Admit
          (Sender, ACPI_Protocol.Role_Tag (Packet.authorityTag), Decoded,
           Reference_Word, Declared_Length, Request_Capacity, Response_Capacity));
   end Classify;
   procedure Poll
     (Request_Capacity, Response_Capacity : Wire.Word;
      Sender : out CuBit.Messages.Process_ID; Packet : out CuBit.Messages.Message;
      Found : out Boolean; Result : out Classification) is
   begin
      Result := (others => <>);
      ACPI_Runtime_Query_Polling.Poll_Request (Sender, Packet, Found);
      if not Found then
         Sender := Admission.PID.No_Process; Packet := CuBit.Messages.NULL_MESSAGE;
         return;
      end if;
      Result := Classify (Sender, Packet, Request_Capacity, Response_Capacity);
   end Poll;
end ACPI_Native_Query_Input;
