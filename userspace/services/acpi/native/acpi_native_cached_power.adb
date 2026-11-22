pragma Ada_2022;
with ACPI_Cached_Power_Codec;
with ACPI_Cached_Power_Dispatch;
with ACPI_Native_Event_Domain;
with ACPI_Protocol;
with ACPI_Endpoint;
package body ACPI_Native_Cached_Power is
   use type Interfaces.Unsigned_32, Interfaces.Unsigned_64;
   pragma Compile_Time_Error (ACPI_Requests.Read_Metrics /= ACPI_Power_Observation_Protocol.Read_Metrics_Label,
     "cached observer operation differs from request core");
   package Codec is new ACPI_Cached_Power_Codec
     (ACPI_Power_Observation_Protocol.Read_Metrics_Label, Metrics_Lid_Page, Metrics_Source_Page);
   pragma Compile_Time_Error (Codec.Summary_Page /= ACPI_Power_Observation_Protocol.Summary_Page
     or else Codec.Time_Page /= ACPI_Power_Observation_Protocol.Time_Page
     or else Codec.Raw_Page /= ACPI_Power_Observation_Protocol.Raw_Page,
     "cached observer subpages differ from codec");
   package Reader is new ACPI_Cached_Power_Dispatch
     (ACPI_Requests.State, Codec, ACPI_Native_Event_Domain.Read_Power);
   procedure Dispatch (Server : ACPI_Requests.State; Request : CuBit.Messages.Message;
      Read_Allowed : Boolean; Reply : out CuBit.Messages.Message; Handled : out Boolean) is
      Observed : Reader.Reply;
      Result : ACPI_Requests.Response;
      Packet : ACPI_Requests.Packet;
   begin
      Reply := CuBit.Messages.NULL_MESSAGE;
      Handled := False;
      Reader.Dispatch (Server, ACPI_Protocol.Role_Tag (Request.authorityTag),
        (Label => Request.tag.label, Length => Request.tag.length,
         Flags => CuBit.Messages.Flag_Byte (Request.tag), Reserved => Request.tag.reserved,
         Data => Codec.Words (Request.words)), Observed, Read_Allowed);
      case Observed.Status is
         when Reader.Not_Handled => return;
         when Reader.Denied => Result := (Status => ACPI_Requests.Denied, others => <>);
         when Reader.Malformed => Result := (Status => ACPI_Requests.Malformed, others => <>);
         when Reader.Stale => Result := (Status => ACPI_Requests.Stale, others => <>);
         when Reader.OK => Result := (ACPI_Requests.OK, ACPI_Requests.Response_Words (Observed.Data));
      end case;
      Handled := True;
      Packet := ACPI_Endpoint.Encode (Result);
      Reply.tag := (label => Packet.Label, length => Packet.Length,
        protocol => 0, ipc => <>, reserved => Packet.Reserved);
      Reply.words := CuBit.Messages.MessageWords (Packet.Data);
   end Dispatch;
end ACPI_Native_Cached_Power;
