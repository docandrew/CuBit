with ACPI_Native_Query_Codec;
with ACPI_Native_Query_Input;
with ACPI_Runtime_Query_Replies;
with ACPI_Query_Transport;
package Native_Codec_Bindings is
   package Input is new ACPI_Native_Query_Input (ACPI_Native_Query_Codec.Decode);
   package Replies is new ACPI_Runtime_Query_Replies
     (ACPI_Query_Transport.Reply_Details, ACPI_Native_Query_Codec.Reply);
end Native_Codec_Bindings;
