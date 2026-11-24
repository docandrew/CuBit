package body ACPI_Endpoint is
 use type ACPI_Requests.Outcome;
 function Encode(R:ACPI_Requests.Response) return ACPI_Requests.Packet is
 (if R.Status=ACPI_Requests.OK then (Reply_OK,4,R.Data)
  else (Reply_Error,4,[Interfaces.Unsigned_64(ACPI_Requests.Outcome'Pos(R.Status)),R.Data(0),R.Data(1),0]));
end ACPI_Endpoint;
