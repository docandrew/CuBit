pragma Ada_2022;
package body ACPI_Observer_Endpoint with SPARK_Mode is
   procedure Dispatch
     (Server : in out ACPI_Requests.State; Stamp : ACPI_Protocol.Role_Tag;
      Request : ACPI_Requests.Packet; Reply : out ACPI_Requests.Packet) is
      Result : ACPI_Requests.Response;
   begin
      ACPI_Requests.Handle (Server,
        (if ACPI_Protocol.Is_Observer (Stamp) then ACPI_Requests.Observer
         else ACPI_Requests.No_Authority), Request, Result);
      Reply := ACPI_Endpoint.Encode (Result);
   end Dispatch;
end ACPI_Observer_Endpoint;
