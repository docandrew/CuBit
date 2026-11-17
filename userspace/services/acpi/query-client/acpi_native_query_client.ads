pragma Ada_2022;
with CuBit.Handles;with ACPI_Passive_Query.Pages;with ACPI_Query_Transport;
with ACPI_Query_Client_Lifetime;with ACPI_Query_Client_Results;
generic
   Endpoint:CuBit.Handles.Endpoint;
package ACPI_Native_Query_Client with SPARK_Mode=>Off is
   Page_Bytes:constant Positive:=4096;
   subtype Buffer is ACPI_Passive_Query.Pages.Byte_Array(0..Page_Bytes-1);
   subtype Report is ACPI_Query_Client_Results.Report;
   -- Keep this instance and its page alive at a stable address until
   -- Cleanup_Pending=False, including after failure or timeout. A library/
   -- process-lifetime instance is suitable for native test tools.
   -- For console/application callers above ACPI in the layering. Call is
   -- synchronous with this explicit absolute kernel-millisecond deadline.
   -- Never use from an owner to which ACPI may synchronously call back.
   procedure Start(Request_Data:ACPI_Passive_Query.Pages.Byte_Array;
      Deadline:ACPI_Query_Transport.Word;Accepted:out Boolean);
   procedure Progress(Did_Work:out Boolean);
   function Status return ACPI_Query_Client_Lifetime.Phase;
   function Cleanup_Pending return Boolean;
   -- Typed envelope/correlation only. Validate each node/resource/description
   -- item with its shared typed reader before publishing observations.
   procedure Take(Value:out Report;Data:out Buffer;Taken:out Boolean);
end ACPI_Native_Query_Client;
