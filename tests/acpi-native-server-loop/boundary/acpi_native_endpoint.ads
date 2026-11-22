with ACPI_Requests;
with CuBit.Messages;
-- Request must be received from the kernel. From is used for refusal logging
-- and reply delivery only; no process identity or message word grants authority.
package ACPI_Native_Endpoint is
   procedure Dispatch
     (Server : in out ACPI_Requests.State;
      From : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message);
end ACPI_Native_Endpoint;
