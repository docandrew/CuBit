with ACPI_Requests;
with CuBit.Messages;
package ACPI_Native_Server is
   type Stop_Reason is (Wait_Unavailable, Unexpected_Completion);
   procedure Run (Server : in out ACPI_Requests.State; Reason : out Stop_Reason);
   generic
      with procedure Dispatch_Message
        (Server : in out ACPI_Requests.State; From : CuBit.Messages.Process_ID;
         Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message);
   package With_Dispatch is
      procedure Serve (Server : in out ACPI_Requests.State; Reason : out Stop_Reason);
   end With_Dispatch;
end ACPI_Native_Server;
