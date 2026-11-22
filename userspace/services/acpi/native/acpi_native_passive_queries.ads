pragma Ada_2022;
with ACPI_Requests;with CuBit.Messages;with Interfaces;
package ACPI_Native_Passive_Queries is
   procedure Dispatch(Server:in out ACPI_Requests.State;Sender:CuBit.Messages.Process_ID;
      Packet:CuBit.Messages.Message;Admission_Open:Boolean;
      Reply:out CuBit.Messages.Message;Handled,Reply_Now:out Boolean);
   procedure Advance(Server:in out ACPI_Requests.State;Did_Work:out Boolean);
   procedure Maintain(Deadline:out Interfaces.Unsigned_64;May_Continue:out Boolean);
   procedure Begin_Stop;
   procedure Drain(Server:in out ACPI_Requests.State;Complete:out Boolean);
   function Quiet return Boolean;
end ACPI_Native_Passive_Queries;
