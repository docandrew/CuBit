with ACPI_Requests; with CuBit.Messages; with Interfaces;
package ACPI_Native_Passive_Queries is
 procedure Dispatch (Server : in out ACPI_Requests.State; From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message; Admission_Open : Boolean; Reply : out CuBit.Messages.Message; Handled, Reply_Now : out Boolean);
 procedure Advance (Server : in out ACPI_Requests.State; Worked : out Boolean);
 procedure Maintain (Deadline : out Interfaces.Unsigned_64; Healthy : out Boolean);
end ACPI_Native_Passive_Queries;
