with CuBit.Messages;
package body ACPI_Native_Passive_Queries is
 procedure Advance(Server:in out ACPI_Requests.State;Worked:out Boolean) is
  pragma Unreferenced(Server);begin Worked:=False;end Advance;
 procedure Maintain(Deadline:out Interfaces.Unsigned_64;Healthy:out Boolean) is
 begin Deadline:=CuBit.Messages.Wait_Forever;Healthy:=True;end Maintain;
end ACPI_Native_Passive_Queries;
