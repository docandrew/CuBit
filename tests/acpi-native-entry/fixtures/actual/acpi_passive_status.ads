pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Passive_Members;
with ACPI_Passive_Query.Fields;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Reader is new ACPI_Passive_Members (Core => Core);
package ACPI_Passive_Status with SPARK_Mode is
   package Fields renames ACPI_Passive_Query.Fields;
   use type Fields.Field_Name;
   -- Read only the current device's direct STA. Missing is explicitly defaulted
   -- in the wire field; integer zero is observed. Unavailable/invalid/failed
   -- reads produce no field. Methods and Fields are not evaluated. No owned
   -- namespace handle or text span escapes this scalar projection.
   procedure Read
     (Service : in out Core.State; Device : Core.Node_Descriptor;
      Result : out Fields.Field_Result; Observation : out Reader.Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service) and then
         (if Result.Accepted then
           Fields.Valid (Result.Value, ACPI_Passive_Query.Node_Limit (Core.Max_Namespace_Nodes), 0)
           and then Result.Value.Name = Fields.Device_Status);
end ACPI_Passive_Status;
