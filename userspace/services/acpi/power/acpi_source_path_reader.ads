pragma Ada_2022;
with ACPI_Service_Core;with ACPI_Source_Paths;
generic
   with package Core is new ACPI_Service_Core(<>);
package ACPI_Source_Path_Reader is
   -- Read-only metadata traversal of a locally held device descriptor. Never
   -- accepts a wire node/path, executes AML, returns a prefix or grants access.
   procedure Read(Service:Core.State;Device:Core.Node_Descriptor;Value:out ACPI_Source_Paths.Path)
     with Pre=>Core.Valid(Service);
end ACPI_Source_Path_Reader;
