pragma Ada_2022;
with CuBit.Handles;
with ACPI_Region_Protocol;
with ACPI_Register_Transport;
with ACPI_Field_Remote_Provider;
with AML_Field_Access;
generic
   with procedure Read_Window
     (Now, Deadline : out ACPI_Region_Protocol.Time_Instant);
   Maximum_Registers : Positive := AML_Field_Access.Default_Maximum_Registers;
   Maximum_Confirmations : Positive := AML_Field_Access.Default_Maximum_Confirmations;
package ACPI_Field_Native_Provider is
   package Provider is new ACPI_Field_Remote_Provider
     (ACPI_Register_Transport.State, CuBit.Handles.Endpoint, CuBit.Handles.No_Endpoint,
      ACPI_Register_Transport.Exchange, Read_Window, Maximum_Registers, Maximum_Confirmations);
end ACPI_Field_Native_Provider;
