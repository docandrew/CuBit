pragma Ada_2022;
with AML_Decode;
package body ACPI_Native_Firmware_Data is
   use type AML_Decode.Integer_Value, ACPI_Service.Namespace.Region_Space_ID;
   function Original (Item : ACPI_Service.Namespace.Operation_Region_Attributes;
      Geometry : AML_Field_Layout.Register_Geometry) return Boolean is
      (Item.Origin.Known and then ACPI_Service.Namespace.Has_Definition (Item.Origin.Definition)
       and then Item.Syntax.Address_Is_Literal and then Item.Syntax.Length_Is_Literal
       and then Item.Space = ACPI_Service.Namespace.Region_Space_ID (AML_Field_Layout.Memory_Space)
       and then Item.Address = Geometry.Region.Base and then Item.Length = Geometry.Region.Length);
end ACPI_Native_Firmware_Data;
