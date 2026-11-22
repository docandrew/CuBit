pragma Ada_2022;
with ACPI_Service_Core;
generic
   with package Core is new ACPI_Service_Core (<>);
package ACPI_Region_Activation_Order with SPARK_Mode is
   -- Closest proper ancestor with an initializer considered by the device
   -- walker. Root is the fallback after its compatibility initializer step.
   -- No field access, AML, hardware readiness assertion or retained alias.
   procedure Required_Ancestor
     (Service : Core.State; Scope : Core.Namespace.Node_ID;
      Ancestor : out Core.Namespace.Node_ID; Accepted : out Boolean)
     with Pre => Core.Valid (Service);
end ACPI_Region_Activation_Order;
