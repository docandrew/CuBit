pragma Ada_2022;
with ACPI_Service;
with ACPI_Firmware_Data_Frontend_Native;
with ACPI_Firmware_Data_Transport;
with ACPI_Firmware_Data_Startup;
with AML_Field_Layout;
with CuBit.Handles;
with CuBit.Handles.IPC;
-- Process-lifetime frontend assembly. It binds no endpoint and enables no
-- feature at elaboration; the startup owner supplies its trusted endpoint.
generic
   with function Close_Carried (Item : CuBit.Handles.IPC.Carried)
      return CuBit.Handles.IPC.Close_Result;
package ACPI_Native_Firmware_Data is
   package Native is new ACPI_Firmware_Data_Frontend_Native (Close_Carried);
   No_Carried : constant CuBit.Handles.IPC.Carried := (Kind => CuBit.Handles.IPC.Absent);
   package RPC is new ACPI_Firmware_Data_Transport
      (Native.Channel,CuBit.Handles.IPC.Carried,No_Carried,Native.Present,
       Native.Clock,Native.Exchange,Native.Dispose);
   function Original (Item : ACPI_Service.Namespace.Operation_Region_Attributes;
      Geometry : AML_Field_Layout.Register_Geometry) return Boolean;
   package Session is new ACPI_Firmware_Data_Startup
      (CuBit.Handles.Endpoint,CuBit.Handles.No_Endpoint,Native.Channel,RPC,
       Native.Initialize,Native.Clock,ACPI_Service.Namespace.Operation_Region_Attributes,
       Original,Allow_Writes => False);
end ACPI_Native_Firmware_Data;
