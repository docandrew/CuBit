pragma Ada_2022;
with ACPI_Service;
with AML_Field_Access;
with AML_Field_Layout;
with ACPI_Capacity_Diagnostics;
with ACPI_Inventory_Protocol;
with ACPI_Global_Lock_Protocol;
with CuBit.Handles;
-- Test boundary for the default-disabled native firmware-data path.
-- Every callback raises: enabling or accidentally invoking it fails the test.
-- Enabled-path behavior is exercised by the concrete hosted owner/session suite.
package ACPI_Native_Data is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package W renames ACPI_Inventory_Protocol.Wire;
   package Session is
      procedure Initialize (Destination : CuBit.Handles.Endpoint; Symbol : A.Endpoint_ID;
         Deadline : W.Time_Instant; Accepted : in out Boolean);
      procedure Inspect (Is_Ready, Is_Pending : in out Boolean;
         Observation : in out A.Observation; Capacity : in out ACPI_Capacity_Diagnostics.Report);
      procedure Describe (Target : A.Target; Geometry : in out L.Register_Geometry; Accepted : in out Boolean);
      procedure Authorize (Target : A.Target; For_Write : Boolean; Accepted : in out Boolean);
      procedure Dispatch (Item : A.Request; Reply : in out A.Response);
      procedure Resolve (Item : ACPI_Service.Namespace.Operation_Region_Attributes;
         Geometry : L.Register_Geometry; Target : in out A.Target; Accepted : in out Boolean);
      procedure Begin_Field (Requires_Lock, For_Write : Boolean; Deadline : W.Time_Instant;
         Ticket : ACPI_Global_Lock_Protocol.Ticket; Accepted : in out Boolean);
      procedure End_Field (Accepted : in out Boolean);
      procedure Cleanup (Accepted : in out Boolean);
   end Session;
end ACPI_Native_Data;
