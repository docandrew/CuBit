pragma Ada_2022;
with ACPI_Device_Enumeration;
generic
   -- Fixed native owner holds a read-only namespace-capture claim across
   -- batches: no other evaluator/catalog/binding mutation may run meanwhile.
   with function Metadata_Owner_Admitted return Boolean;
package ACPI_Bootstrap.Device_Enumeration is
   package Cursor is new ACPI_Device_Enumeration (ACPI_Service);
   type Admission is (Not_Ready, Owner_Busy, Admitted);
   subtype Run is Cursor.Session;
   procedure Start (Item : in out Run; Session : State; Budget : Natural; Allowed : out Admission)
      with Pre => Valid (Session);
   procedure Advance (Item : in out Run; Session : State; Max_Nodes : Positive;
      Device : out ACPI_Service.Node_Descriptor; Reads : out Natural;
      Result : out Cursor.Step_Result; Allowed : out Admission)
      with Pre => Valid (Session);
end ACPI_Bootstrap.Device_Enumeration;
