pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_PCI_Routes;
generic
   with package Core is new ACPI_Service_Core (<>);
package ACPI_PCI_Routing_Reader with SPARK_Mode is
   type Source_Kind is (Global_Pool, Link_Device);
   type Source (Kind : Source_Kind := Global_Pool) is record
      case Kind is
         when Global_Pool => Interrupt : ACPI_PCI_Routes.Global_Interrupt := 0;
         when Link_Device =>
            Device : Core.Node_Descriptor := Core.Values.No_Node;
            Index : ACPI_PCI_Routes.Source_Index := 0;
      end case;
   end record;
   type Route_Entry is record
      Device : ACPI_PCI_Routes.Device_Number := 0;
      Pin : ACPI_PCI_Routes.Interrupt_Pin := ACPI_PCI_Routes.INTA;
      Target : Source;
   end record;
   type Read_Status is (Parsed, Invalid_Table, Not_A_Package, Row_Out_Of_Bounds,
      Bad_Row_Length, Field_Access_Failed, Wrong_Field_Type, Bad_Address,
      Bad_Pin, Bad_Source, Bad_Source_Index, Cleanup_Failed);
   type Location is (Table_Object, Row_Object, Address_Field, Pin_Field,
      Source_Field, Source_Index_Field, Cleanup);
   subtype Pending_Slot is Positive range 1 .. 2;
   type Pending_Array is array (Pending_Slot) of Core.Values.Value_Handle;
   type Report is record
      Status : Read_Status := Invalid_Table;
      Where : Location := Table_Object;
      Access_Status : Core.Values.Access_Status := Core.Values.Invalid_Value;
      -- Normally empty. If an internal release fails, preserve the exact owned
      -- handles for caller cleanup instead of silently discarding their roots.
      Pending : Pending_Array := [others => Core.Values.No_Value];
   end record;
   -- Table is borrowed. Row and field pins are released on every normal/error
   -- path; cleanup failures return remaining owned handles in Report.Pending.
   -- Zero-based row index; no fixed table capacity or AML invocation. The owner
   -- serializes service access and retains Table throughout. Link descriptors
   -- must be revalidated before later _CRS/_SRS calls; parsing grants no rights.
   procedure Read_Entry (Service : in out Core.State;
      Table : Core.Values.Value_Handle; Index : Natural;
      Item : out Route_Entry; Result : out Report)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service);
end ACPI_PCI_Routing_Reader;
