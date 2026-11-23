pragma Ada_2022;
with CCL.Device_Sections;
with CCL.Resource_Sections;
with CuBit.Startup_Tables;
with ACPI_CMOS_Handover_Protocol;
package ACPI_CMOS_Manifest_Windows with Pure, SPARK_Mode is
   package ST renames CuBit.Startup_Tables;
   package RS renames CCL.Resource_Sections;
   package DS renames CCL.Device_Sections;
   package Wire renames ACPI_CMOS_Handover_Protocol;
   use type Wire.Profile;
   type Declaration is private;
   type Selection_Status is (Admitted, Not_Declared, Malformed_Declaration,
                             Not_Approved, Receipt_Unavailable, Recipient_Busy);
   -- This is a distinct named CMOS lane of the approved ACPI hardware grant.
   -- Never insert it in the ordinary FADT/SystemIO register-window list.
   type Window_Kind is (No_Window, System_CMOS_Window);
   type Selection is record
      Status : Selection_Status := Not_Declared;
      Kind : Window_Kind := No_Window;
      Binding : ST.Entry_Count := 0;
      Ports : Natural range 0 .. 4 := 0;
   end record;
   function Inspect
     (Declared : ST.Table; Resources : RS.Byte_Array; Devices : DS.Byte_Array)
      return Declaration
   with Pre => ST.Declaration (Declared);
   function Select_Window
     (Item : Declaration; Approved, Receipt_Known, Recipient_Available : Boolean;
      Receipt : Wire.Receipt) return Selection
   with Post => (if Select_Window'Result.Status = Admitted then
                    Approved and then Receipt_Known and then Recipient_Available
                    and then Receipt.Device /= Wire.Unidentified
                    and then Select_Window'Result.Kind = System_CMOS_Window
                    and then Select_Window'Result.Binding > 0
                    and then Select_Window'Result.Ports in 2 | 4
                 else Select_Window'Result.Kind = No_Window
                    and then Select_Window'Result.Binding = 0
                    and then Select_Window'Result.Ports = 0);
private
   type Declaration is record
      Status : Selection_Status := Not_Declared;
      Binding : ST.Entry_Count := 0;
      Maximum_Ports : Natural range 0 .. 4 := 0;
   end record;
end ACPI_CMOS_Manifest_Windows;
