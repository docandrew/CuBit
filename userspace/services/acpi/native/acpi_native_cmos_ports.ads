pragma Ada_2022;
with ACPI_CMOS_Transactions;
with CuBit.Handles;

-- One service-local port owner. Capture accepts only the launcher's copied
-- boot handover facts; Initialize adds the immutable validated FADT century.
package ACPI_Native_CMOS_Ports is
   package T renames ACPI_CMOS_Transactions;
   procedure Capture
     (Held : CuBit.Handles.IO_Ports; Profile : T.P.Profile;
      Known_NMI, NMI_Disabled : Boolean);
   procedure Initialize (Century : T.I.Byte; Accepted : out Boolean);
   function Available return Boolean;
   function Pending return Boolean;
   procedure Read_Configuration
     (Profile : out T.P.Profile; Century : out T.I.Byte;
      Known_NMI, NMI_Disabled, Standard_Granted, Extended_Granted : out Boolean);
   -- Called only by the retained local field owner after Check_Datum. The
   -- underlying adapter independently verifies range, NMI and protected bytes.
   procedure Transfer_Byte (Plan : T.Transfer; Value : out T.I.Byte; Outcome : out T.Completion);
end ACPI_Native_CMOS_Ports;
