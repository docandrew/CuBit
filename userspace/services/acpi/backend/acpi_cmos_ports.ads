pragma Ada_2022;
with Interfaces;
with ACPI_CMOS_Transactions;
with ACPI_Backend_Binding_Types;

generic
   type Port_Handle is private;
   No_Ports : Port_Handle;
   with procedure Inspect (Held : Port_Handle; Item : out ACPI_Backend_Binding_Types.Description);
   with procedure Read_Byte
     (Held : Port_Handle; Port : Interfaces.Unsigned_16;
      Value : out ACPI_CMOS_Transactions.I.Word;
      Outcome : out ACPI_CMOS_Transactions.Completion);
   with procedure Write_Byte
     (Held : Port_Handle; Port : Interfaces.Unsigned_16;
      Value : ACPI_CMOS_Transactions.I.Byte;
      Outcome : out ACPI_CMOS_Transactions.Completion);
-- This is the only select/data implementation. Native actuals are direct
-- capability syscalls; hosted actuals record the identical operation order.
-- No clock, AML, IPC, selector readback or retry occurs between the pair.
package ACPI_CMOS_Ports is
   package T renames ACPI_CMOS_Transactions;
   package P renames T.P;
   package I renames T.I;
   type Context is limited private;
   procedure Initialize
     (IO : in out Context; Held : Port_Handle; Profile : P.Profile;
      Century : I.Byte; Known_NMI, NMI_Disabled : Boolean; Accepted : out Boolean);
   function Is_Ready (IO : Context) return Boolean;
   function Pending (IO : Context) return Boolean;
   procedure Configuration
     (IO : Context; Profile : out P.Profile; Century : out I.Byte;
      Known_NMI, NMI_Disabled, Standard_Granted, Extended_Granted : out Boolean);
   procedure Transfer_Byte
     (IO : in out Context; Plan : T.Transfer; Value : out I.Byte;
      Outcome : out T.Completion);
private
   type Context is limited record
      Attempted, Configured, Working, Faulted : Boolean := False;
      Held : Port_Handle := No_Ports;
      Device : P.Profile;
      Century : I.Byte := 0;
      NMI_Disabled : Boolean := False;
   end record;
end ACPI_CMOS_Ports;
