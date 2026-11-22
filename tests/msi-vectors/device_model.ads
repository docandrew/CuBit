--  A PCI function with MSI-X (or MSI) and the kernel's delivery path, for
--  the hosted tests (main.adb). The device raises interrupts per entry; a
--  message reaches the kernel model only from an enabled, unmasked entry,
--  otherwise it is held pending (the PBA) and sent on unmask, as hardware
--  does. Writing an entry's address or data while it is unmasked is a
--  hazard (PCI 3.0 6.8.2.9: undefined), and is recorded.
with Interfaces; use Interfaces;
with Device_Vectors;
with PCI_Message_Interrupts; use PCI_Message_Interrupts;

package Device_Model is
   TABLE_ENTRIES : constant := 8;
   subtype Model_Entry is Entry_Index range 0 .. TABLE_ENTRIES - 1;

   --  The kernel's live table, as Device_Interrupts keeps it.
   Kernel : Device_Vectors.Vector_Table := Device_Vectors.EMPTY_TABLE;

   type Owner_Counts is array (Device_Vectors.Owner_Key range 0 .. 15) of Natural;
   Delivered : Owner_Counts := [others => 0];
   Stray     : Natural := 0;   --  messages to a vector with no bound owner
   Hazards   : Natural := 0;   --  address/data written while unmasked
   Wrong_Address : Natural := 0;  --  a message outside the APIC window

   procedure Reset_Device (MSIX : Boolean);

   --  Hardware access, as devmgr's executor performs it.
   procedure Write_Entry
     (Index : Entry_Index; Register : Entry_Register; Value : Unsigned_32);
   function Read_Config_16 (Offset : Natural) return Unsigned_16;
   procedure Write_Config_16 (Offset : Natural; Value : Unsigned_16);
   procedure Write_Config_32 (Offset : Natural; Value : Unsigned_32);

   --  The device signals entry Index (MSI: Index 0).
   procedure Raise_Interrupt (Index : Model_Entry);

   function Pending (Index : Model_Entry) return Boolean;
end Device_Model;
