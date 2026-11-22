with Firmware_Tables;
with Firmware_Tables.Catalog;
with Firmware_Tables.Snapshots;
with Virtmem;
package ACPI is
   MAX_IOAPICS : constant := 16;
   lapicAddr, hpetAddr, facsAddr : Virtmem.PhysAddress := 0;
   type IOAPIC_Addresses is array (1 .. MAX_IOAPICS) of Virtmem.PhysAddress;
   ioapicAddrs : IOAPIC_Addresses := [others => 0];
   ioapicOverflow : Boolean := False;
   type Table_Copy_Status is
     (Copied, No_Table, Buffer_Too_Small, Snapshot_Unavailable);
   Snapshot_Mode : Firmware_Tables.Snapshots.Phase := Firmware_Tables.Snapshots.Ready;
   Descriptors : Natural range 0 .. 2 := 1;
   Metadata_Missing, Copy_Failure, Corrupt_Data : Boolean := False;
   First_Name : Firmware_Tables.Signature := "MCFG";
   Advertised_Bytes : Positive := 60;
   Copy_Calls, Describe_Calls, Copy_First, Copy_Length, Copy_Index : Natural := 0;
   procedure Configure (Allocations : Natural);
   function Table_Snapshot_State return Firmware_Tables.Snapshots.Phase is (Snapshot_Mode);
   function Snapshot_Table_Count return Natural is (Descriptors);
   function Describe_Snapshot_Table (Index : Natural)
     return Firmware_Tables.Snapshots.Metadata_Result;
   procedure Copy_Table (Index : Positive; Destination : out Firmware_Tables.Bytes;
                         Result : out Table_Copy_Status);
   function Table_Inventory_State return Firmware_Tables.Catalog.Phase is
     (Firmware_Tables.Catalog.Ready);
   function Table_Count return Natural is (0);
   function Table_Source (Index : Positive) return Firmware_Tables.Catalog.Descriptor;
end ACPI;
