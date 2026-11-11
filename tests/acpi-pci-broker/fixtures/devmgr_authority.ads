pragma Ada_2022;
with Interfaces;use Interfaces;with CuBit.Handles;with CuBit.ACPI_Memory_Classes;with Devmgr_Claim_Ledgers;with Devmgr_Heap;
package Devmgr_Authority is
 package Ledgers is new Devmgr_Claim_Ledgers(Devmgr_Heap.Allocate,Devmgr_Heap.Release);
 subtype Capacity_Report is Ledgers.Capacity_Report;
 type ACPI_Window is record Memory:CuBit.Handles.Device_Memory:=CuBit.Handles.No_Device_Memory;First,Bytes:Unsigned_64:=0;Kind:CuBit.ACPI_Memory_Classes.Memory_Kind:=CuBit.ACPI_Memory_Classes.PCI_Config;end record;
 Held_Root : ACPI_Window; Root_Count : Natural := 1;
 function PCI_Root_Count return Natural is (Root_Count);
 function PCI_Root_At(K:Positive) return ACPI_Window;
end;
