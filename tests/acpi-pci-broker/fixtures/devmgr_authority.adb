pragma Ada_2022;
package body Devmgr_Authority is function PCI_Root_At(K:Positive) return ACPI_Window is pragma Unreferenced(K);begin return Held_Root;end;end;
