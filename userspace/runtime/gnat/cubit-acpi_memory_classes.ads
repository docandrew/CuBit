------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What an ACPI firmware-memory window is, and how the kernel maps it, as
--  the window's own kernel capability says (the ACPI hardware grant,
--  coordination/acpi-region-boundary.md, slice 2).
--
--  @description
--  The kernel stamps each firmware-memory root with a tag when it mints it
--  at boot (Capability_ABI.Firmware_NVS_Tag, Firmware_Control_Tag,
--  Firmware_PCI_Config_Tag), and no derivation can change a device-memory
--  tag. A program reads the tag of
--  the handle it holds (INSPECT_CAPABILITY), never a launcher's word, so a
--  forged or mismatched startup entry cannot claim another kind of memory
--  or another cache mode: an unknown tag is no firmware window at all.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Capability_ABI;

package CuBit.ACPI_Memory_Classes with Pure, SPARK_Mode is

   --  A run of whole ACPI NVS pages, the FACS page alone (the Global
   --  Lock), whatever the firmware map calls that page, or PCI segment 0
   --  bus 0's configuration space through the MCFG's ECAM (ACPI-PCI-001:
   --  256 functions of PCI_Function_Bytes, function F of device D at page
   --  D * PCI_Functions_Per_Device + F).
   type Memory_Kind is (ACPI_NVS, Firmware_Control, PCI_Config);

   --  How the kernel maps the pages.
   type Cache_Mode is (Write_Back, Uncached);

   function Firmware_Tag (Tag : Unsigned_64) return Boolean is
     (Tag = Capability_ABI.Firmware_NVS_Tag or else Tag = Capability_ABI.Firmware_Control_Tag
      or else Tag = Capability_ABI.Firmware_PCI_Config_Tag);

   function Kind_Of (Tag : Unsigned_64) return Memory_Kind is
     (if Tag = Capability_ABI.Firmware_Control_Tag then Firmware_Control
      elsif Tag = Capability_ABI.Firmware_PCI_Config_Tag then PCI_Config
      else ACPI_NVS)
   with Pre => Firmware_Tag (Tag);

   --  The kernel maps NVS and the FACS page write-back (kernel
   --  Physical_Ranges, "Firmware memory"): NVS is RAM, and the Global
   --  Lock's locked compare-exchange must see the firmware's cached view.
   --  PCI configuration is device registers: uncached.
   Cache_Of : constant array (Memory_Kind) of Cache_Mode :=
     [PCI_Config => Uncached, others => Write_Back];

   --  Whether a window of the kind is granted (and mapped) writable: NVS
   --  and the FACS page are; PCI configuration never is (its kernel root
   --  has no write right, and the kernel maps it only read-only; writes
   --  stay devmgr's).
   Writable_Of : constant array (Memory_Kind) of Boolean :=
     [PCI_Config => False, others => True];

   --  PCI segment 0 bus 0's ECAM slice (kernel Physical_Ranges).
   PCI_Devices_Per_Bus      : constant := 32;
   PCI_Functions_Per_Device : constant := 8;
   PCI_Function_Bytes       : constant := 4096;
   PCI_Bus_Bytes            : constant :=
     PCI_Devices_Per_Bus * PCI_Functions_Per_Device * PCI_Function_Bytes;

   --  Physical pages below the kernel's limit (2 ** 47 bytes).
   Page_Bytes : constant := 4096;
   Page_Limit : constant := 2 ** 35;

   --  A capability the kernel describes as Tag over [First, First + Bytes)
   --  is a firmware-memory window: a firmware tag, whole pages, nonempty,
   --  below the limit. Anything else (device memory, an unknown tag, a
   --  partial page) is refused, whatever a startup entry calls it.
   function Admits (Tag, First, Bytes : Unsigned_64) return Boolean is
     (Firmware_Tag (Tag)
      and then First mod Page_Bytes = 0 and then Bytes mod Page_Bytes = 0
      and then First / Page_Bytes < Page_Limit
      and then Bytes / Page_Bytes in 1 .. Page_Limit - First / Page_Bytes);

end CuBit.ACPI_Memory_Classes;
