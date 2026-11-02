------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Table-named firmware data windows from devmgr (the ACPI firmware-data
--  grant, coordination/acpi.md "Table-named firmware data"; kernel
--  Physical_Ranges Data_Memory).
--
--  @description
--  A program the trusted startup plan starts with (acpi-firmware-data
--  approve-declared) on its ACPI hardware grant gets one more startup
--  entry: devmgr's endpoint badged with Authority_Tag, named
--  Data_Label (Binding). Over it, and only over it, Data_Window_Request asks
--  for the whole pages of one SystemMemory region a sealed table names at a
--  literal extent (the requester's own assertion; devmgr and the kernel
--  enforce only what they can know: the memory's class and who else holds
--  the pages). Synchronous calls go down: the requester calls devmgr, with
--  a finite deadline of its own; devmgr never calls back.
--
--  Request (a call accepting a capability), Request_Words words:
--    First_Page_Word  the envelope's first physical page
--    Page_Count_Word  its pages (at least one)
--    Request_Word     the requester's request id (never zero)
--  Reply, label Data_Window_Request, Reply_Words words:
--    Reply_Request_Word      the request id
--    Disposition_Word        Disposition'Enum_Rep
--    Reply_First_Page_Word   the envelope granted (zero when refused)
--    Reply_Page_Count_Word   its pages (zero when refused)
--  Granted_Data and Reused_NVS carry one Device_Memory capability
--  covering exactly the envelope, read-write without grant: the installed
--  grant reference is the carried handle (CuBit.Handles.IPC.Carried_By).
--  Granted_Data's is tagged Firmware_Data_ABI.Firmware_Data_Tag (mapped
--  uncached);
--  Reused_NVS's keeps the NVS root's own Firmware_NVS_Tag (write-back):
--  a page in an NVS root is never also given an uncached alias. The
--  requester inspects what it got (tag, bounds, rights) and never trusts
--  these words for authority. A granted envelope is ACPI's for the rest of
--  the boot: devmgr refuses any later device assignment that overlaps it.
--  A request id asked again (after a timeout) gets the original answer,
--  with a copy of the same capability: devmgr never derives a second grant
--  for it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Firmware_Data_Exchange with Pure, SPARK_Mode is

   --  Outside the ACPI pair's own 0x2A0 .. 0x2BF blocks, beside devmgr's
   --  ACPI window requests (CuBit.ACPI_Window_Exchange).
   Data_Window_Request : constant Unsigned_32 := 16#024D#;

   Request_Words   : constant := 3;
   First_Page_Word : constant := 0;
   Page_Count_Word : constant := 1;
   Request_Word    : constant := 2;

   Reply_Words           : constant := 4;
   Reply_Request_Word    : constant := 0;
   Disposition_Word      : constant := 1;
   Reply_First_Page_Word : constant := 2;
   Reply_Page_Count_Word : constant := 3;

   --  devmgr's endpoint badge for the grant. ASCII "FWDATA" and a version.
   Authority_Tag : constant Unsigned_64 := 16#4657_4441_5441_0001#;

   --  "hardware-data": the badged endpoint's startup name beside the
   --  hardware grant's windows (CuBit.ACPI_Hardware_Authority labels).
   Data_Infix : constant String := "-data";
   function Data_Label (Binding : String) return String is (Binding & Data_Infix)
   with Pre => Binding'Last <= Natural'Last - Data_Infix'Length;

   --  How devmgr answered.
   type Disposition is
     (Granted_Data,         --  a narrowed firmware-data capability, uncached
      Reused_NVS,           --  inside an NVS root: its own write-back tag
      Overlaps_Device,      --  a page is a PCI BAR's or a device's/driver's
      Not_Firmware_Class,   --  a page is in no root: RAM, kernel memory, tables
      No_Root,              --  not wholly inside one data root (or one NVS root)
      Capacity,             --  devmgr's records could not grow (heap)
      Overlaps_Control,     --  a page is the FACS page
      Overlaps_PCI_Config,  --  a page is PCI configuration space (ECAM)
      Not_Approved,         --  not through the firmware-data badge
      Malformed);           --  no pages, past the physical limit, or id zero
   for Disposition use
     (Granted_Data        => 1,
      Reused_NVS          => 2,
      Overlaps_Device     => 3,
      Not_Firmware_Class  => 4,
      No_Root             => 5,
      Capacity            => 6,
      Overlaps_Control    => 7,
      Overlaps_PCI_Config => 8,
      Not_Approved        => 9,
      Malformed           => 10);

   --  The disposition's name, for logs (the runtime keeps no
   --  enumeration images).
   function Name (D : Disposition) return String is
     (case D is
        when Granted_Data        => "GRANTED_DATA",
        when Reused_NVS          => "REUSED_NVS",
        when Overlaps_Device     => "OVERLAPS_DEVICE",
        when Not_Firmware_Class  => "NOT_FIRMWARE_CLASS",
        when No_Root             => "NO_ROOT",
        when Capacity            => "CAPACITY",
        when Overlaps_Control    => "OVERLAPS_CONTROL",
        when Overlaps_PCI_Config => "OVERLAPS_PCI_CONFIG",
        when Not_Approved        => "NOT_APPROVED",
        when Malformed           => "MALFORMED");

   function Granted (D : Disposition) return Boolean is
     (D in Granted_Data | Reused_NVS);

   --  The disposition a reply word names, if any.
   function Valid_Disposition (Word : Unsigned_64) return Boolean is
     (for some D in Disposition => Disposition'Enum_Rep (D) = Word);

   --  Physical pages below the kernel's limit (2 ** 47 bytes).
   Page_Bytes : constant := 4096;
   Page_Limit : constant := 2 ** 35;

   --  The request words name whole pages below the limit and an id.
   function Well_Formed (First_Page, Pages, Request : Unsigned_64) return Boolean is
     (Request /= 0 and then Pages >= 1 and then First_Page < Page_Limit
      and then Pages <= Page_Limit - First_Page);

end CuBit.Firmware_Data_Exchange;
