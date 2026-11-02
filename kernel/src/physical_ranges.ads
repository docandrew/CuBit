pragma Ada_2022;
with Interfaces; use Interfaces;
with Device_Memory_Publication;

-------------------------------------------------------------------------------
-- Physical_Ranges (CAP-001)
--
-- Typed classification of the physical address space, built once at boot
-- from the firmware memory map (Multiboot/UEFI via the loader), the loader's
-- framebuffer and module descriptions, ACPI (local/I/O APIC, HPET) and the
-- kernel's own image. Each described range is a Span with a Range_Class.
-- A page's class is the MOST restrictive class of any span touching it, so
-- overlapping or sloppy firmware entries can only make a page less mappable.
-- Pages no span touches are Unreported: bus address space (the PCI holes and
-- everything above the top of described memory, where 64-bit BARs and ECAM
-- live).
--
-- Device-memory authority (CAP_DEVICE_MEM, MAP_DEVICE, MAP_INTO device
-- mappings, MAPFB) may only name Device_Mappable pages. The same fact --
-- "RAM vs device vs firmware-reserved" -- is what a per-device I/O page
-- table (IOMMU-001) needs: RAM classes are DMA targets only through explicit
-- DMA domains, Firmware_Reserved corresponds to RMRR-style identity ranges,
-- and Kernel_MMIO (the interrupt window) is never a DMA target.
--
-- Fail closed: a map that could not record every span (capacity) or that
-- has not been sealed authorizes nothing.
-------------------------------------------------------------------------------
package Physical_Ranges with SPARK_Mode, Pure is

   Page_Bytes    : constant := 4096;
   -- Exclusive physical limit; matches Virtmem.PhysAddress (47 bits).
   Address_Limit : constant := 2 ** 47;
   -- The legacy real-mode/BIOS window: IVT, BDA, EBDA, the AP trampoline,
   -- VGA and option ROMs. Kernel-owned; no device-memory user needs it.
   Low_Memory_Limit : constant := 16#10_0000#;

   subtype Address is Unsigned_64 range 0 .. Address_Limit - 1;
   subtype Page_Number is Unsigned_64 range 0 .. Address_Limit / Page_Bytes - 1;
   Last_Page : constant Page_Number := Page_Number'Last;

   -- Most restrictive first: a page touched by several spans takes the
   -- minimum class. Everything before Firmware_Reserved is memory the
   -- kernel manages or depends on and is never device-mappable:
   --   Kernel_Image (text/data/bss/stacks), Low_Memory (below
   --   Low_Memory_Limit), Boot_Module (loader modules, read-only
   --   shareable), Usable_RAM (firmware "available": buddy/boot/slab, page
   --   tables, process, owned and DMA memory), Defective_RAM, ACPI_NVS,
   --   ACPI_Tables (reclaimable, retained), Kernel_MMIO (local APIC/MSI
   --   window, I/O APICs, HPET); then Firmware_Reserved (ECAM, ROM, MMIO
   --   the firmware names), Firmware_Framebuffer, Unreported (bus space).
   -- The list is the published one (Device_Memory_Publication), so a
   -- class reaches logs and the Desktop bootstrap screen as its position.
   type Range_Class is new Device_Memory_Publication.Physical_Class;

   function Device_Mappable (Class : Range_Class) return Boolean is
     (Class >= Firmware_Reserved);

   function Page_Of (A : Address) return Page_Number is (A / Page_Bytes);

   -- Inclusive byte range.
   type Span is record
      First, Last : Address := 0;
      Class       : Range_Class := Unreported;
   end record;

   Max_Spans : constant := 1_200;
   subtype Span_Count is Natural range 0 .. Max_Spans;
   subtype Span_Index is Positive range 1 .. Max_Spans;
   type Span_Array is array (Span_Index) of Span;

   -- PCI configuration space: PCI segment 0, bus 0 through the MCFG's
   -- enhanced configuration mechanism (ECAM), one 4 KiB page per function,
   -- 32 devices of 8 functions: 1 MiB.
   PCI_Devices_Per_Bus      : constant := 32;
   PCI_Functions_Per_Device : constant := 8;
   PCI_Function_Bytes       : constant := Page_Bytes;
   PCI_Bus_Pages            : constant :=
     PCI_Devices_Per_Bus * PCI_Functions_Per_Device * PCI_Function_Bytes / Page_Bytes;
   PCI_Bus_Bytes            : constant := PCI_Bus_Pages * Page_Bytes;
   -- The highest page a bus slice may start at and still end below the
   -- physical limit.
   Last_PCI_Bus_Page        : constant Page_Number := Last_Page - (PCI_Bus_Pages - 1);

   -- The sealed ACPI tables' bytes (the kernel's admitted table catalog
   -- and its root table), inclusive. They do not change any page's class
   -- (SeaBIOS keeps its tables, and the FACS beside them, in a reserved
   -- span); they only keep table bytes out of firmware data (below). The
   -- catalog's representation bound, root table included (checked where
   -- both are seen, Physical_Range_Map).
   Max_Table_Extents : constant := 256;
   subtype Table_Extent_Count is Natural range 0 .. Max_Table_Extents;
   subtype Table_Extent_Index is Positive range 1 .. Max_Table_Extents;
   type Extent is record
      First, Last : Address := 0;
   end record;
   type Extent_Array is array (Table_Extent_Index) of Extent;

   -- Control_Page: the page holding the firmware's FACS (the ACPI Global
   -- Lock), when the FADT names one (Control_Known). It is the one page
   -- outside ACPI NVS that firmware-memory authority may name (below).
   -- PCI_Config_Page: the first page of segment 0 bus 0's ECAM slice, when
   -- the MCFG names one (PCI_Config_Known); the slice is PCI_Bus_Pages long.
   -- Tables: the sealed tables' extents; Tables_Complete is False once one
   -- could not be recorded (then no page is firmware data).
   type Map is record
      Spans    : Span_Array := [others => (First => 0, Last => 0, Class => Unreported)];
      Count    : Span_Count := 0;
      Complete : Boolean := True;  -- False once a span could not be recorded
      Sealed   : Boolean := False;
      Control_Known : Boolean := False;
      Control_Page  : Page_Number := 0;
      PCI_Config_Known : Boolean := False;
      PCI_Config_Page  : Page_Number := 0;
      Tables          : Extent_Array := [others => (First => 0, Last => 0)];
      Tables_Used     : Table_Extent_Count := 0;
      Tables_Complete : Boolean := True;
   end record;

   Empty_Map : constant Map :=
     (Spans    => [others => (First => 0, Last => 0, Class => Unreported)],
      Count    => 0,
      Complete => True,
      Sealed   => False,
      Control_Known => False,
      Control_Page  => 0,
      PCI_Config_Known => False,
      PCI_Config_Page  => 0,
      Tables          => [others => (First => 0, Last => 0)],
      Tables_Used     => 0,
      Tables_Complete => True);

   function Well_Formed (M : Map) return Boolean is
     ((for all I in 1 .. M.Count => M.Spans (I).First <= M.Spans (I).Last)
      and then (for all I in 1 .. M.Tables_Used => M.Tables (I).First <= M.Tables (I).Last));

   function Touches (S : Span; P : Page_Number) return Boolean is
     (Page_Of (S.First) <= P and then P <= Page_Of (S.Last));

   ---------------------------------------------------------------------------
   -- Building (boot only, before any user process exists)
   ---------------------------------------------------------------------------

   -- Records [First, Last] (inclusive) as Class. Bytes at or above
   -- Address_Limit are dropped (never mappable anyway); an empty or wholly
   -- out-of-range description is ignored. A span that does not fit marks
   -- the map incomplete, which then authorizes nothing.
   procedure Add (M : in out Map; First, Last : Unsigned_64; Class : Range_Class)
     with Pre  => not M.Sealed and then Well_Formed (M),
          Post => not M.Sealed and then Well_Formed (M)
                  and then M.Count >= M.Count'Old
                  and then (if not M.Complete'Old then not M.Complete)
                  and then (for all I in 1 .. M.Count'Old =>
                              M.Spans (I) = M.Spans'Old (I))
                  and then
                  (if First <= Last and then First < Address_Limit
                      and then M.Complete
                   then M.Count = M.Count'Old + 1
                        and then M.Spans (M.Count) =
                          (First => First,
                           Last  => Unsigned_64'Min (Last, Address_Limit - 1),
                           Class => Class));

   -- Records the sealed ACPI table bytes [First, Last] (inclusive). Bytes at
   -- or above Address_Limit are dropped; an empty or wholly out-of-range
   -- extent is ignored. One that does not fit leaves no page firmware data.
   procedure Add_Table (M : in out Map; First, Last : Unsigned_64)
     with Pre  => not M.Sealed and then Well_Formed (M),
          Post => not M.Sealed and then Well_Formed (M)
                  and then M.Count = M.Count'Old
                  and then M.Spans = M.Spans'Old
                  and then M.Complete = M.Complete'Old
                  and then M.Control_Known = M.Control_Known'Old
                  and then M.Control_Page = M.Control_Page'Old
                  and then M.PCI_Config_Known = M.PCI_Config_Known'Old
                  and then M.PCI_Config_Page = M.PCI_Config_Page'Old
                  and then M.Tables_Used >= M.Tables_Used'Old
                  and then (if not M.Tables_Complete'Old then not M.Tables_Complete);

   -- The sealed tables are not known: no page is firmware data.
   procedure Tables_Unknown (M : in out Map)
     with Pre  => not M.Sealed and then Well_Formed (M),
          Post => not M.Sealed and then Well_Formed (M)
                  and then M.Count = M.Count'Old
                  and then M.Spans = M.Spans'Old
                  and then M.Complete = M.Complete'Old
                  and then not M.Tables_Complete;

   -- A description could not be obtained: the map authorizes nothing.
   procedure Invalidate (M : in out Map)
     with Pre  => not M.Sealed and then Well_Formed (M),
          Post => not M.Sealed and then Well_Formed (M) and then not M.Complete;

   -- Records the FACS's physical address (the FADT's X_FIRMWARE_CTRL, or
   -- FIRMWARE_CTRL): its page becomes Control_Page. Addresses at or above
   -- Address_Limit are ignored (no control page).
   procedure Set_Control (M : in out Map; Physical : Unsigned_64)
     with Pre  => not M.Sealed and then Well_Formed (M),
          Post => not M.Sealed and then Well_Formed (M)
                  and then M.Count = M.Count'Old
                  and then M.Spans = M.Spans'Old
                  and then M.Complete = M.Complete'Old
                  and then M.Control_Known = (Physical < Address_Limit)
                  and then (if M.Control_Known
                            then M.Control_Page = Physical / Page_Bytes)
                  and then M.PCI_Config_Known = M.PCI_Config_Known'Old
                  and then M.PCI_Config_Page = M.PCI_Config_Page'Old;

   -- Records segment 0 bus 0's ECAM slice from the MCFG: Bus_Zero is the
   -- physical address of bus 0's function 0:0 (the base of an allocation
   -- for segment 0 that starts at bus 0). An unaligned base, or a slice
   -- that would reach Address_Limit, is ignored (no slice).
   procedure Set_PCI_Config (M : in out Map; Bus_Zero : Unsigned_64)
     with Pre  => not M.Sealed and then Well_Formed (M),
          Post => not M.Sealed and then Well_Formed (M)
                  and then M.Count = M.Count'Old
                  and then M.Spans = M.Spans'Old
                  and then M.Complete = M.Complete'Old
                  and then M.Control_Known = M.Control_Known'Old
                  and then M.Control_Page = M.Control_Page'Old
                  and then M.PCI_Config_Known =
                             (Bus_Zero mod Page_Bytes = 0
                              and then Bus_Zero <= Address_Limit - PCI_Bus_Bytes)
                  and then (if M.PCI_Config_Known
                            then M.PCI_Config_Page = Bus_Zero / Page_Bytes
                                 and then M.PCI_Config_Page <= Last_PCI_Bus_Page);

   procedure Seal (M : in out Map)
     with Pre  => Well_Formed (M),
          Post => M.Sealed and then Well_Formed (M)
                  and then M.Count = M.Count'Old
                  and then M.Spans = M.Spans'Old
                  and then M.Complete = M.Complete'Old
                  and then M.Control_Known = M.Control_Known'Old
                  and then M.Control_Page = M.Control_Page'Old
                  and then M.PCI_Config_Known = M.PCI_Config_Known'Old
                  and then M.PCI_Config_Page = M.PCI_Config_Page'Old
                  and then M.Tables = M.Tables'Old
                  and then M.Tables_Used = M.Tables_Used'Old
                  and then M.Tables_Complete = M.Tables_Complete'Old;

   ---------------------------------------------------------------------------
   -- Lookups
   ---------------------------------------------------------------------------

   -- The class of physical page P: the minimum class of every span touching
   -- it, Unreported when none does.
   function Class_Of (M : Map; P : Page_Number) return Range_Class
     with Pre  => Well_Formed (M),
          Post =>
            (for all I in 1 .. M.Count =>
               (if Touches (M.Spans (I), P)
                then Class_Of'Result <= M.Spans (I).Class))
            and then
            (Class_Of'Result = Unreported
             or else (for some I in 1 .. M.Count =>
                        Touches (M.Spans (I), P)
                        and then M.Spans (I).Class = Class_Of'Result));

   -- No span of a non-mappable class touches any page of [First, Last].
   function Blocker_Free (M : Map; First, Last : Address) return Boolean is
     (for all I in 1 .. M.Count =>
        Device_Mappable (M.Spans (I).Class)
        or else Page_Of (M.Spans (I).Last) < Page_Of (First)
        or else Page_Of (M.Spans (I).First) > Page_Of (Last));

   -- [First, Last] (inclusive bytes) lies entirely in device-mappable pages
   -- of a sealed, complete map.
   function Device_Range (M : Map; First, Last : Address) return Boolean is
     (M.Sealed and then M.Complete and then First <= Last
      and then Blocker_Free (M, First, Last));

   -- The same question for a base and byte count, rejecting empty,
   -- wrapping and out-of-range requests.
   function Device_Bytes (M : Map; Base, Bytes : Unsigned_64) return Boolean is
     (Bytes > 0 and then Base < Address_Limit
      and then Bytes - 1 <= Address_Limit - 1 - Base
      and then Device_Range (M, Base, Base + (Bytes - 1)));

   -- RAM is never device-mappable: every page of a Device_Range has a
   -- device-mappable class (so no Usable_RAM, kernel image, boot module,
   -- ACPI, defective or kernel-MMIO page).
   procedure Lemma_Device_Range_Excludes_RAM (M : Map; First, Last : Address)
     with Ghost, Global => null,
          Pre  => Well_Formed (M) and then Device_Range (M, First, Last),
          Post => (for all P in Page_Of (First) .. Page_Of (Last) =>
                     Device_Mappable (Class_Of (M, P)));

   -- The lowest page of [First, Last] that a non-mappable span touches,
   -- for reports. Found = False exactly when the range is Blocker_Free.
   procedure First_Blocked_Page
     (M : Map; First, Last : Address; Found : out Boolean; Page : out Page_Number)
     with Pre  => Well_Formed (M) and then First <= Last,
          Post => (if Found then
                     Page in Page_Of (First) .. Page_Of (Last)
                     and then not Device_Mappable (Class_Of (M, Page))
                   else Blocker_Free (M, First, Last));

   ---------------------------------------------------------------------------
   -- Device runs (devmgr's boot root authority)
   ---------------------------------------------------------------------------

   -- The first maximal run of device-mappable pages starting at or after
   -- page From, if any. Whatever the search finds is re-checked, so a found
   -- run is always a Device_Range, page aligned, and at or after From.
   procedure Next_Device_Run
     (M : Map; From : Page_Number;
      Found : out Boolean; First, Last : out Address)
     with Pre  => Well_Formed (M),
          Post => (if Found then
                     Device_Range (M, First, Last)
                     and then First mod Page_Bytes = 0
                     and then Last mod Page_Bytes = Page_Bytes - 1
                     and then Page_Of (First) >= From);

   type Run is record
      First, Last : Address := 0;
   end record;
   Max_Roots : constant := 8;
   subtype Root_Count is Natural range 0 .. Max_Roots;
   subtype Root_Index is Positive range 1 .. Max_Roots;
   type Run_Array is array (Root_Index) of Run;

   -- Up to Max_Roots device runs, largest first. Runs beyond the largest
   -- Max_Roots are left out (typically small reserved islands between RAM
   -- fragments, where no BAR lives). Every reported run is a Device_Range.
   procedure Largest_Device_Runs
     (M : Map; Runs : out Run_Array; Count : out Root_Count)
     with Pre  => Well_Formed (M),
          Post => (for all I in 1 .. Count =>
                     Device_Range (M, Runs (I).First, Runs (I).Last));

   ---------------------------------------------------------------------------
   -- Firmware memory (the ACPI hardware grant, slice 2)
   --
   -- Separate from device memory: pages the firmware keeps for itself that
   -- AML SystemMemory OperationRegions and the FACS Global Lock name. Only
   -- a firmware-memory capability (Device_Memory_Authority) may map them,
   -- never a device-memory one, and only whole pages:
   --   * an NVS page lies wholly inside one ACPI_NVS span and no span of
   --     any other class touches it (so never RAM, kernel memory, a boot
   --     module, ACPI tables or kernel MMIO, and never a page a byte range
   --     was rounded out into);
   --   * the control page (the FACS) lies wholly inside one ACPI_NVS or
   --     Firmware_Reserved span and no span of any other class touches it.
   --   * a PCI configuration page lies in segment 0 bus 0's ECAM slice (the
   --     MCFG's, PCI_Config_Page) and no span of a non-device-mappable class
   --     touches it (ECAM is firmware-reserved or bus space, never RAM).
   --   * a firmware-data page (table-named firmware data: a SystemMemory
   --     region a sealed table names at a literal extent) lies wholly inside
   --     an ACPI NVS or firmware-reserved span, no span of any other class
   --     touches it (Data_Page_Admitted), and it is no other kind's page:
   --     never an NVS page, the FACS page, the PCI slice, nor a page a
   --     sealed table touches. Which data page a program gets is devmgr's
   --     decision (it also keeps device and driver claims out); the kernel
   --     only bounds it by class.
   -- Firmware-reserved spans are also device memory, through devmgr, whose
   -- ledger keeps a page either a device's or ACPI's. Each kind is its own
   -- authority (an NVS capability never maps the control page, nor the
   -- reverse). NVS and the control page are mapped write-back: NVS is RAM,
   -- and the Global Lock is a locked compare-exchange that must see the
   -- firmware's own cached view. PCI configuration is device registers:
   -- mapped uncached, and only read-only (Device_Memory_Authority), so the
   -- AML interpreter reads PCI_Config fields without IPC while the legacy
   -- configuration ports and every configuration write stay devmgr's.
   -- Firmware data is mapped uncached and may be written: a reserved span
   -- can hold firmware RAM or chipset MMIO and the map cannot tell which;
   -- uncached is right for both. Since no data page is another kind's,
   -- every alias of a page has one cache mode.
   ---------------------------------------------------------------------------

   -- Page P is wholly inside span S.
   function Within (S : Span; P : Page_Number) return Boolean is
     (S.First <= P * Page_Bytes and then P * Page_Bytes + (Page_Bytes - 1) <= S.Last);

   function Control_Class (Class : Range_Class) return Boolean is
     (Class = ACPI_NVS or else Class = Firmware_Reserved);

   function NVS_Page (M : Map; P : Page_Number) return Boolean is
     ((for all I in 1 .. M.Count =>
         M.Spans (I).Class = ACPI_NVS or else not Touches (M.Spans (I), P))
      and then (for some I in 1 .. M.Count =>
                  M.Spans (I).Class = ACPI_NVS and then Within (M.Spans (I), P)));

   function Control_Page_Admitted (M : Map; P : Page_Number) return Boolean is
     (M.Control_Known and then P = M.Control_Page
      and then (for all I in 1 .. M.Count =>
                  Control_Class (M.Spans (I).Class) or else not Touches (M.Spans (I), P))
      and then (for some I in 1 .. M.Count =>
                  Control_Class (M.Spans (I).Class) and then Within (M.Spans (I), P)));

   -- Page P is in the map's segment 0 bus 0 ECAM slice.
   function In_PCI_Config (M : Map; P : Page_Number) return Boolean is
     (M.PCI_Config_Known and then M.PCI_Config_Page <= Last_PCI_Bus_Page
      and then P >= M.PCI_Config_Page and then P - M.PCI_Config_Page < PCI_Bus_Pages);

   function PCI_Config_Page_Admitted (M : Map; P : Page_Number) return Boolean is
     (In_PCI_Config (M, P)
      and then (for all I in 1 .. M.Count =>
                  Device_Mappable (M.Spans (I).Class) or else not Touches (M.Spans (I), P)));

   -- Page P holds bytes of a sealed ACPI table.
   function Table_Page (M : Map; P : Page_Number) return Boolean is
     (for some I in 1 .. M.Tables_Used =>
        Page_Of (M.Tables (I).First) <= P and then P <= Page_Of (M.Tables (I).Last));

   -- Every span touching P is ACPI NVS or firmware-reserved, and one of
   -- them holds it whole: the control page's test without the FACS.
   function Data_Page_Admitted (M : Map; P : Page_Number) return Boolean is
     ((for all I in 1 .. M.Count =>
         Control_Class (M.Spans (I).Class) or else not Touches (M.Spans (I), P))
      and then (for some I in 1 .. M.Count =>
                  Control_Class (M.Spans (I).Class) and then Within (M.Spans (I), P)));

   -- A firmware-data page: admitted, no other kind's, no table's.
   function Data_Page (M : Map; P : Page_Number) return Boolean is
     (M.Tables_Complete
      and then Data_Page_Admitted (M, P)
      and then not NVS_Page (M, P)
      and then not (M.Control_Known and then P = M.Control_Page)
      and then not In_PCI_Config (M, P)
      and then not Table_Page (M, P));

   -- The kinds of firmware memory, each its own authority (the kernel tags
   -- each root with its kind, Device_Memory_Authority): NVS runs, the FACS
   -- page, PCI configuration (segment 0 bus 0) and table-named firmware data.
   type Firmware_Kind is (NVS_Memory, Control_Memory, PCI_Config_Memory, Data_Memory);

   function Firmware_Page (M : Map; P : Page_Number; Kind : Firmware_Kind) return Boolean is
     ((Kind = NVS_Memory and then NVS_Page (M, P))
      or else (Kind = Control_Memory and then Control_Page_Admitted (M, P))
      or else (Kind = PCI_Config_Memory and then PCI_Config_Page_Admitted (M, P))
      or else (Kind = Data_Memory and then Data_Page (M, P)));

   -- A firmware-data page is no other kind's page.
   procedure Lemma_Data_Page_Alone (M : Map; P : Page_Number)
     with Ghost, Global => null,
          Pre  => Well_Formed (M) and then Data_Page (M, P),
          Post => (for all K in Firmware_Kind =>
                     (if K /= Data_Memory then not Firmware_Page (M, P, K)));

   -- The classes a firmware page of Kind may have: ACPI NVS or
   -- firmware-reserved for NVS, the control page and firmware data,
   -- device-mappable for PCI configuration. Never RAM, kernel memory, a
   -- boot module, ACPI tables or kernel MMIO.
   function Firmware_Class (Kind : Firmware_Kind; Class : Range_Class) return Boolean is
     (if Kind = PCI_Config_Memory then Device_Mappable (Class) else Control_Class (Class));

   -- [First, Last] (inclusive bytes) is whole pages of firmware memory of
   -- Kind in a sealed, complete map.
   function Firmware_Range
     (M : Map; First, Last : Address; Kind : Firmware_Kind) return Boolean
   is (M.Sealed and then M.Complete and then First <= Last
       and then First mod Page_Bytes = 0
       and then Last mod Page_Bytes = Page_Bytes - 1
       and then (for all P in Page_Of (First) .. Page_Of (Last) =>
                   Firmware_Page (M, P, Kind)));

   -- The same for a base and byte count, rejecting empty, wrapping and
   -- out-of-range requests.
   function Firmware_Bytes
     (M : Map; Base, Bytes : Unsigned_64; Kind : Firmware_Kind) return Boolean
   is (Bytes > 0 and then Base < Address_Limit
       and then Bytes - 1 <= Address_Limit - 1 - Base
       and then Firmware_Range (M, Base, Base + (Bytes - 1), Kind));

   -- A firmware page has its kind's Firmware_Class: never RAM, kernel
   -- memory, a boot module, ACPI tables or kernel MMIO.
   procedure Lemma_Firmware_Range_Excludes_RAM
     (M : Map; First, Last : Address; Kind : Firmware_Kind)
     with Ghost, Global => null,
          Pre  => Well_Formed (M) and then Firmware_Range (M, First, Last, Kind),
          Post => (for all P in Page_Of (First) .. Page_Of (Last) =>
                     Firmware_Class (Kind, Class_Of (M, P)));

   -- The firmware-memory roots, each with its kind, as many as the map
   -- holds (devmgr's launch configuration carries them all, LIM-001b),
   -- enumerated one at a time by Next_Firmware_Run from
   -- First_Firmware_Cursor:
   --   * first, in ascending order: maximal runs of NVS pages (adjacent and
   --     overlapping NVS spans coalesced; a page another class touches
   --     ends a run, and a partial page is never in one), the control page
   --     as a run of its own when it is admitted and in no NVS root, and the
   --     PCI configuration slice (exactly PCI_Bus_Pages from
   --     PCI_Config_Page) when the MCFG named one. A run longer than
   --     Max_Firmware_Run_Pages, or (the PCI slice) not wholly admitted, is
   --     left out whole (Left_Out counts it), never shortened;
   --   * then, in ascending order, runs of firmware-data pages, each at
   --     most Max_Firmware_Run_Pages long (a longer stretch is several
   --     runs: devmgr only ever narrows them).
   Max_Firmware_Run_Pages : constant := 2 ** 16;   -- 256 MiB
   type Firmware_Run is record
      First, Last : Address := 0;
      Kind        : Firmware_Kind := NVS_Memory;
   end record;
   No_Firmware_Run : constant Firmware_Run := (First => 0, Last => 0, Kind => NVS_Memory);

   type Firmware_Phase is (Admitted_Runs, Data_Runs, Finished);
   type Firmware_Cursor is record
      Phase           : Firmware_Phase := Finished;
      From            : Page_Number := 0;   -- the next run starts here or above
      Control_Pending : Boolean := False;
      PCI_Pending     : Boolean := False;
   end record;

   -- Where the enumeration starts: nothing for a map that is not sealed and
   -- complete.
   function First_Firmware_Cursor (M : Map) return Firmware_Cursor
     with Pre  => Well_Formed (M),
          Post => (if not (M.Sealed and then M.Complete)
                   then First_Firmware_Cursor'Result.Phase = Finished);

   -- The next root, if any (Found False: the enumeration is over). Every
   -- root is whole pages of its kind; within each phase the roots ascend,
   -- and every NVS, control and PCI root comes before any data root.
   procedure Next_Firmware_Run
     (M : Map; Cursor : in out Firmware_Cursor;
      Found : out Boolean; Run : out Firmware_Run; Left_Out : in out Natural)
     with Pre  => Well_Formed (M),
          Post => Cursor.Phase >= Cursor'Old.Phase
                  and then (if not Found then Cursor.Phase = Finished)
                  and then
                  (if Found then
                     Firmware_Range (M, Run.First, Run.Last, Run.Kind)
                     and then (if Run.Kind /= Data_Memory then
                                 Cursor'Old.Phase = Admitted_Runs
                                 and then Page_Of (Run.First) >= Cursor'Old.From
                               elsif Cursor'Old.Phase = Data_Runs then
                                 Page_Of (Run.First) >= Cursor'Old.From)
                     and then (if Cursor.Phase = Admitted_Runs then
                                 Run.Kind /= Data_Memory
                                 and then Cursor.From > Page_Of (Run.Last))
                     and then (if Cursor.Phase = Data_Runs and then Run.Kind = Data_Memory
                               then Cursor.From > Page_Of (Run.Last)));

   ---------------------------------------------------------------------------
   -- Firmware-data runs: uniform stretches
   --
   -- Whether page P is a data page depends only on which spans and table
   -- extents touch P or hold it whole, and whether P is the control page or
   -- in the PCI slice. Each of these changes only at a few pages (a span's
   -- first and past-last touched page, its first and past-last whole page;
   -- an extent's first and past-last page; the control page and the one
   -- after it; the slice's first page and the one past it). Between two
   -- such pages nothing changes (Uniform), so one page decides a whole
   -- stretch (Lemma_Uniform_Data), and runs are found and verified a
   -- stretch at a time rather than page by page.
   ---------------------------------------------------------------------------

   -- C changes nothing inside [A, B]: it is at or below A, or past B.
   function Steady (C : Unsigned_64; A, B : Page_Number) return Boolean is
     (C <= A or else C > B);

   -- The first page wholly at or after X, and the first page not wholly at
   -- or before X.
   function Ceil_Page (X : Address) return Unsigned_64 is ((X + (Page_Bytes - 1)) / Page_Bytes);
   function Whole_End (X : Address) return Unsigned_64 is ((X + 1) / Page_Bytes);

   function Span_Steady (S : Span; A, B : Page_Number) return Boolean is
     (Steady (Page_Of (S.First), A, B) and then Steady (Page_Of (S.Last) + 1, A, B)
      and then Steady (Ceil_Page (S.First), A, B) and then Steady (Whole_End (S.Last), A, B));

   function Extent_Steady (E : Extent; A, B : Page_Number) return Boolean is
     (Steady (Page_Of (E.First), A, B) and then Steady (Page_Of (E.Last) + 1, A, B));

   function Uniform (M : Map; A, B : Page_Number) return Boolean is
     ((for all I in 1 .. M.Count => Span_Steady (M.Spans (I), A, B))
      and then (for all I in 1 .. M.Tables_Used => Extent_Steady (M.Tables (I), A, B))
      and then Steady (M.Control_Page, A, B) and then Steady (M.Control_Page + 1, A, B)
      and then Steady (M.PCI_Config_Page, A, B)
      and then Steady (M.PCI_Config_Page + PCI_Bus_Pages, A, B));

   -- Every page of a uniform stretch is a data page exactly when its first
   -- page is.
   procedure Lemma_Uniform_Data (M : Map; A, B, P : Page_Number)
     with Ghost, Global => null,
          Pre  => Well_Formed (M) and then A <= P and then P <= B and then Uniform (M, A, B),
          Post => Data_Page (M, P) = Data_Page (M, A);

end Physical_Ranges;
