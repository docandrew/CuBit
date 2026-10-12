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

   type Map is record
      Spans    : Span_Array := [others => (First => 0, Last => 0, Class => Unreported)];
      Count    : Span_Count := 0;
      Complete : Boolean := True;  -- False once a span could not be recorded
      Sealed   : Boolean := False;
   end record;

   Empty_Map : constant Map :=
     (Spans    => [others => (First => 0, Last => 0, Class => Unreported)],
      Count    => 0,
      Complete => True,
      Sealed   => False);

   function Well_Formed (M : Map) return Boolean is
     (for all I in 1 .. M.Count => M.Spans (I).First <= M.Spans (I).Last);

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

   -- A description could not be obtained: the map authorizes nothing.
   procedure Invalidate (M : in out Map)
     with Pre  => not M.Sealed and then Well_Formed (M),
          Post => not M.Sealed and then Well_Formed (M) and then not M.Complete;

   procedure Seal (M : in out Map)
     with Pre  => Well_Formed (M),
          Post => M.Sealed and then Well_Formed (M)
                  and then M.Count = M.Count'Old
                  and then M.Spans = M.Spans'Old
                  and then M.Complete = M.Complete'Old;

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

end Physical_Ranges;
