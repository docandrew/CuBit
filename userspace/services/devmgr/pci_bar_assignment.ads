pragma Ada_2022;
with Interfaces; use Interfaces;
with PCI_BAR_Placement;
------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The register words that give an unassigned PCI memory BAR an address
--  (PCI-BAR-001), from its words as read, its size, the host bridge's
--  memory windows and the extents already taken.
--
--  @description
--  PCI_BAR_Placement chooses the pages; this package turns that choice
--  into what devmgr writes, and nothing else:
--  - The BAR keeps its type bits: I/O 0, the 32- or 64-bit locator, and the
--    prefetchable flag (PCI Local Bus 3.0, 6.2.5.1). The address bits hold
--    the placed span's first byte.
--  - A 32-bit BAR is placed below 4 GiB: its windows are clipped to the
--    first 4 GiB before the search, and its high word is 0.
--  - No BAR is placed below 1 MiB, the legacy VGA/ISA ranges, whatever a
--    window says: every window is clipped to start at Lowest_Placement_Page.
--    Linux does the same on x86 (arch/x86/pci/i386.c,
--    pcibios_align_resource: "The low 1MB range is reserved for ISA cards",
--    a memory start below BIOS_END is raised to BIOS_END; the bus
--    allocator's floor PCIBIOS_MIN_MEM is pci_mem_start, the start of the
--    largest gap in the e820 map below 4 GiB, arch/x86/kernel/e820.c
--    e820__setup_pci_gap). devmgr's device-memory runs play the part of
--    that gap: only memory the kernel granted devmgr is Allowed.
--  - The place lies inside one of the Allowed runs (devmgr's device memory)
--    as well as inside a window: illegal places are never chosen, so the
--    search falls through to the next gap and the next window instead.
--  - A BAR smaller than a page takes one page, which keeps it aligned to
--    its own size, as Linux does for small LPSS BARs.
--  - An I/O BAR, a size that is not a power of two, or a BAR that already
--    holds an address is not this package's to place.
--
--  Proved (tests/pci-bar-assignment, level 2): an assigned BAR's words
--  decode to the placed span's first byte; its type bits are unchanged; a
--  32-bit BAR lies wholly below 4 GiB; no BAR starts below 1 MiB; and the
--  span meets PCI_BAR_Placement's contract (aligned, in a window and an
--  allowed run, disjoint from every taken span).
------------------------------------------------------------------------------
package PCI_BAR_Assignment with SPARK_Mode, Pure is
   package P renames PCI_BAR_Placement;

   Page_Bytes : constant := 4_096;
   Page_Shift : constant := 12;
   --  Pages below 4 GiB (a 32-bit BAR's whole address space).
   Low_4G_Pages : constant := 2 ** 20;
   --  The lowest address a BAR is placed at: 1 MiB, the end of the legacy
   --  VGA/ISA/BIOS ranges (Linux's BIOS_END, arch/x86/include/asm/e820/api.h).
   Lowest_Placement_Bytes : constant := 16#10_0000#;
   Lowest_Placement_Page  : constant := Lowest_Placement_Bytes / Page_Bytes;

   --  BAR low-word fields (PCI Local Bus 3.0, 6.2.5.1).
   BAR_IO_Space       : constant Unsigned_32 := 16#1#;
   BAR_Type_Mask      : constant Unsigned_32 := 16#6#;
   BAR_Type_64        : constant Unsigned_32 := 16#4#;
   BAR_Flag_Bits      : constant Unsigned_32 := 16#F#;
   BAR_Address_Mask   : constant Unsigned_32 := not BAR_Flag_Bits;

   function Is_IO (Low : Unsigned_32) return Boolean is ((Low and BAR_IO_Space) /= 0);
   function Is_64 (Low : Unsigned_32) return Boolean is ((Low and BAR_Type_Mask) = BAR_Type_64);

   --  Bytes a size needs in whole pages, at least one.
   Largest_BAR_Bytes : constant := 2 ** 39;   --  512 GiB
   subtype BAR_Bytes is Unsigned_64 range 1 .. Largest_BAR_Bytes;

   function Size_Pages (Size : BAR_Bytes) return P.Page is
     (if Size <= Page_Bytes then 1 else P.Page (Size / Page_Bytes))
   with Pre => Size <= Page_Bytes or else Size mod Page_Bytes = 0;

   type Outcome is
     (Assigned,
      Not_Memory,          --  an I/O BAR
      Already_Assigned,    --  its address bits are not all 0
      Invalid_Size,        --  not a power of two
      No_Room);            --  no window has an aligned free span

   type Assignment is record
      Result   : Outcome := No_Room;
      Where    : P.Span := (0, 0);
      New_Low  : Unsigned_32 := 0;
      New_High : Unsigned_32 := 0;
   end record;

   --  The address a BAR's words decode to.
   function Decoded (Low, High : Unsigned_32; Wide : Boolean) return Unsigned_64 is
     ((if Wide then Shift_Left (Unsigned_64 (High), 32) else 0)
      or Unsigned_64 (Low and BAR_Address_Mask));

   function Power_Of_Two (N : Unsigned_64) return Boolean is
     (N /= 0 and then (N and (N - 1)) = 0);

   --  Low and High are the BAR's words as read (High is 0 for a 32-bit
   --  BAR); Size is its decoded size in bytes; Windows are the host
   --  bridge's, in the order to try them; Allowed is devmgr's device
   --  memory (page runs); Taken is every extent already claimed.
   procedure Assign
     (Low, High : Unsigned_32;
      Size      : BAR_Bytes;
      Windows   : P.Span_Array;
      Allowed   : P.Span_Array;
      Taken     : P.Span_Array;
      A         : out Assignment)
   with
     Pre  => Windows'Length <= Natural'Last / 2,
     Post =>
       (if A.Result = Assigned then
          not Is_IO (Low)
          and then P.Valid (A.Where)
          and then (A.New_Low and BAR_Flag_Bits) = (Low and BAR_Flag_Bits)
          and then Decoded (A.New_Low, A.New_High, Is_64 (Low))
                   = A.Where.First * Page_Bytes
          and then (if not Is_64 (Low) then A.New_High = 0 and then A.Where.Past <= Low_4G_Pages)
          and then (A.Where.First and (A.Where.Past - A.Where.First - 1)) = 0
          and then A.Where.First >= Lowest_Placement_Page
          and then (for some W of Windows => P.Valid (W) and then P.Inside (A.Where, W))
          and then (for some R of Allowed => P.Valid (R) and then P.Inside (A.Where, R))
          and then (for all T of Taken => not P.Overlaps (A.Where, T))
        elsif A.Result = Not_Memory then Is_IO (Low)
        elsif A.Result = Invalid_Size then not Power_Of_Two (Size));
end PCI_BAR_Assignment;
