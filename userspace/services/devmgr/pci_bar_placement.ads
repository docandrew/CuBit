pragma Ada_2022;
with Interfaces; use Interfaces;
------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Where to place a PCI memory BAR the firmware left unassigned (PCI-BAR-001).
--
--  @description
--  Some firmware leaves a function's BAR at address 0 on purpose. The
--  IdeaPad's LPSS I2C controllers are an example: the I2C1._PS3 method calls
--  SOD3, which enters D3 and clears BAR 0, and _PS0 is empty when the
--  controller is in PCI mode. Linux places such a BAR inside the host
--  bridge's _CRS window (pci_assign_unassigned_resources). This package
--  decides the place, and nothing else:
--  - Windows are where the host bridge decodes memory to PCI. They come from
--    _CRS through acpi.svc, never from memory-map holes, which may hold
--    hidden devices.
--  - Allowed is where devmgr may place at all: the device-memory runs the
--    kernel granted it. A window is only a candidate where it meets one of
--    them, so a place is legal when it is found, not refused afterwards
--    (the IdeaPad's _CRS lists the legacy VGA hole 0xA0000..0xBFFFF first,
--    which no device-memory run covers).
--  - Taken is every extent already claimed: other BARs, bridge windows,
--    ACPI's claims and firmware reservations.
--  - A BAR of Pages pages (a power of two) is placed at the lowest address
--    that is aligned to its size, lies inside one window and one allowed
--    run and overlaps nothing taken. It searches Windows in the caller's
--    order, so the caller lists the window it prefers first (below 4 GiB
--    for a non-prefetchable BAR); within a window, it tries Allowed in the
--    caller's order (devmgr passes the runs ascending).
--
--  Spans are page ranges [First, Past), Past exclusive. Proved: a found
--  span is aligned, exactly Pages long, inside a window and an allowed run,
--  and disjoint from every taken span; "not found" means no candidate the
--  search visited fit.
------------------------------------------------------------------------------
package PCI_BAR_Placement with SPARK_Mode, Pure is
   --  Physical page numbers: 52-bit physical addresses at most.
   Page_Limit : constant := 2 ** 40;
   subtype Page is Unsigned_64 range 0 .. Page_Limit;

   type Span is record
      First : Page := 0;
      Past  : Page := 0;
   end record;

   function Valid (S : Span) return Boolean is (S.First < S.Past);

   function Overlaps (A, B : Span) return Boolean is
     (A.First < B.Past and then B.First < A.Past);

   function Inside (Inner, Outer : Span) return Boolean is
     (Outer.First <= Inner.First and then Inner.Past <= Outer.Past);

   --  Where A and B meet (not Valid when they do not).
   function Meet (A, B : Span) return Span is
     ((Page'Max (A.First, B.First), Page'Min (A.Past, B.Past)))
   with Post => (if Valid (Meet'Result) then Inside (Meet'Result, A) and then Inside (Meet'Result, B));

   type Span_Array is array (Positive range <>) of Span;

   --  A BAR's size in pages: a power of two, at least one page.
   function Power_Of_Two (N : Page) return Boolean is
     (N /= 0 and then (N and (N - 1)) = 0);

   type Outcome is (Placed, No_Room, Invalid_Size);

   procedure Place
     (Windows : Span_Array;
      Allowed : Span_Array;
      Taken   : Span_Array;
      Pages   : Page;
      Result  : out Outcome;
      Where   : out Span)
   with
     Post =>
       (if Result = Placed then
          Power_Of_Two (Pages)
          and then Valid (Where)
          and then Where.Past - Where.First = Pages
          and then (Where.First and (Pages - 1)) = 0
          and then (for some W of Windows => Valid (W) and then Inside (Where, W))
          and then (for some R of Allowed => Valid (R) and then Inside (Where, R))
          and then (for all T of Taken => not Overlaps (Where, T))
        elsif Result = Invalid_Size then not Power_Of_Two (Pages));
end PCI_BAR_Placement;
