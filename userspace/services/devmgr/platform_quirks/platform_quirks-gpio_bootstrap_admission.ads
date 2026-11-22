------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  devmgr's decision on a GPIO bootstrap admission (ACPI-POOL-001 step
--  1b, CuBit.GPIO_Bootstrap_Exchange): may the spans acpi.svc observed in
--  the GPIO controller's _CRS before _INI become platform-pool members?
--
--  @description
--  The user's rule (2026-11-12): when P2SB discovery failed, one bounded
--  pre-_INI _HID/_STA/_CRS observation of the identified GPIO controller
--  supplies firmware-declared spans; devmgr validates them against the
--  attested memory map and adopts them as Chipset_Shared pool members
--  before GPIO-touching _INI, logged with provenance. ACPI is trusted
--  firmware data (one SystemMemory admission rule, 2026-11-04), so the
--  rule is general, not a per-field allow-list:
--    * gate: this boot's P2SB discovery ran and reported a typed failure
--      (else P2SB_Available / No_Discovery_Failure), and nothing was
--      admitted yet (Already_Admitted);
--    * identity: the quirk table names a GPIO controller for this platform
--      and the observed _HID is it, with _STA present and enabled
--      (Unknown_Controller);
--    * shape: every span nonempty, whole pages, below the physical limit,
--      and the spans pairwise disjoint (Malformed);
--    * memory map, per span in _CRS order: no page is usable RAM, a PCI
--      BAR's or bridge window's extent, ECAM, or an existing pool member;
--      and the span lies wholly inside ONE device-memory root (the
--      kernel's classification: PCI holes and firmware-reserved MMIO,
--      never RAM, kernel, ACPI NVS or tables): partly inside is Straddles,
--      outside every root Not_Chipset_MMIO.
--  devmgr holds no RAM map of its own: at run time it passes the RAM list
--  empty and relies on the kernel's device-memory roots, which never
--  contain RAM (a RAM span is then Not_Chipset_MMIO, and the kernel's
--  insert check refuses it again). The RAM rule is kept so the decision
--  is complete for any caller that has the map (the hosted tests do).
--
--  Pure and proved (tests/platform-quirks): the caller gathers the roots
--  and ranges; the decision maps, derives and inserts nothing.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.GPIO_Bootstrap_Exchange;
with Devmgr_Data_Windows;
with GPIO_Bootstrap_Payload;

package Platform_Quirks.GPIO_Bootstrap_Admission with Pure, SPARK_Mode is

   package GX renames CuBit.GPIO_Bootstrap_Exchange;
   package DW renames Devmgr_Data_Windows;
   package GP renames GPIO_Bootstrap_Payload;
   use type GX.Disposition;

   Page_Bytes : constant := 4096;

   --  The P2SB discovery this boot, as the sideband subsystem reported it.
   type Gate is record
      Discovery_Ran   : Boolean := False;  --  the quirk table selected P2SB_SBREG
      Discovery_Found : Boolean := False;  --  ... and it found the window
      Admitted_Before : Boolean := False;  --  an admission already happened
   end record;

   --  What devmgr knows of physical memory (pages).
   type Memory_View (Devices, RAM, BARs, ECAM, Pool : Natural) is record
      Device_Roots : DW.Range_Array (1 .. Devices);  --  its device-memory roots
      RAM_Ranges   : DW.Range_Array (1 .. RAM);      --  usable RAM, when known
      BAR_Extents  : DW.Range_Array (1 .. BARs);     --  claimed BAR/bridge extents
      ECAM_Ranges  : DW.Range_Array (1 .. ECAM);     --  MCFG allocations
      Pool_Members : DW.Range_Array (1 .. Pool);     --  members already in the pool
   end record;

   function View_Valid (V : Memory_View) return Boolean is
     (DW.Ranges_Valid (V.Device_Roots) and then DW.Ranges_Valid (V.RAM_Ranges)
      and then DW.Ranges_Valid (V.BAR_Extents) and then DW.Ranges_Valid (V.ECAM_Ranges)
      and then DW.Ranges_Valid (V.Pool_Members));

   --  The ACPI _STA bits a controller must report (ACPI 6.5 section 6.3.7:
   --  bit 0 present, bit 1 enabled).
   STA_Present : constant Unsigned_32 := 2 ** 0;
   STA_Enabled : constant Unsigned_32 := 2 ** 1;

   function Present_And_Enabled (STA : Unsigned_32) return Boolean is
     ((STA and STA_Present) /= 0 and then (STA and STA_Enabled) /= 0);

   --  The _HID the quirk table's controller has (Intel_GPIO_Communities).
   function Controller_Matches
     (Controller : Platform_Quirks.GPIO_Controller; HID : GP.HID_Text) return Boolean;

   --  A span's shape: nonempty whole pages below the page limit.
   function Shape_Valid (S : GP.Span) return Boolean is
     (S.Length > 0 and then S.Base mod Page_Bytes = 0 and then S.Length mod Page_Bytes = 0
      and then S.Base / Page_Bytes < DW.Page_Limit
      and then S.Length / Page_Bytes <= DW.Page_Limit - S.Base / Page_Bytes);

   function Pages_Of (S : GP.Span) return DW.Page_Range is
     ((First_Page => S.Base / Page_Bytes, Pages => S.Length / Page_Bytes))
   with Pre  => Shape_Valid (S),
        Post => DW.Valid_Range (Pages_Of'Result);

   function Shapes_Valid (R : GP.Request) return Boolean is
     (for all I in 1 .. R.Spans => Shape_Valid (R.Items (I)));

   function Spans_Disjoint (R : GP.Request) return Boolean is
     (for all I in 1 .. R.Spans =>
        (for all J in 1 .. R.Spans =>
           (if I /= J then not DW.Touches (Pages_Of (R.Items (I)), Pages_Of (R.Items (J))))))
   with Pre => Shapes_Valid (R);

   function Inside_Some (A : DW.Page_Range; Items : DW.Range_Array) return Boolean is
     (for some I in Items'Range => DW.Inside (A, Items (I)))
   with Pre => DW.Valid_Range (A) and then DW.Ranges_Valid (Items);

   --  Every rule for one span, against the memory map.
   function Span_Admissible (S : GP.Span; V : Memory_View) return Boolean is
     (Shape_Valid (S)
      and then not DW.Touches_Any (Pages_Of (S), V.RAM_Ranges)
      and then not DW.Touches_Any (Pages_Of (S), V.BAR_Extents)
      and then not DW.Touches_Any (Pages_Of (S), V.ECAM_Ranges)
      and then not DW.Touches_Any (Pages_Of (S), V.Pool_Members)
      and then Inside_Some (Pages_Of (S), V.Device_Roots))
   with Pre => View_Valid (V);

   type Decision is record
      Answer : GX.Disposition := GX.Malformed;
      Span   : GP.Span_Count := 0;   --  the span a span refusal names (1-based)
   end record;

   function Decide
     (G : Gate; Controller : Platform_Quirks.GPIO_Controller;
      R : GP.Request; V : Memory_View) return Decision
   with Pre  => GP.Valid (R) and then View_Valid (V),
        Post =>
          (if Decide'Result.Answer = GX.Admitted then
             G.Discovery_Ran and then not G.Discovery_Found and then not G.Admitted_Before
             and then Controller_Matches (Controller, R.HID)
             and then Present_And_Enabled (R.STA)
             and then Shapes_Valid (R)
             and then Spans_Disjoint (R)
             and then (for all I in 1 .. R.Spans => Span_Admissible (R.Items (I), V)))
          and then (if not G.Discovery_Ran then Decide'Result.Answer = GX.No_Discovery_Failure)
          and then (if G.Discovery_Ran and then G.Discovery_Found then
                      Decide'Result.Answer = GX.P2SB_Available)
          and then Decide'Result.Answer in
                     GX.Admitted | GX.P2SB_Available | GX.No_Discovery_Failure
                   | GX.Already_Admitted | GX.Malformed | GX.Unknown_Controller
                   | GX.Not_Chipset_MMIO | GX.Overlaps_RAM | GX.Overlaps_BAR
                   | GX.Overlaps_PCI_Config | GX.Overlaps_Pool_Member | GX.Straddles
          and then (if Decide'Result.Answer in GX.Not_Chipset_MMIO | GX.Overlaps_RAM
                      | GX.Overlaps_BAR | GX.Overlaps_PCI_Config | GX.Overlaps_Pool_Member
                      | GX.Straddles
                    then Decide'Result.Span in 1 .. R.Spans);

   --  Verified for use without an admission (devmgr read the P2SB this
   --  boot, so the answer is P2SB_Available): the proposal names this
   --  platform's controller, present and enabled, and its spans are
   --  exactly the GPIO community ranges devmgr derived from the P2SB and
   --  already inserted as pool members: as many spans as ranges, each span
   --  equal to one range and to one inserted member. Nothing is admitted
   --  by it; it only lets devmgr record the controller (and launch its
   --  driver) as if it had admitted those spans.
   function Matches_Communities
     (Controller : Platform_Quirks.GPIO_Controller; R : GP.Request;
      Communities, Members : DW.Range_Array) return Boolean
   is (Controller_Matches (Controller, R.HID)
       and then Present_And_Enabled (R.STA)
       and then Shapes_Valid (R)
       and then Spans_Disjoint (R)
       and then R.Spans = Communities'Length
       and then (for all I in 1 .. R.Spans =>
                   (for some C of Communities => DW."=" (C, Pages_Of (R.Items (I))))
                   and then (for some M of Members => DW."=" (M, Pages_Of (R.Items (I))))))
   with Pre => GP.Valid (R) and then DW.Ranges_Valid (Communities)
               and then DW.Ranges_Valid (Members);

end Platform_Quirks.GPIO_Bootstrap_Admission;
