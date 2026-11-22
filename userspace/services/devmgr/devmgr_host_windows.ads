pragma Ada_2022;
with Interfaces; use Interfaces;
with PCI_BAR_Placement;
------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The host bridges' memory windows (PCI-BAR-001): where each PCI root
--  decodes memory, as acpi.svc publishes it from the root's live _CRS.
--
--  @description
--  devmgr places a BAR the firmware left unassigned only inside these
--  windows, never in memory-map holes, which may hold hidden devices.
--  - The ACPI host-bridge snapshot consumer replaces a root's windows with
--    each accepted snapshot.
--  - Until the first snapshot arrives, Known is False and launches that need
--    a placed BAR wait (devmgr retries them when it changes).
--  - Windows are page spans, held on devmgr's heap with no fixed count
--    (LIM-001).
------------------------------------------------------------------------------
package Devmgr_Host_Windows is
   package PL renames PCI_BAR_Placement;

   type Root_Window is record
      Segment      : Unsigned_16 := 0;
      First_Bus    : Unsigned_8 := 0;
      Last_Bus     : Unsigned_8 := 0;
      Window       : PL.Span := (0, 0);
      Prefetchable : Boolean := False;
   end record;

   type Update_Result is (Updated, No_Memory);

   --  Replace the windows of the root (Segment, First_Bus .. Last_Bus) with
   --  Windows. Each window is a memory range the root decodes to PCI.
   type Window is record
      Span         : PL.Span := (0, 0);
      Prefetchable : Boolean := False;
   end record;
   type Window_Array is array (Positive range <>) of Window;

   procedure Replace_Root
     (Segment : Unsigned_16; First_Bus, Last_Bus : Unsigned_8;
      Windows : Window_Array; Result : out Update_Result);

   --  Whether any root's windows have been published.
   function Known return Boolean;

   --  The windows of the root that decodes Segment/Bus, in the order a BAR
   --  should try them: windows below 4 GiB first, unless High_First (a
   --  prefetchable 64-bit BAR prefers the high window). Last is the number
   --  written to Result (0 when no root covers the bus).
   procedure Windows_For
     (Segment : Unsigned_16; Bus : Unsigned_8; High_First : Boolean;
      Result : out PL.Span_Array; Last : out Natural);

   --  How many windows Windows_For can return for Segment/Bus.
   function Count_For (Segment : Unsigned_16; Bus : Unsigned_8) return Natural;
end Devmgr_Host_Windows;
