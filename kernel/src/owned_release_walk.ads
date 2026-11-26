-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- The walk of a munmap-style release over a process's regions (MEM-007,
-- K5): RELEASE_OWNED_REGION's range is cut out of every region it overlaps,
-- lowest region first, across the holes between them. The kernel
-- (Process.Owned_Memory.Release_Range) and the hosted model test
-- (tests/owned-regions/release_walk_tests) instantiate the same walk, so
-- the cursor arithmetic and the parts each region is cut into
-- (Owned_Reservation_Policy.Split, proved) are what is tested.
--
-- Next_Region gives the lowest region overlapping [From, Limit); Apply
-- takes the part Cut.Dropped of it (pages), keeping Cut.Lower pages below
-- and Cut.Upper above (the whole region when both are 0), and says whether
-- it did. The walk stops at the first refusal; regions before it stay cut
-- (as munmap). Touched: some region overlapped the range.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Owned_Reservation_Policy;

generic
   with procedure Next_Region (From, Limit : Unsigned_64; Found : out Boolean;
                               Base, Bytes : out Unsigned_64);
   with procedure Apply (Base, Bytes : Unsigned_64; Cut : Owned_Reservation_Policy.Run_Split;
                         Done : out Boolean);
procedure Owned_Release_Walk (Base, Bytes : Unsigned_64; Touched, Done : out Boolean)
  with Pre => Bytes > 0 and then Bytes mod Owned_Reservation_Policy.Page_Bytes = 0
              and then Base mod Owned_Reservation_Policy.Page_Bytes = 0
              and then Base <= Unsigned_64'Last - Bytes;
