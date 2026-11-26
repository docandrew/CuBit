--  SPDX-License-Identifier: GPL-3.0-or-later
--  MEM-007 (K5): the hosted model test of a munmap-style release, the walk
--  the kernel instantiates (kernel/src/owned_release_walk, used by
--  Process.Owned_Memory.Release_Range) over a model of a process's regions,
--  checked against an oracle.
--
--  Exhaustive over every layout of regions and holes on Pages pages (each
--  page a hole, the start of a region, or the continuation of the region
--  before it) and every page range, including ranges that reach past the
--  layout on either side, and, for each, a refusal injected at each Apply.
--  The oracle: with no refusal, every page of the range leaves its region,
--  each region's pages outside the range stay, split into separate regions
--  where the range cuts through; Touched exactly when a region page lay in
--  the range. With a refusal at the k-th region, the regions before it are
--  cut as the oracle says and the rest are unchanged.
--  The kernel's decommit, locking, record moves and run reparenting are not
--  modelled here (the native check, owned-record-table, runs those).
pragma Ada_2022;
with Interfaces; use Interfaces;
with Owned_Reservation_Policy;
with Owned_Release_Walk;
with Ada.Text_IO;
procedure Release_Walk_Tests is
   package Policy renames Owned_Reservation_Policy;
   Page : constant Unsigned_64 := Policy.Page_Bytes;
   Pages : constant := 8;
   --  The model's first page: a page-aligned address, as the aperture's.
   Origin : constant Unsigned_64 := 16#0000_1000_0000_0000#;
   Checks : Natural := 0;

   --  Each page's region (0: a hole). Regions are numbered by their first
   --  page; equal numbers on adjacent pages are one region.
   type Layout is array (0 .. Pages - 1) of Natural;
   Model : Layout;
   Next_Label : Natural;
   Refuse_At, Applied : Natural;

   procedure Check (Good : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Good then
         Ada.Text_IO.Put_Line ("FAIL " & What);
         raise Program_Error;
      end if;
   end Check;

   function Address (P : Natural) return Unsigned_64 is (Origin + Unsigned_64 (P) * Page);

   --  The lowest region overlapping [From, Limit): its first page and size.
   procedure Next_Region (From, Limit : Unsigned_64; Found : out Boolean;
                          Base, Bytes : out Unsigned_64) is
      P : Natural := 0;
   begin
      Found := False;
      Base := 0;
      Bytes := 0;
      while P < Pages loop
         if Model (P) /= 0 then
            declare
               Q : Natural := P;
            begin
               while Q + 1 < Pages and then Model (Q + 1) = Model (P) loop
                  Q := Q + 1;
               end loop;
               if Address (P) < Limit and then Address (Q) + Page > From then
                  Found := True;
                  Base := Address (P);
                  Bytes := Unsigned_64 (Q - P + 1) * Page;
                  return;
               end if;
               P := Q + 1;
            end;
         else
            P := P + 1;
         end if;
      end loop;
   end Next_Region;

   --  Take Cut.Dropped pages out of the region at Base; the upper part of a
   --  middle cut becomes a region of its own (a new label).
   procedure Apply (Base, Bytes : Unsigned_64; Cut : Policy.Run_Split; Done : out Boolean) is
      First : constant Natural := Natural ((Base - Origin) / Page);
   begin
      Applied := Applied + 1;
      if Applied = Refuse_At then
         Done := False;
         return;
      end if;
      Check (Cut.Lower + Cut.Dropped + Cut.Upper = Bytes / Page, "the cut covers the region");
      for P in First + Natural (Cut.Lower) .. First + Natural (Cut.Lower + Cut.Dropped) - 1 loop
         Model (P) := 0;
      end loop;
      if Cut.Lower > 0 and then Cut.Upper > 0 then
         Next_Label := Next_Label + 1;
         for P in First + Natural (Cut.Lower + Cut.Dropped) .. First + Natural (Bytes / Page) - 1 loop
            Model (P) := Next_Label;
         end loop;
      end if;
      Done := True;
   end Apply;

   procedure Walk is new Owned_Release_Walk (Next_Region, Apply);

   --  Regions as (first page, last page) runs of equal labels.
   function Same_Regions (A, B : Layout) return Boolean is
   begin
      for P in Layout'Range loop
         if (A (P) = 0) /= (B (P) = 0) then return False; end if;
         if P > 0 and then A (P) /= 0 and then A (P - 1) /= 0
           and then (A (P) = A (P - 1)) /= (B (P) = B (P - 1))
         then
            return False;
         end if;
      end loop;
      return True;
   end Same_Regions;

   Initial, Expected : Layout;
   Code, Rest : Natural;
   Touched, Done : Boolean;
   Combinations : Natural := 0;
begin
   --  3 ** Pages layouts: per page 0 hole, 1 a new region, 2 continue
   --  (a continuation after a hole is a new region).
   for Index in 0 .. 3 ** Pages - 1 loop
      Code := Index;
      Next_Label := 0;
      for P in Layout'Range loop
         Rest := Code mod 3;
         Code := Code / 3;
         if Rest = 0 then
            Initial (P) := 0;
         elsif Rest = 2 and then P > 0 and then Initial (P - 1) /= 0 then
            Initial (P) := Initial (P - 1);
         else
            Next_Label := Next_Label + 1;
            Initial (P) := Next_Label;
         end if;
      end loop;
      --  Every range [Low, High) of pages, one page before the layout to
      --  one after it.
      for Low in -1 .. Pages loop
         for High in Low + 1 .. Pages + 1 loop
            declare
               Range_Base : constant Unsigned_64 := Origin + Unsigned_64 (Low + 1) * Page - Page;
               Range_Bytes : constant Unsigned_64 := Unsigned_64 (High - Low) * Page;
               Regions_In : Natural := 0;
               Seen_Label : Natural := 0;
            begin
               --  How many regions the range touches (refusals at 1 .. that).
               for P in Layout'Range loop
                  if P >= Low and then P < High and then Initial (P) /= 0
                    and then Initial (P) /= Seen_Label
                  then
                     Regions_In := Regions_In + 1;
                     Seen_Label := Initial (P);
                  end if;
               end loop;
               for Refusal in 0 .. Regions_In loop
                  Model := Initial;
                  Applied := 0;
                  Refuse_At := Refusal;
                  Walk (Range_Base, Range_Bytes, Touched, Done);
                  --  The oracle: the regions before the refused one cut,
                  --  the rest unchanged.
                  Expected := Initial;
                  declare
                     Count : Natural := 0;
                     Label : Natural := 0;
                     Split_Label : Natural := 1_000;
                  begin
                     for P in Layout'Range loop
                        if Initial (P) /= 0 and then Initial (P) /= Label then
                           Label := Initial (P);
                           if P < High and then
                             (for some Q in Layout'Range =>
                                Initial (Q) = Label and then Q >= Low and then Q < High)
                           then
                              Count := Count + 1;
                           end if;
                        end if;
                        if Initial (P) /= 0 and then P >= Low and then P < High
                          and then (Refusal = 0 or else Count < Refusal)
                        then
                           Expected (P) := 0;
                        end if;
                        --  A page above the cut of a region the walk cut
                        --  becomes its own region.
                        if Initial (P) /= 0 and then P >= High and then P > 0
                          and then Expected (P - 1) = 0 and then Initial (P - 1) = Initial (P)
                          and then (Refusal = 0 or else Count < Refusal)
                        then
                           Split_Label := Split_Label + 1;
                        end if;
                        if Initial (P) /= 0 and then P >= High and then Split_Label > 1_000
                          and then (for some Q in 0 .. P - 1 =>
                                      Initial (Q) = Initial (P) and then Expected (Q) = 0
                                      and then Q >= Low)
                        then
                           Expected (P) := Split_Label;
                        end if;
                     end loop;
                  end;
                  Check (Touched = (Regions_In > 0), "touched");
                  Check (Done = (Refusal = 0), "done");
                  Check (Same_Regions (Model, Expected), "regions after the walk");
                  Combinations := Combinations + 1;
               end loop;
            end;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("owned release walk: PASS" & Natural'Image (Combinations) &
                         " walks, checks=" & Natural'Image (Checks) &
                         "; a hosted model of the regions, not the kernel's records");
end Release_Walk_Tests;
