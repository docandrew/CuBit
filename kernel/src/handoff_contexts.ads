-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Which xHCI controllers pass DMA through untranslated until devmgr has
-- taken them from the firmware (DMA_Remapping, Linux's
-- quirk_usb_early_handoff order).
--
-- @description
-- The N150's firmware drives 00:14.0 for legacy USB until the OS asks for
-- it (xHCI 1.2 section 4.22.1, USBLEGSUP). With the IOMMU translating from
-- boot, the firmware's DMA into its own reserved memory faulted, and its
-- SMI handler held the whole machine in SMM for about 10 s (image 34b:
-- "stage handoff ok ... ms=9966"; 00:0D.0, which the firmware does not
-- own, ms=0). Linux takes the controller from the firmware before DMA
-- remapping is on.
--
-- Here, at Setup (before translation), every xHCI-class function behind a
-- translating unit is Admitted: its context entry passes DMA through, so
-- the firmware's DMA reaches memory. Setup then Seals the table: nothing is
-- admitted after boot. devmgr does the firmware handoff and halts the
-- controller before its first DMA grant; that grant's domain Switches the
-- requester to Translated. The only way into Passing_Through is Admit
-- before the seal, and nothing leads out of Translated: a requester never
-- passes through again.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Handoff_Contexts with Pure, SPARK_Mode is

   subtype Requester is Unsigned_16;

   type Phase is (Untracked, Passing_Through, Translated);

   type Table is private;

   function Sealed (T : Table) return Boolean;
   function Phase_Of (T : Table; Who : Requester) return Phase;

   -- Boot: nothing tracked, not sealed.
   function Empty return Table
   with Post => not Sealed (Empty'Result)
     and then (for all R in Requester => Phase_Of (Empty'Result, R) = Untracked);

   -- Setup only (before the seal): an untracked requester passes through.
   procedure Admit (T : in out Table; Who : Requester; Admitted : out Boolean)
   with Pre => not Sealed (T),
        Post => not Sealed (T)
          and then Admitted = (Phase_Of (T'Old, Who) = Untracked)
          and then (if Admitted then Phase_Of (T, Who) = Passing_Through
                    else Phase_Of (T, Who) = Phase_Of (T'Old, Who))
          and then (for all R in Requester =>
                      (if R /= Who then Phase_Of (T, R) = Phase_Of (T'Old, R)));

   -- The end of Setup: no admission after it, ever.
   procedure Seal (T : in out Table)
   with Post => Sealed (T)
          and then (for all R in Requester => Phase_Of (T, R) = Phase_Of (T'Old, R));

   -- Who's first domain: a passing-through requester is Translated from
   -- now on (Was_Passing True); any other is left as it was.
   procedure Switch (T : in out Table; Who : Requester; Was_Passing : out Boolean)
   with Post => Sealed (T) = Sealed (T'Old)
          and then Was_Passing = (Phase_Of (T'Old, Who) = Passing_Through)
          and then (if Was_Passing then Phase_Of (T, Who) = Translated
                    else Phase_Of (T, Who) = Phase_Of (T'Old, Who))
          and then (for all R in Requester =>
                      (if R /= Who then Phase_Of (T, R) = Phase_Of (T'Old, R)));

   -- The promise, for callers and the hosted test: after the seal no
   -- operation makes a requester pass through that did not already.
   function Never_Reopened (Before, After : Table) return Boolean is
     (for all R in Requester =>
        (if Phase_Of (After, R) = Passing_Through
         then Phase_Of (Before, R) = Passing_Through))
   with Ghost;

private
   type Phase_Map is array (Requester) of Phase with Pack;
   type Table is record
      Is_Sealed : Boolean := False;
      Phases    : Phase_Map := [others => Untracked];
   end record;

   function Sealed (T : Table) return Boolean is (T.Is_Sealed);
   function Phase_Of (T : Table; Who : Requester) return Phase is (T.Phases (Who));
end Handoff_Contexts;
