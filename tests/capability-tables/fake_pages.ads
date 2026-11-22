-- Hosted stand-in for Charged_Metadata_Pages: hands out real 4 KiB,
-- page-aligned blocks, counts what each owner holds, and refuses on demand
-- (a budget of pages, or the next N requests) so tests can drive every
-- allocation failure. Detects double and foreign releases.
with Interfaces; use Interfaces;
with System;

package Fake_Pages with SPARK_Mode is
   subtype Owner_Id is Unsigned_64;
   type Page_Total is range 0 .. 2 ** 31 - 1;

   procedure Allocate_Page
     (Owner : Owner_Id; Page : out System.Address; OK : out Boolean);
   procedure Release_Page (Page : System.Address);

   -- Test controls (not called by the table).
   procedure Reset;
   -- Allow at most this many further successful allocations.
   procedure Set_Budget (Pages : Page_Total);
   procedure Clear_Budget;
   -- Pages currently held (allocated, not released), per owner and total.
   function Held (Owner : Owner_Id) return Page_Total;
   function Held_Total return Page_Total;
   function Allocations return Page_Total;
   function Releases return Page_Total;
   function Refusals return Page_Total;
   -- A release named a page that was not held.
   function Bad_Releases return Page_Total;
end Fake_Pages;
