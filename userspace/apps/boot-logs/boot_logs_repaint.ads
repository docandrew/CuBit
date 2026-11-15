------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What Boot Logs repaints when its records change (LAT-002, 2026-11-15).
--  Every arriving record used to repaint the whole window, though most
--  land off the page shown and change only the counts line. Display's and
--  Desktop's own once-a-second log lines (written because a frame was
--  presented) kept that going on an idle machine: ~950k pixels four times a
--  second on the laptop.
--
--  A Snapshot is what one part of the window shows: each line's position,
--  colour and text, in drawing order. The window repaints the whole body
--  only when the body's snapshot differs from the one last painted, the
--  counts line alone when only it differs, and nothing otherwise. A
--  snapshot that did not fit never compares equal, so it repaints.
------------------------------------------------------------------------------
package Boot_Logs_Repaint with SPARK_Mode, Pure is
   type Snapshot (Capacity : Positive) is private;
   procedure Clear (S : in out Snapshot);
   procedure Add_Line (S : in out Snapshot; Y : Natural; Text : String; Danger : Boolean);
   --  Both complete and holding the same lines.
   function Same (A, B : Snapshot) return Boolean;
   function Complete (S : Snapshot) return Boolean;
   --  The line of B at the first difference from A ("" when the same), for
   --  the viewer's serial evidence of what made it repaint.
   function First_Change (A, B : Snapshot) return String;

   type Need is (Nothing, Counts_Only, Whole);
   function Decide (Painted_Body, Body_Now, Painted_Counts, Counts_Now : Snapshot) return Need is
     (if not Same (Painted_Body, Body_Now) then Whole
      elsif not Same (Painted_Counts, Counts_Now) then Counts_Only
      else Nothing);
private
   type Snapshot (Capacity : Positive) is record
      Length : Natural := 0;
      Overflow : Boolean := False;
      Data : String (1 .. Capacity) := [others => ' '];
   end record
     with Predicate => Length <= Capacity;
   function Complete (S : Snapshot) return Boolean is (not S.Overflow);
   function Same (A, B : Snapshot) return Boolean is
     (not A.Overflow and then not B.Overflow and then A.Length = B.Length and then
      A.Data (1 .. A.Length) = B.Data (1 .. B.Length));
end Boot_Logs_Repaint;
