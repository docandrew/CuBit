--  Boot Logs repaints only what a change shows (LAT-002, 2026-11-15):
--  tests/boot-logs-repaint. The scenarios are the window's, as its main
--  builds them: a body (header, pinned summary, the page's rows) and the
--  counts line. Before the fix every arriving record repainted the whole
--  window; the first scenario is that idle repainter.
with Ada.Text_IO;
with Boot_Logs_Repaint; use Boot_Logs_Repaint;
procedure Boot_Logs_Repaint_Tests is
   Body_Bytes : constant := 64 * 1024;
   Counts_Bytes : constant := 256;
   Painted_Body, Body_Now : Snapshot (Body_Bytes);
   Painted_Counts, Counts_Now : Snapshot (Counts_Bytes);
   Checks : Natural := 0;

   --  Page Page of Total records, Rows to a page, as Body_Lines lays it out.
   procedure Window (S : in out Snapshot; C : in out Snapshot; Page, Rows, Total : Natural;
                     Pinned : String := "") is
      First : constant Natural := Page * Rows + 1;
   begin
      Clear (S);
      Add_Line (S, 10, "Boot diagnostics: all sources", False);
      if Pinned'Length > 0 then
         Add_Line (S, 40, Pinned, True);
      end if;
      for I in 0 .. Rows - 1 loop
         if First + I <= Total then
            Add_Line (S, 80 + I * 20, "record" & Natural'Image (First + I), False);
         end if;
      end loop;
      Clear (C);
      Add_Line (C, 0, "Page" & Natural'Image (Page + 1) & "  Records" & Natural'Image (Total), False);
   end Window;

   procedure Expect (Got, Wanted : Need; What : String) is
   begin
      Checks := Checks + 1;
      if Got /= Wanted then
         Ada.Text_IO.Put_Line ("FAIL " & What & ": " & Need'Image (Got) & " /= " & Need'Image (Wanted));
         raise Program_Error;
      end if;
   end Expect;

   procedure Paint is
   begin
      Painted_Body := Body_Now;
      Painted_Counts := Counts_Now;
   end Paint;
begin
   --  The idle repainter: page 1 of 3 shown, a record arrives at the end.
   Window (Body_Now, Counts_Now, 0, 25, 60); Paint;
   Window (Body_Now, Counts_Now, 0, 25, 61);
   Expect (Decide (Painted_Body, Body_Now, Painted_Counts, Counts_Now), Counts_Only,
           "a record off the shown page");
   --  Nothing new at all (a names refresh that changed no label).
   Paint;
   Window (Body_Now, Counts_Now, 0, 25, 61);
   Expect (Decide (Painted_Body, Body_Now, Painted_Counts, Counts_Now), Nothing, "no change");
   --  The last page is shown and has room: the record appears on it.
   Window (Body_Now, Counts_Now, 2, 25, 61); Paint;
   Window (Body_Now, Counts_Now, 2, 25, 62);
   Expect (Decide (Painted_Body, Body_Now, Painted_Counts, Counts_Now), Whole, "a record on the shown page");
   --  A warning pinned to the summary.
   Paint;
   Window (Body_Now, Counts_Now, 2, 25, 62, Pinned => "intel-gpu: backing denied");
   Expect (Decide (Painted_Body, Body_Now, Painted_Counts, Counts_Now), Whole, "a pinned record");
   --  The page turns.
   Paint;
   Window (Body_Now, Counts_Now, 0, 25, 62, Pinned => "intel-gpu: backing denied");
   Expect (Decide (Painted_Body, Body_Now, Painted_Counts, Counts_Now), Whole, "a page turn");
   --  A body that does not fit never compares equal: repaint it whole.
   Paint;
   declare
      Small_Painted, Small_Now : Snapshot (64);
   begin
      Clear (Small_Painted);
      Add_Line (Small_Painted, 10, [1 .. 100 => 'x'], False);
      Small_Now := Small_Painted;
      pragma Assert (not Complete (Small_Now));
      Expect (Decide (Small_Painted, Small_Now, Painted_Counts, Counts_Now), Whole, "an overflowing snapshot");
   end;
   --  Position and colour are part of what is shown.
   Clear (Painted_Body); Add_Line (Painted_Body, 10, "same", False);
   Clear (Body_Now); Add_Line (Body_Now, 30, "same", False);
   Expect (Decide (Painted_Body, Body_Now, Painted_Counts, Counts_Now), Whole, "a moved line");
   Clear (Body_Now); Add_Line (Body_Now, 10, "same", True);
   Expect (Decide (Painted_Body, Body_Now, Painted_Counts, Counts_Now), Whole, "a recoloured line");
   --  Nothing painted yet: the first look repaints.
   declare
      Never_Painted : Snapshot (Body_Bytes);
   begin
      Clear (Never_Painted);
      Window (Body_Now, Counts_Now, 0, 25, 1);
      Expect (Decide (Never_Painted, Body_Now, Painted_Counts, Counts_Now), Whole, "the first paint");
   end;
   --  The serial evidence names the first changed line.
   Clear (Painted_Body); Add_Line (Painted_Body, 10, "a", False); Add_Line (Painted_Body, 30, "b", False);
   Clear (Body_Now); Add_Line (Body_Now, 10, "a", False); Add_Line (Body_Now, 30, "c", False);
   pragma Assert (First_Change (Painted_Body, Body_Now) = " 30 c");
   pragma Assert (First_Change (Body_Now, Body_Now) = "");
   Ada.Text_IO.Put_Line ("PASS boot-logs repaint checks=" & Natural'Image (Checks));
end Boot_Logs_Repaint_Tests;
