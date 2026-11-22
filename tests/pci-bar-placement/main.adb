pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with PCI_BAR_Placement; use PCI_BAR_Placement;
--  PCI-BAR-001: placement of a BAR the firmware left at address 0. Each case
--  checks the result against an independent brute-force search, so the
--  lowest valid place is found, not just some place.
procedure Main is
   Checks, Failures : Natural := 0;

   procedure Check (Ok : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Ok then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("PCI-BAR-PLACEMENT: FAIL " & What);
      end if;
   end Check;

   --  The lowest aligned span inside some window (in window order) and,
   --  within it, inside some allowed run (in run order) that overlaps
   --  nothing taken, by trying every aligned candidate.
   function Oracle (Windows, Allowed, Taken : Span_Array; Pages : Page; Where : out Span) return Boolean is
   begin
      Where := (0, 0);
      for W of Windows loop
         for R of Allowed loop
            if Valid (W) and then Valid (R) then
               declare
                  Lo : constant Page := Page'Max (W.First, R.First);
                  Hi : constant Page := Page'Min (W.Past, R.Past);
                  C : Page := (Lo + Pages - 1) / Pages * Pages;
               begin
                  while C + Pages <= Hi loop
                     if (for all T of Taken => not Overlaps ((C, C + Pages), T)) then
                        Where := (C, C + Pages);
                        return True;
                     end if;
                     C := C + Pages;
                  end loop;
               end;
            end if;
         end loop;
      end loop;
      return False;
   end Oracle;

   Everything : constant Span_Array := [(0, Page_Limit)];

   procedure Case_Of (Name : String; Windows, Taken : Span_Array; Pages : Page;
                      Allowed : Span_Array := Everything) is
      Result : Outcome;
      Where, Expected : Span;
      Exists : constant Boolean := Oracle (Windows, Allowed, Taken, Pages, Expected);
   begin
      Place (Windows, Allowed, Taken, Pages, Result, Where);
      Check ((Result = Placed) = Exists, Name & ": placed=" & Result'Image & " oracle=" & Exists'Image);
      if Result = Placed and Exists then
         Check (Where = Expected, Name & ": got" & Where.First'Image & " expected" & Expected.First'Image);
         Check ((for some R of Allowed => Valid (R) and then Inside (Where, R)), Name & ": inside an allowed run");
      end if;
   end Case_Of;

   --  The IdeaPad shape: a 64-bit window above 4 GiB (pages from 0x4000000)
   --  with firmware BARs at its start, and a 32-bit window below.
   High : constant Span := (16#400_0000#, 16#800_0000#);
   Low32 : constant Span := (16#8_0000#, 16#E_0000#);
   No_Taken : constant Span_Array (1 .. 0) := [others => (0, 0)];
   Result : Outcome;
   Where : Span;
begin
   Case_Of ("empty window, nothing taken", [High], No_Taken, 1);
   Case_Of ("first page taken", [High], [(16#400_0000#, 16#400_0001#)], 1);
   Case_Of ("aligned past a run of BARs", [High],
            [(16#400_0000#, 16#400_0010#), (16#400_0010#, 16#400_0011#)], 4);
   Case_Of ("hole between BARs exactly fits", [High],
            [(16#400_0000#, 16#400_0004#), (16#400_0008#, 16#400_0010#)], 4);
   Case_Of ("hole too small, goes past", [High],
            [(16#400_0000#, 16#400_0004#), (16#400_0006#, 16#400_0010#)], 4);
   Case_Of ("unordered taken spans", [High],
            [(16#400_0008#, 16#400_0010#), (16#400_0000#, 16#400_0004#), (16#400_0004#, 16#400_0008#)], 2);
   Case_Of ("window full", [(16#400_0000#, 16#400_0004#)], [(16#400_0000#, 16#400_0004#)], 1);
   Case_Of ("first window full, second used", [(16#400_0000#, 16#400_0004#), Low32],
            [(16#400_0000#, 16#400_0004#)], 1);
   Case_Of ("unaligned window start", [(16#400_0003#, 16#400_0100#)], No_Taken, 16);
   Case_Of ("taken span covering the window", [High], [(16#300_0000#, 16#900_0000#)], 1);
   Case_Of ("invalid window skipped", [(16#500#, 16#400#), Low32], No_Taken, 2);
   --  A window smaller than the BAR never fits (an unsigned Past - Pages
   --  would wrap and let it through).
   Place ([(16#400_0000#, 16#400_0002#)], Everything, No_Taken, 4, Result, Where);
   Check (Result = No_Room, "window smaller than the BAR refused");
   --  A window that ends below the BAR's size: Past - Pages wraps to a huge
   --  value, so the wrong loop test enters and places outside the window.
   Place ([(1, 3)], Everything, No_Taken, 4, Result, Where);
   Check (Result = No_Room, "window ending below the BAR size refused");
   Case_Of ("small window then a fitting one", [(16#400_0000#, 16#400_0002#), Low32], No_Taken, 4);
   Place ([High], Everything, No_Taken, 3, Result, Where);
   Check (Result = Invalid_Size, "non-power-of-two size refused");
   Place ([High], Everything, No_Taken, 0, Result, Where);
   Check (Result = Invalid_Size, "zero size refused");
   --  Allowed runs (devmgr's device memory): a place is found only where a
   --  window meets one, falling through to the next gap and window.
   Case_Of ("first window wholly outside the runs", [Low32, High], No_Taken, 1,
            Allowed => [(16#300_0000#, 16#900_0000#)]);
   Place ([Low32, High], [(16#300_0000#, 16#900_0000#)], No_Taken, 1, Result, Where);
   Check (Result = Placed and then Where.First = High.First, "outside runs: lowest page of the second window");
   Case_Of ("first window partly covered", [Low32, High], No_Taken, 4,
            Allowed => [(16#C_0000#, 16#900_0000#)]);
   Place ([Low32, High], [(16#C_0000#, 16#900_0000#)], No_Taken, 4, Result, Where);
   Check (Result = Placed and then Where.First = 16#C_0000#, "partly covered: first page of the covered part");
   Case_Of ("covered part taken, next run used", [Low32, High], [(16#8_0000#, 16#9_0000#)], 1,
            Allowed => [(16#8_0000#, 16#9_0000#), (16#D_0000#, 16#D_0010#)]);
   Place ([Low32, High], [(16#8_0000#, 16#9_0000#), (16#D_0000#, 16#D_0010#)], No_Taken, 32, Result, Where);
   Check (Result = Placed and then Where.First = 16#8_0000#, "32 pages fit the first run");
   Case_Of ("run too small for the BAR", [Low32], No_Taken, 32,
            Allowed => [(16#8_0000#, 16#8_0010#)]);
   Place ([Low32, High], [(16#8_0000#, 16#8_0010#)], No_Taken, 32, Result, Where);
   Check (Result = No_Room, "no window meets a run big enough: No_Room");
   Place ([Low32, High], [(16#1#, 16#10#)], No_Taken, 1, Result, Where);
   Check (Result = No_Room, "runs nowhere near the windows: No_Room");
   Place ([High], No_Taken, No_Taken, 1, Result, Where);
   Check (Result = No_Room, "no runs at all: No_Room");
   --  Random runs and taken fields against the oracle.
   for Seed in 1 .. 300 loop
      declare
         Taken : Span_Array (1 .. 6);
         Runs : Span_Array (1 .. 3);
         X : Unsigned_64 := Unsigned_64 (Seed) * 2654435761;
         function Next return Page is
         begin
            X := X * 6364136223846793005 + 1442695040888963407;
            return Shift_Right (X, 40);
         end Next;
      begin
         for I in Taken'Range loop
            declare
               F : constant Page := 16#400_0000# + Next mod 96;
            begin
               Taken (I) := (F, F + 1 + Next mod 6);
            end;
         end loop;
         for I in Runs'Range loop
            declare
               F : constant Page := 16#400_0000# - 8 + Next mod 112;
            begin
               Runs (I) := (F, F + Next mod 40);
            end;
         end loop;
         Case_Of ("random runs" & Seed'Image, [(16#400_0000#, 16#400_0060#), (16#400_0040#, 16#400_0070#)],
                  Taken, Page (2 ** Natural (Seed mod 4)), Allowed => Runs);
      end;
   end loop;
   --  A dense random-ish field of taken pages, against the oracle.
   for Seed in 1 .. 200 loop
      declare
         Taken : Span_Array (1 .. 12);
         X : Unsigned_64 := Unsigned_64 (Seed) * 2654435761;
      begin
         for I in Taken'Range loop
            X := X * 6364136223846793005 + 1442695040888963407;
            declare
               F : constant Page := 16#400_0000# + Shift_Right (X, 40) mod 64;
            begin
               Taken (I) := (F, F + 1 + Shift_Right (X, 20) mod 6);
            end;
         end loop;
         Case_Of ("random field" & Seed'Image, [(16#400_0000#, 16#400_0050#)], Taken,
                  Page (2 ** Natural (Seed mod 4)));
      end;
   end loop;
   if Failures = 0 then
      Ada.Text_IO.Put_Line ("PCI-BAR-PLACEMENT: PASS checks=" & Checks'Image);
   else
      Ada.Text_IO.Put_Line ("PCI-BAR-PLACEMENT: FAIL" & Failures'Image & " of" & Checks'Image);
      raise Program_Error;
   end if;
end Main;
