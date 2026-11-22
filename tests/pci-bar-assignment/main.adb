pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with PCI_BAR_Placement;
with PCI_BAR_Assignment; use PCI_BAR_Assignment;
--  PCI-BAR-001: the words devmgr writes to give an unassigned memory BAR an
--  address. Every case decodes the words back and checks them against the
--  span, the type bits, the 4 GiB rule and an independent search.
procedure Main is
   package PL renames PCI_BAR_Placement;
   use type PL.Span;
   Checks, Failures : Natural := 0;
   procedure Check (Ok : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Ok then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("PCI-BAR-ASSIGNMENT: FAIL " & What);
      end if;
   end Check;

   G : constant := 2 ** 18;   --  pages per GiB
   --  The IdeaPad-like host bridge: a 32-bit window 0x8000_0000 .. 0xC000_0000
   --  and a 64-bit window at 0x40_0000_0000 (256 GiB) of 64 GiB.
   Low_Window  : constant PL.Span := (2 * G, 3 * G);
   High_Window : constant PL.Span := (256 * G, 320 * G);
   Both : constant PL.Span_Array := [High_Window, Low_Window];
   None : constant PL.Span_Array (1 .. 0) := [others => (0, 0)];
   Everything : constant PL.Span_Array := [(0, PL.Page_Limit)];

   LPSS_64 : constant Unsigned_32 := 16#4#;           --  64-bit, non-prefetchable, address 0
   Plain_32 : constant Unsigned_32 := 16#0#;          --  32-bit, non-prefetchable
   Prefetch_64 : constant Unsigned_32 := 16#C#;       --  64-bit, prefetchable

   procedure Expect_Assigned (Low : Unsigned_32; Size : BAR_Bytes; W, T : PL.Span_Array; What : String;
                              Out_A : out Assignment; Allowed : PL.Span_Array := Everything) is
      A : Assignment;
   begin
      Assign (Low, 0, Size, W, Allowed, T, A);
      Out_A := A;
      Check (A.Result = Assigned, What & ": assigned");
      if A.Result /= Assigned then return; end if;
      Check ((A.New_Low and BAR_Flag_Bits) = (Low and BAR_Flag_Bits), What & ": type bits kept");
      Check (Decoded (A.New_Low, A.New_High, Is_64 (Low)) = A.Where.First * Page_Bytes,
             What & ": words decode to the span");
      Check (A.Where.Past - A.Where.First * Page_Bytes >= Size, What & ": big enough");
      Check (Decoded (A.New_Low, A.New_High, Is_64 (Low)) mod Unsigned_64'Max (Size, Page_Bytes) = 0,
             What & ": aligned to its size");
      Check ((for all X of T => not PL.Overlaps (A.Where, X)), What & ": overlaps nothing taken");
      Check ((for some R of Allowed => PL.Valid (R) and then PL.Inside (A.Where, R)),
             What & ": inside devmgr's device memory");
      Check (A.Where.First >= Lowest_Placement_Page, What & ": at or above 1 MiB");
      if not Is_64 (Low) then
         Check (A.New_High = 0 and then A.Where.Past <= Low_4G_Pages, What & ": 32-bit stays below 4 GiB");
      end if;
   end Expect_Assigned;

   A : Assignment;
begin
   --  The IdeaPad's LPSS I2C BAR: 64-bit, 4 KiB, address 0 after SOD3.
   Expect_Assigned (LPSS_64, 4_096, Both, None, "LPSS 4 KiB in the high window first", A);
   Check (A.Where = (High_Window.First, High_Window.First + 1), "LPSS: lowest page of the first window");
   Check (A.New_Low = 16#4# and then A.New_High = 16#40#, "LPSS: words 0x4 / 0x40 for 0x40_0000_0000");

   --  The second controller avoids the first.
   declare
      Taken : constant PL.Span_Array := [A.Where];
      B : Assignment;
   begin
      Expect_Assigned (LPSS_64, 4_096, Both, Taken, "second LPSS beside the first", B);
      Check (B.Where = (High_Window.First + 1, High_Window.First + 2), "second LPSS: next page");
   end;

   --  A 32-bit BAR never goes to the high window, even listed first.
   Expect_Assigned (Plain_32, 16#10_0000#, Both, None, "32-bit 1 MiB", A);
   Check (PL.Inside (A.Where, Low_Window), "32-bit: inside the low window");
   --  A window straddling 4 GiB is clipped for a 32-bit BAR.
   declare
      Straddle : constant PL.Span_Array := [(4 * G - 4, 4 * G + 100)];
      B : Assignment;
   begin
      Expect_Assigned (Plain_32, 4 * Page_Bytes, Straddle, None, "32-bit in a straddling window", B);
      Check (B.Where = (4 * G - 4, 4 * G), "straddle: the last 4 pages below 4 GiB");
      Assign (Plain_32, 0, 8 * Page_Bytes, Straddle, Everything, None, B);
      Check (B.Result = No_Room, "straddle: 8 pages don't fit below 4 GiB");
      Assign (LPSS_64, 0, 8 * Page_Bytes, Straddle, Everything, None, B);
      Check (B.Result = Assigned and then B.Where.First = 4 * G - 8 + 8, "64-bit may cross 4 GiB, aligned to 8 pages");
   end;

   --  Prefetchable flag kept; sizes below a page take one page.
   Expect_Assigned (Prefetch_64, 256, Both, None, "prefetchable 256 B", A);
   Check ((A.New_Low and 16#8#) /= 0, "prefetchable flag kept");
   Check (A.Where.Past - A.Where.First = 1, "256 B takes one page");

   --  Large BAR alignment: 256 MiB aligned to 256 MiB.
   Expect_Assigned (LPSS_64, 2 ** 28, Both, [(High_Window.First, High_Window.First + 1)], "256 MiB after a taken page", A);
   Check (A.Where.First mod (2 ** 16) = 0 and then A.Where.First > High_Window.First, "256 MiB: next 256 MiB boundary");

   --  The IdeaPad Slim 3i (image 30b): acpi.svc publishes the host bridge's
   --  _CRS memory windows 0xA0000..0xBFFFF (legacy VGA), 0x50400000..
   --  0xBFFFFFFF and 0x40_0000_0000..0x7F_FFFF_FFFF; the kernel grants devmgr
   --  device memory that leaves out everything below 1 MiB. The LPSS I2C
   --  BAR (64-bit, non-prefetchable, 4 KiB) was placed at 0xA0000, outside
   --  devmgr's device memory, and both controllers stayed unstarted.
   declare
      function Pages_Of (First, Last_Byte : Unsigned_64) return PL.Span is
        ((First / Page_Bytes, (Last_Byte + 1) / Page_Bytes));
      VGA : constant PL.Span := Pages_Of (16#A_0000#, 16#B_FFFF#);
      Window_32 : constant PL.Span := Pages_Of (16#5040_0000#, 16#BFFF_FFFF#);
      Window_64 : constant PL.Span := Pages_Of (16#40_0000_0000#, 16#7F_FFFF_FFFF#);
      --  Windows_For's order for a non-prefetchable BAR: below 4 GiB first.
      Laptop_Windows : constant PL.Span_Array := [VGA, Window_32, Window_64];
      --  The kernel's devmgr device roots on that boot (log-kernel.txt).
      Laptop_Memory : constant PL.Span_Array :=
        [Pages_Of (16#3A3C_9000#, 16#3ACC_8FFF#), Pages_Of (16#3CAD_F000#, 16#3EB7_EFFF#),
         Pages_Of (16#41C0_0000#, 16#FEBF_FFFF#), Pages_Of (16#FEC0_1000#, 16#FECF_FFFF#),
         Pages_Of (16#FED0_1000#, 16#FED8_FFFF#), Pages_Of (16#FED9_2000#, 16#FEDF_FFFF#),
         Pages_Of (16#FEF0_0000#, 16#FFFF_FFFF#), Pages_Of (16#2_AFC0_0000#, 16#7FFF_FFFF_FFFF#)];
      First_I2C, Second_I2C : Assignment;
   begin
      Expect_Assigned (LPSS_64, 4_096, Laptop_Windows, None, "IdeaPad I2C 00:15.0", First_I2C,
                       Allowed => Laptop_Memory);
      Check (not PL.Overlaps (First_I2C.Where, VGA), "IdeaPad: not in the VGA hole");
      Check (PL.Inside (First_I2C.Where, Window_32), "IdeaPad: in the 32-bit window (non-prefetchable)");
      Check (First_I2C.Where.First * Page_Bytes = 16#5040_0000#, "IdeaPad: at 0x50400000");
      Check (First_I2C.New_Low = 16#5040_0004# and then First_I2C.New_High = 0, "IdeaPad: words 0x50400004 / 0");
      Expect_Assigned (LPSS_64, 4_096, Laptop_Windows, [First_I2C.Where], "IdeaPad I2C 00:15.1", Second_I2C,
                       Allowed => Laptop_Memory);
      Check (Second_I2C.Where.First * Page_Bytes = 16#5040_1000#, "IdeaPad: second at 0x50401000");
      --  The 1 MiB floor alone keeps it out of the VGA hole even when device
      --  memory would cover it.
      Expect_Assigned (LPSS_64, 4_096, Laptop_Windows, None, "IdeaPad, all memory allowed", A);
      Check (A.Where.First * Page_Bytes = 16#5040_0000#, "IdeaPad, all memory allowed: VGA hole skipped");
      --  A prefetchable 64-bit BAR still takes the high window when it is
      --  listed first (Windows_For's High_First).
      Expect_Assigned (Prefetch_64, 4_096, [Window_64, Window_32, VGA], None, "IdeaPad prefetchable", A,
                       Allowed => Laptop_Memory);
      Check (A.Where.First * Page_Bytes = 16#40_0000_0000#, "IdeaPad prefetchable: high window");
      --  The first window wholly excluded, then the second partly covered:
      --  the place is the first page of the covered part.
      Expect_Assigned (LPSS_64, 4_096, Laptop_Windows, None, "32-bit window partly covered", A,
                       Allowed => [Pages_Of (16#8000_0000#, 16#FFFF_FFFF#)]);
      Check (A.Where.First * Page_Bytes = 16#8000_0000#, "partly covered: first covered page");
      --  Device memory only above 4 GiB: a 64-bit BAR goes high, a 32-bit
      --  BAR is refused.
      Expect_Assigned (LPSS_64, 4_096, Laptop_Windows, None, "only high memory allowed", A,
                       Allowed => [Pages_Of (16#2_AFC0_0000#, 16#7FFF_FFFF_FFFF#)]);
      Check (A.Where.First * Page_Bytes = 16#40_0000_0000#, "only high memory: the high window");
      Assign (Plain_32, 0, 4_096, Laptop_Windows, [Pages_Of (16#2_AFC0_0000#, 16#7FFF_FFFF_FFFF#)], None, A);
      Check (A.Result = No_Room, "32-bit BAR with only high device memory: No_Room");
      --  Device memory nowhere in a window: a typed refusal, nothing written.
      Assign (LPSS_64, 0, 4_096, Laptop_Windows, [Pages_Of (16#3A3C_9000#, 16#3ACC_8FFF#)], None, A);
      Check (A.Result = No_Room and then A.New_Low = 0 and then A.New_High = 0,
             "no device memory in any window: No_Room");
      --  Only the VGA hole: below 1 MiB, never placed.
      Assign (LPSS_64, 0, 4_096, [VGA], Everything, None, A);
      Check (A.Result = No_Room, "VGA hole alone: No_Room");
      Assign (Plain_32, 0, 4_096, [(0, 16#200#)], Everything, None, A);
      Check (A.Result = Assigned and then A.Where.First = Lowest_Placement_Page,
             "window from 0: placed at 1 MiB");
   end;

   --  Refusals.
   Assign (16#1#, 0, 4_096, Both, Everything, None, A);
   Check (A.Result = Not_Memory, "I/O BAR refused");
   Assign (LPSS_64, 0, 3 * Page_Bytes, Both, Everything, None, A);
   Check (A.Result = Invalid_Size, "non-power-of-two size refused");
   Assign (16#FE00_0004#, 0, 4_096, Both, Everything, None, A);
   Check (A.Result = Already_Assigned, "assigned 64-bit BAR left alone");
   Assign (LPSS_64, 16#1#, 4_096, Both, Everything, None, A);
   Check (A.Result = Already_Assigned, "high word set: already assigned");
   Assign (LPSS_64, 0, 4_096, None, Everything, None, A);
   Check (A.Result = No_Room, "no windows: no room");
   Assign (LPSS_64, 0, 4_096, Both, Everything, [High_Window, Low_Window], A);
   Check (A.Result = No_Room, "everything taken: no room");

   --  Exhaustive small sweep: every size 1..64 pages (powers of two), every
   --  taken prefix of a small window, both widths; decode and contract hold.
   for Shift in 0 .. 6 loop
      for Prefix in PL.Page range 0 .. 70 loop
         for Wide in Boolean loop
            declare
               W : constant PL.Span_Array := [(G, G + 64)];
               T : constant PL.Span_Array := [(G, G + Prefix)];
               Size : constant BAR_Bytes := Unsigned_64 (2 ** Shift) * Page_Bytes;
               Low : constant Unsigned_32 := (if Wide then LPSS_64 else Plain_32);
               B : Assignment;
               Pages : constant PL.Page := 2 ** Shift;
               Expected_First : constant PL.Page := (G + Prefix + Pages - 1) / Pages * Pages;
            begin
               Assign (Low, 0, Size, W, Everything, T, B);
               if Expected_First + Pages <= G + 64 then
                  Check (B.Result = Assigned and then B.Where.First = Expected_First
                         and then Decoded (B.New_Low, B.New_High, Wide) = Expected_First * Page_Bytes,
                         "sweep" & Shift'Image & Prefix'Image & Wide'Image);
               else
                  Check (B.Result = No_Room, "sweep full" & Shift'Image & Prefix'Image & Wide'Image);
               end if;
            end;
         end loop;
      end loop;
   end loop;

   Ada.Text_IO.Put_Line ("PCI-BAR-ASSIGNMENT:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures /= 0 then
      raise Program_Error;
   end if;
end Main;
