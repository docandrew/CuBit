pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Host_Bridge_Wire;
with PCI_BAR_Placement;
with Host_Bridge_Rows; use Host_Bridge_Rows;
--  PCI-BAR-001: which of a host bridge's _CRS rows are memory windows, and
--  which buses it decodes, on a root shaped like Alder Lake's PC00._CRS.
procedure Main is
   package WI renames ACPI_Host_Bridge_Wire;
   use type PCI_BAR_Placement.Span;
   Checks, Failures : Natural := 0;
   procedure Check (Ok : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Ok then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("HOST-BRIDGE-ROWS: FAIL " & What);
      end if;
   end Check;

   All_Claims : constant WI.Word := WI.Has_General or WI.Has_Minimum or WI.Has_Maximum or WI.Has_Length;
   function Row (Form, Kind, Flags, Min, Max : WI.Word; Len : WI.Word := 0;
                 Cache : WI.Word := 0; Translation : WI.Word := 0; Claims : WI.Word := All_Claims)
                 return WI.Row is
     ([WI.Form => Form, WI.Kind => Kind, WI.General_Flags => Flags, WI.Minimum => Min, WI.Maximum => Max,
       WI.Length => (if Len = 0 then Max - Min + 1 else Len), WI.Cache => Cache, WI.Translation => Translation,
       WI.Claims => Claims or (if Cache /= 0 then WI.Has_Cache else 0)
                           or (if Translation /= 0 then WI.Has_Translation else 0),
       others => 0]);
   Producer : constant WI.Word := 0;
   Consumer : constant WI.Word := 1;

   CRS : constant WI.Rows :=
     [Row (WI.Word_Address, WI.Bus_Kind, Producer, 0, 16#FF#),                        --  buses 0..255
      Row (WI.Word_Address, WI.IO_Kind, Producer, 0, 16#CF7#),                        --  I/O window
      Row (WI.DWord_Address, WI.Memory_Kind, Producer, 16#A_0000#, 16#B_FFFF#, Cache => 1),   --  legacy VGA
      Row (WI.DWord_Address, WI.Memory_Kind, Producer, 16#8000_0000#, 16#DFFF_FFFF#, Cache => 0),  --  M32
      Row (WI.QWord_Address, WI.Memory_Kind, Producer, 16#40_0000_0000#, 16#7F_FFFF_FFFF#, Cache => 3), --  M64
      Row (WI.Fixed_Memory_32, WI.Memory_Kind, Producer, 16#FED0_0000#, 16#FED0_3FFF#), --  own registers
      Row (WI.DWord_Address, WI.Memory_Kind, Consumer, 16#E000_0000#, 16#EFFF_FFFF#),   --  consumer only
      Row (WI.QWord_Address, WI.Memory_Kind, Producer, 16#1_0000_0000#, 16#1_FFFF_FFFF#, Translation => 16#1000#)];

   Spans : PCI_BAR_Placement.Span_Array (1 .. CRS'Length);
   Pre : Flags (1 .. CRS'Length);
   Count : Natural;
   First, Last : Unsigned_8;
begin
   Windows (CRS, Spans, Pre, Count);
   Check (Count = 3, "three memory windows (legacy, M32, M64)");
   Check (Spans (1) = (16#A0#, 16#C0#), "legacy VGA window pages");
   Check (Spans (2) = (16#8_0000#, 16#E_0000#), "M32 0x8000_0000..0xDFFF_FFFF");
   Check (Spans (3) = (16#400_0000#, 16#800_0000#), "M64 0x40_0000_0000..0x7F_FFFF_FFFF");
   Check (not Pre (1) and then not Pre (2) and then Pre (3), "only M64 (cache 3) is prefetchable");

   Buses (CRS, True, 0, First, Last);
   Check (First = 0 and then Last = 255, "bus range from the bus descriptor");
   Buses (CRS (2 .. 5), True, 3, First, Last);
   Check (First = 3 and then Last = 255, "no bus descriptor: base bus .. 255");
   Buses (CRS (2 .. 5), False, 0, First, Last);
   Check (First = 0 and then Last = 255, "no bus descriptor, no base bus: 0 .. 255");

   --  Rounding inward, Length shortening, empty and edge windows.
   declare
      Odd : constant WI.Rows :=
        [Row (WI.DWord_Address, WI.Memory_Kind, Producer, 16#1001#, 16#4FFE#),
         Row (WI.DWord_Address, WI.Memory_Kind, Producer, 16#1_0000#, 16#1_FFFF#, Len => 16#2000#),
         Row (WI.DWord_Address, WI.Memory_Kind, Producer, 16#2_0010#, 16#2_0FF0#),
         Row (WI.QWord_Address, WI.Memory_Kind, Producer, 16#FFFF_FFFF_FFFF_F000#, WI.Word'Last),
         Row (WI.DWord_Address, WI.Memory_Kind, Producer, 16#3_0000#, 16#3_FFFF#,
              Claims => WI.Has_General or WI.Has_Minimum)];
      S : PCI_BAR_Placement.Span_Array (1 .. Odd'Length);
      P : Flags (1 .. Odd'Length);
      N : Natural;
   begin
      Windows (Odd, S, P, N);
      Check (N = 2, "two usable of five odd windows");
      Check (S (1) = (2, 4), "unaligned bounds round inward to whole pages");
      Check (S (2) = (16#10#, 16#12#), "Length shortens the window");
   end;

   --  A zero-length descriptor is never a window (found by the ACPI agent's
   --  review: QWord memory, fixed bounds, Length 0).
   declare
      Zero : constant WI.Rows :=
        [[WI.Form => WI.QWord_Address, WI.Kind => WI.Memory_Kind, WI.General_Flags => 12,
          WI.Minimum => 16#8000_0000#, WI.Maximum => 16#8000_FFFF#, WI.Length => 0,
          WI.Claims => All_Claims, others => 0],
         Row (WI.QWord_Address, WI.Memory_Kind, Producer, 16#8000_0000#, 16#8000_FFFF#)];
      S : PCI_BAR_Placement.Span_Array (1 .. Zero'Length);
      P : Flags (1 .. Zero'Length);
      N : Natural;
   begin
      Windows (Zero, S, P, N);
      Check (N = 1 and then S (1) = (16#8_0000#, 16#8_0010#), "zero-length descriptor is not a window; its positive twin is");
   end;

   Ada.Text_IO.Put_Line ("HOST-BRIDGE-ROWS:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures /= 0 then
      raise Program_Error;
   end if;
end Main;
