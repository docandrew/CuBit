--  Hosted tests of the PCI capability walk (PCI_Capabilities). The walk's
--  postconditions run as assertions (-gnata) on every case, including
--  the random configuration spaces.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with PCI_Capabilities; use PCI_Capabilities;
with Config_Spaces; use Config_Spaces;

procedure Main is
   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   --  A function with a capability list starting at First.
   procedure Fresh (First : Unsigned_8) is
   begin
      Current := [others => 0];
      Current (Status_Offset) := Status_Capabilities_List;
      Current (Capabilities_Pointer_Offset) := First;
   end Fresh;

   procedure Put_Entry (At_Offset : Config_Offset; ID, Next : Unsigned_8;
                        Control : Unsigned_16 := 0) is
   begin
      Current (At_Offset) := ID;
      Current (At_Offset + 1) := Next;
      if At_Offset + 3 <= Config_Offset'Last then
         Current (At_Offset + 2) := Unsigned_8 (Control and 16#FF#);
         Current (At_Offset + 3) := Unsigned_8 (Shift_Right (Control, 8));
      end if;
   end Put_Entry;

   --  The same walk over a read that counts: a loop is detected at its
   --  first repeat, not by running out of passes.
   Reads : Natural := 0;
   function Counted_Read (Offset : Config_Offset) return Unsigned_8 is
   begin
      Reads := Reads + 1;
      return Current (Offset);
   end Counted_Read;
   package Counted is new PCI_Capabilities.Walks (Counted_Read);

   F : Interrupt_Facts;
   Seed : Unsigned_32 := 16#C0B1_7001#;
   function Random return Unsigned_8 is
   begin
      --  xorshift32: reproducible.
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 17);
      Seed := Seed xor Shift_Left (Seed, 5);
      return Unsigned_8 (Seed and 16#FF#);
   end Random;
begin
   --  No Capabilities List bit: nothing, even with a pointer.
   Current := [others => 0];
   Current (Capabilities_Pointer_Offset) := 16#40#;
   Put_Entry (16#40#, MSI_ID, 0);
   F := Walk.Interrupts;
   Check (F.Status = No_List and then F.MSI = No_Entry, "status bit clear");

   --  The bit set but a zero pointer.
   Fresh (0);
   Check (Walk.Interrupts.Status = No_List, "zero first pointer");

   --  MSI (one vector), a vendor entry, MSI-X (16 entries).
   Fresh (16#50#);
   Put_Entry (16#50#, MSI_ID, 16#60#, Control => 0);
   Put_Entry (16#60#, Vendor_Specific_ID, 16#70#);
   Put_Entry (16#70#, MSIX_ID, 0, Control => 16#000F#);
   F := Walk.Interrupts;
   Check (F.Status = Complete, "complete list");
   Check (F.MSI = 16#50# and then F.MSI_Vectors = 1, "MSI one vector");
   Check (F.MSIX = 16#70# and then F.MSIX_Vectors = 16, "MSI-X sixteen");
   Check (Walk.Find (Vendor_Specific_ID) = 16#60#, "find vendor entry");
   Check (Walk.Find (16#10#) = No_Entry, "find absent ID");

   --  MSI-X table size field: 2048 entries at most, enable bits ignored.
   Fresh (16#40#);
   Put_Entry (16#40#, MSIX_ID, 0, Control => 16#C7FF#);
   Check (Walk.Interrupts.MSIX_Vectors = 2048, "MSI-X 2048");

   --  Multiple Message Capable: 2**n for 0 .. 5, one vector when reserved.
   for N in Unsigned_16 range 0 .. 7 loop
      Fresh (16#40#);
      Put_Entry (16#40#, MSI_ID, 0, Control => Shift_Left (N, 1) or 16#0081#);
      Check (Walk.Interrupts.MSI_Vectors = (if N <= 5 then 2 ** Natural (N) else 1),
             "MSI multiple message capable" & N'Image);
   end loop;

   --  Reserved pointer bits are masked: 0x53 is entry 0x50.
   Fresh (16#53#);
   Put_Entry (16#50#, MSI_ID, 16#43#);
   Put_Entry (16#40#, MSIX_ID, 16#02#);
   F := Walk.Interrupts;
   Check (F.MSI = 16#50# and then F.MSIX = 16#40# and then F.Status = Complete,
          "reserved pointer bits masked");

   --  Looping lists: two entries, and an entry pointing at itself.
   Fresh (16#40#);
   Put_Entry (16#40#, MSI_ID, 16#50#);
   Put_Entry (16#50#, 16#01#, 16#40#);
   F := Walk.Interrupts;
   Check (F.Status = Looped and then F.MSI = 16#40#, "two-entry loop keeps MSI");
   Check (Walk.Find (MSIX_ID) = No_Entry, "find in loop terminates");
   Reads := 0;
   F := Counted.Interrupts;
   --  Status, pointer, ID and next of 0x40 and 0x50 and MSI's control
   --  word, plus the contracts' reads (-gnata): not 49 passes' worth.
   Check (F.Status = Looped and then Reads <= 40,
          "loop detected at its first repeat (reads" & Reads'Image & ")");
   Fresh (16#80#);
   Put_Entry (16#80#, 16#01#, 16#80#);
   Check (Walk.Interrupts.Status = Looped, "self loop");

   --  An absent function reads all ones: a loop at 0xFC, nothing found.
   Current := [others => 16#FF#];
   F := Walk.Interrupts;
   Check (F.Status = Looped and then F.MSI = No_Entry and then F.MSIX = No_Entry,
          "all ones");

   --  Truncated: a pointer into the standard header.
   Fresh (16#40#);
   Put_Entry (16#40#, MSI_ID, 16#10#);
   F := Walk.Interrupts;
   Check (F.Status = Truncated and then F.MSI = 16#40#, "pointer into header");
   Fresh (16#3C#);
   Check (Walk.Interrupts.Status = Truncated, "first pointer into header");

   --  Truncated: structures past the end of configuration space.
   Fresh (16#F8#);
   Put_Entry (16#F8#, MSIX_ID, 0, Control => 3);
   F := Walk.Interrupts;
   Check (F.Status = Truncated and then F.MSIX = No_Entry, "MSI-X at 0xF8 truncated");
   Fresh (16#F4#);
   Put_Entry (16#F4#, MSIX_ID, 0, Control => 3);
   F := Walk.Interrupts;
   Check (F.Status = Complete and then F.MSIX = 16#F4# and then F.MSIX_Vectors = 4,
          "MSI-X at 0xF4 fits");
   Fresh (16#F8#);
   Put_Entry (16#F8#, MSI_ID, 0);
   Check (Walk.Interrupts.Status = Truncated and then Walk.Interrupts.MSI = No_Entry,
          "MSI at 0xF8 truncated");
   Fresh (16#F4#);
   Put_Entry (16#F4#, MSI_ID, 0);
   Check (Walk.Interrupts.MSI = 16#F4#, "MSI at 0xF4 fits");
   Fresh (16#FC#);
   Put_Entry (16#FC#, Vendor_Specific_ID, 0);
   Check (Walk.Interrupts.Status = Complete and then Walk.Find (Vendor_Specific_ID) = 16#FC#,
          "entry at 0xFC");

   --  The longest list: every position once, then the end, or a loop.
   Fresh (16#40#);
   for P in 0 .. Maximum_Entries - 1 loop
      Put_Entry (First_Entry + P * Entry_Stride, 16#01#,
                 (if P = Maximum_Entries - 1 then 0
                  else Unsigned_8 (First_Entry + (P + 1) * Entry_Stride)));
   end loop;
   Check (Walk.Interrupts.Status = Complete, "48-entry list complete");
   Current (Last_Entry + 1) := 16#40#;
   Check (Walk.Interrupts.Status = Looped, "48-entry list looped");

   --  First of each kind wins; a second MSI does not replace it.
   Fresh (16#40#);
   Put_Entry (16#40#, MSI_ID, 16#48#, Control => 2);
   Put_Entry (16#48#, MSI_ID, 0, Control => 10);
   Check (Walk.Interrupts.MSI = 16#40# and then Walk.Interrupts.MSI_Vectors = 2,
          "first MSI entry");

   --  Random spaces: the walk always ends, and its postconditions hold.
   for Trial in 1 .. 200_000 loop
      for B of Current loop
         B := Random;
      end loop;
      Current (Status_Offset) := Current (Status_Offset) or Status_Capabilities_List;
      F := Walk.Interrupts;
      Checks := Checks + 1;
      if F.Status = No_List and then Current (Capabilities_Pointer_Offset) >= 4 then
         Failures := Failures + 1;
         Put_Line ("FAIL: random space reported no list");
      end if;
   end loop;

   if Failures = 0 then
      Put_Line ("pci-capabilities: PASS" & Checks'Image & " checks");
   else
      Put_Line ("pci-capabilities: FAIL" & Failures'Image & " of" & Checks'Image);
      raise Program_Error;
   end if;
end Main;
