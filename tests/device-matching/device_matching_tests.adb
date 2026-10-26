------------------------------------------------------------------------------
--  HW-002 (docs/device-matching.md): the image's real driver index, built
--  by ccl-device-index from the drivers' own manifests (run.sh), decoded
--  and bound against two machines' PCI inventories: the IdeaPad Slim 3i
--  (i3-N350, lspci -nn of 2026-10-23) and QEMU q35 as the headless tests
--  run it. Then the precedence rules on hand-made tables, and every
--  truncation and single-bit corruption of the index and of a section
--  (decoders run with checks on: they must never fail one).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Device_Sections;
with CCL.Device_Index;
with CCL.Resource_Sections;
with Devmgr_Windows;

procedure Device_Matching_Tests is
   package DS renames CCL.Device_Sections;
   package DI renames CCL.Device_Index;
   use type DI.Binding_Kind;
   use type DI.Decode_Status;
   use type DS.Decode_Status;
   use type DI.Byte_Array;
   use type DS.Match_Table;

   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Put_Line ("DEVICE-MATCHING: FAIL " & What);
         raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Check;

   function Load (Path : String) return DI.Byte_Array is
      use Ada.Streams.Stream_IO;
      F : Ada.Streams.Stream_IO.File_Type;
      Size : constant Natural := Natural (Ada.Directories.Size (Path));
      Raw : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Size));
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Open (F, Ada.Streams.Stream_IO.In_File, Path);
      Read (F, Raw, Last);
      Close (F);
      return [for I in 1 .. Size => Unsigned_8 (Raw (Ada.Streams.Stream_Element_Offset (I)))];
   end Load;

   Data : constant DI.Byte_Array := Load (Ada.Command_Line.Argument (1));
   Index : DI.Index;
   Status : DI.Decode_Status;

   type Expected_Binding is record
      Vendor, Device : Unsigned_16;
      Class, Subclass, Program : Unsigned_8;
      Driver : String (1 .. 16);   --  blank-padded; "" (all blanks) for none
   end record;
   function Pad (S : String) return String is
     (S & [1 .. 16 - S'Length => ' ']);

   function Name_Of (D : DI.Driver_Number) return String is
     (Index.Drivers (D).Name.Text (1 .. Index.Drivers (D).Name.Length));

   procedure Expect (Machine : String; Item : Expected_Binding) is
      B : DI.Binding;
      F : constant DS.PCI_Function :=
        (Vendor => Item.Vendor, Device => Item.Device, Class => Item.Class,
         Subclass => Item.Subclass, Program => Item.Program);
      Want : constant String := Item.Driver;
      Label : constant String := Machine & Unsigned_16'Image (Item.Vendor) & ":" &
        Unsigned_16'Image (Item.Device);
   begin
      DI.Bind (Index, F, B);
      if Want = Pad ("") then
         Check (B.Kind = DI.Unbound, Label & " matches no driver");
      else
         Check (B.Kind = DI.Bound and then Pad (Name_Of (B.Driver)) = Want,
                Label & " binds " & Want);
      end if;
   end Expect;

   --  lspci -nn, IdeaPad Slim 3i (Core i3-N350, Twin Lake / ADL-N).
   Laptop : constant array (Positive range <>) of Expected_Binding :=
     [(16#8086#, 16#461C#, 16#06#, 16#00#, 16#00#, Pad ("")),               --  host bridge
      (16#8086#, 16#46D4#, 16#03#, 16#00#, 16#00#, Pad ("intel-gpu.drv")),  --  graphics
      (16#8086#, 16#464E#, 16#0C#, 16#03#, 16#30#, Pad ("xhci.drv")),       --  TCSS xHCI
      (16#8086#, 16#54ED#, 16#0C#, 16#03#, 16#30#, Pad ("xhci.drv")),       --  PCH xHCI
      (16#8086#, 16#54FF#, 16#01#, 16#09#, 16#01#, Pad ("ufs.drv")),        --  UFS
      (16#8086#, 16#54E8#, 16#0C#, 16#80#, 16#00#, Pad ("")),               --  I2C 0
      (16#8086#, 16#54E9#, 16#0C#, 16#80#, 16#00#, Pad ("")),               --  I2C 1
      (16#8086#, 16#54FC#, 16#07#, 16#00#, 16#00#, Pad ("")),               --  ISH
      (16#8086#, 16#54C8#, 16#04#, 16#03#, 16#00#, Pad ("hda.drv")),        --  HD Audio
      (16#8086#, 16#54C8#, 16#04#, 16#03#, 16#80#, Pad ("hda.drv")),        --  HD Audio, DSP mode
      (16#8086#, 16#54E0#, 16#07#, 16#80#, 16#00#, Pad ("")),               --  HECI
      (16#10EC#, 16#B852#, 16#02#, 16#80#, 16#00#, Pad ("")),               --  RTL8852BE (no driver yet)
      (16#10EC#, 16#522A#, 16#FF#, 16#00#, 16#00#, Pad (""))];              --  card reader

   --  QEMU q35 as tests/headless/run.sh starts it.
   QEMU : constant array (Positive range <>) of Expected_Binding :=
     [(16#8086#, 16#29C0#, 16#06#, 16#00#, 16#00#, Pad ("")),               --  host bridge
      (16#1234#, 16#1111#, 16#03#, 16#00#, 16#00#, Pad ("")),               --  std VGA: not intel-gpu's
      (16#1AF4#, 16#1050#, 16#03#, 16#00#, 16#00#, Pad ("virtio-gpu.drv")), --  virtio-vga
      (16#1AF4#, 16#1000#, 16#02#, 16#00#, 16#00#, Pad ("virtio-net.drv")),
      (16#1AF4#, 16#1052#, 16#09#, 16#80#, 16#00#, Pad ("virtio-input.drv")),
      (16#1B36#, 16#0010#, 16#01#, 16#08#, 16#02#, Pad ("nvme.drv")),
      (16#8086#, 16#293E#, 16#04#, 16#03#, 16#00#, Pad ("hda.drv")),
      (16#1B36#, 16#000D#, 16#0C#, 16#03#, 16#30#, Pad ("xhci.drv")),       --  qemu-xhci
      (16#8086#, 16#2922#, 16#01#, 16#06#, 16#01#, Pad ("")),               --  ICH9 AHCI (no driver)
      (16#1B36#, 16#0013#, 16#01#, 16#09#, 16#01#, Pad ("ufs.drv"))];       --  QEMU UFS (tests/ufs)

   function PCI_IDs (Vendor, First, Last : Unsigned_16) return DS.Device_Match is
     ((Kind => DS.PCI_IDs, Vendor => Vendor, First => First, Last => Last, others => <>));
   function PCI_Class (Class, Subclass, Program : Unsigned_8) return DS.Device_Match is
     ((Kind => DS.PCI_Class, Class => Class, Subclass => Subclass, Program => Program, others => <>));
   function One (M : DS.Device_Match) return DS.Match_Table is
     ((Count => 1, Entries => [1 => M, others => <>]));
   function Named (S : String) return DI.Driver_Name is
     ((Length => S'Length, Text => S & [1 .. DI.NAME_BYTES - S'Length => ASCII.NUL]));
   B : DI.Binding;
   Found : Boolean;
   Left, Right : DI.Driver_Count;
   Left_Entry, Right_Entry : DS.Entry_Count;
begin
   DI.Decode (Data, Index, Status);
   Check (Status = DI.Decoded, "the image's index decodes");
   Check (DI.Find (Index, "intel-gpu.drv") /= 0 and then DI.Find (Index, "xhci.drv") /= 0
          and then DI.Find (Index, "ps2.drv") /= 0, "every driver is indexed");
   Check (DI.Find (Index, "mixer.svc") = 0, "a service with no devices is not");
   for Item of Laptop loop
      Expect ("IdeaPad", Item);
   end loop;
   for Item of QEMU loop
      Expect ("QEMU", Item);
   end loop;
   DI.First_Ambiguity (Index, Found, Left, Right, Left_Entry, Right_Entry);
   Check (not Found, "the image's drivers are unambiguous");

   --  The generic launch's device-memory windows never exceed the BAR: the
   --  UFS driver once asked 16 KiB of the IdeaPad's 4 KiB BAR 0.
   declare
      use type Devmgr_Windows.Window_Grant;
      BAR_4K : constant := 4096;
      Asked_16K : constant := 16_384;
   begin
      Check (Devmgr_Windows.Window (Asked_16K, BAR_4K) = (Bytes => BAR_4K, Clamped => True),
             "an oversize request is clamped to the BAR");
      Check (Devmgr_Windows.Window (BAR_4K, BAR_4K) = (Bytes => BAR_4K, Clamped => False),
             "a request of the whole BAR is granted");
      Check (Devmgr_Windows.Window (256, BAR_4K) = (Bytes => 256, Clamped => False),
             "a smaller request is granted as asked");
      Check (Devmgr_Windows.Window (Asked_16K, 0).Bytes = 0,
             "no window from an unusable BAR");
   end;

   --  ufs.drv asks no more device memory than 54FF's BAR 0 decodes
   --  (its .cubit.resources section, extracted by run.sh).
   declare
      package RS renames CCL.Resource_Sections;
      use type RS.Resource_Kind;
      use type RS.Decode_Status;
      IdeaPad_UFS_BAR_Bytes : constant := 4096;
      Raw : constant DI.Byte_Array := Load (Ada.Command_Line.Argument (2));
      Section : constant RS.Byte_Array (1 .. Raw'Length) :=
        [for I in 1 .. Raw'Length => Raw (Raw'First + I - 1)];
      Plan : RS.Section_Plan;
      Section_Status : RS.Decode_Status;
      Windows : Natural := 0;
   begin
      RS.Decode (Section, Plan, Section_Status);
      Check (Section_Status = RS.Decoded, "ufs.drv's resource section decodes");
      for I in 1 .. Plan.Count loop
         if Plan.Entries (I).Kind = RS.Device_Memory then
            Windows := Windows + 1;
            Check (Plan.Entries (I).Amount <= IdeaPad_UFS_BAR_Bytes,
                   "ufs.drv's register window fits 54FF's 4 KiB BAR");
         end if;
      end loop;
      Check (Windows = 1, "ufs.drv asks for one register window");
   end;

   --  Precedence on hand-made tables: an ID range beats a class, a
   --  narrower range a wider one, equals are ambiguous.
   declare
      Class_Driver : constant DI.Driver := (Named ("vga.drv"), One (PCI_Class (3, 0, 0)));
      Family : constant DI.Driver := (Named ("family.drv"), One (PCI_IDs (16#8086#, 16#46D0#, 16#46D4#)));
      Exact : constant DI.Driver := (Named ("exact.drv"), One (PCI_IDs (16#8086#, 16#46D4#, 16#46D4#)));
      Twin : constant DI.Driver := (Named ("twin.drv"), One (PCI_IDs (16#8086#, 16#46D2#, 16#46D6#)));
      F : constant DS.PCI_Function := (16#8086#, 16#46D4#, 3, 0, 0);
      I : DI.Index := (Count => 2, Drivers => [1 => Class_Driver, 2 => Family, others => <>]);
   begin
      DI.Bind (I, F, B);
      Check (B.Kind = DI.Bound and then B.Driver = 2, "an ID range beats a class");
      I.Drivers (3) := Exact;
      I.Count := 3;
      DI.Bind (I, F, B);
      Check (B.Kind = DI.Bound and then B.Driver = 3, "a narrower range beats a wider one");
      I.Drivers (3) := Twin;
      DI.Bind (I, F, B);
      Check (B.Kind = DI.Ambiguous and then B.Driver = 2 and then B.Rival = 3,
             "equally specific drivers are ambiguous");
      DI.First_Ambiguity (I, Found, Left, Right, Left_Entry, Right_Entry);
      Check (Found and then Left = 2 and then Right = 3, "the image check names them");
      DI.Bind (I, (16#8086#, 16#46D2#, 3, 0, 0), B);
      Check (B.Kind = DI.Ambiguous, "46D2 lies in both ranges");
      DI.Bind (I, (16#1234#, 16#1111#, 3, 0, 0), B);
      Check (B.Kind = DI.Bound and then B.Driver = 1, "another vendor's VGA gets the class driver");
      DI.Bind (I, (DS.NO_VENDOR, 0, 3, 0, 0), B);
      Check (B.Kind = DI.Unbound, "an empty slot binds nothing");
   end;

   --  Round trip, then robustness.
   declare
      Bytes : DI.Index_Bytes;
      Length : DI.Index_Length;
      Again : DI.Index;
   begin
      DI.Encode (Index, Bytes, Length);
      Check (Length = Data'Length and then Bytes (1 .. Length) = Data, "encode reproduces the tool's bytes");
      for Cut in 0 .. Data'Length - 1 loop
         DI.Decode (Data (1 .. Cut), Again, Status);
         Check (Status /= DI.Decoded, "a truncated index is refused");
      end loop;
      for Byte in Data'Range loop
         for Bit in 0 .. 7 loop
            declare
               Bad : DI.Byte_Array := Data;
            begin
               Bad (Byte) := Bad (Byte) xor Shift_Left (Unsigned_8'(1), Bit);
               DI.Decode (Bad, Again, Status);
               Checks := Checks + 1;
            end;
         end loop;
      end loop;
   end;
   declare
      Bytes : DS.Section_Bytes;
      Length : DS.Section_Length;
      Table, Again : DS.Match_Table;
      Section_Status : DS.Decode_Status;
   begin
      Table := (Count => 3, Entries =>
        [1 => PCI_IDs (16#10EC#, 16#B852#, 16#B852#),
         2 => (Kind => DS.ACPI_ID, ID => ['P', 'N', 'P', '0', 'C', '5', '0', ASCII.NUL],
                ID_Length => 7, others => <>),
         3 => (Kind => DS.Platform, Device => CCL.Resource_Sections.CMOS_RTC, others => <>),
         others => <>]);
      DS.Encode (Table, Bytes, Length);
      DS.Decode (Bytes (1 .. Length), Again, Section_Status);
      Check (Section_Status = DS.Decoded and then Again = Table, "a section round-trips");
      for Byte in 1 .. Length loop
         for Bit in 0 .. 7 loop
            declare
               Bad : DS.Byte_Array := Bytes (1 .. Length);
            begin
               Bad (Byte) := Bad (Byte) xor Shift_Left (Unsigned_8'(1), Bit);
               DS.Decode (Bad, Again, Section_Status);
               Check (Section_Status /= DS.Decoded or else DS.Valid_Table (Again),
                      "a corrupt section is refused or still valid");
            end;
         end loop;
      end loop;
   end;
   Put_Line ("DEVICE-MATCHING: PASS" & Natural'Image (Checks) & " checks");
end Device_Matching_Tests;
