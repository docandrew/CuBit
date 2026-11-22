--  Hosted tests of Devmgr_ACPI_Windows: an ACPI-matched driver's K-th
--  device-memory request gets exactly the admitted span its index names
--  (in _CRS order), at most its max_bytes; any request that cannot be met
--  refuses the whole launch, naming the request. Run by run.sh; exits
--  nonzero on a failure.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with CCL.Resource_Sections;
with Devmgr_ACPI_Windows;
with Devmgr_Phase_Declarations;
with CuBit.Consumer_Initialization_Exchange;
with CuBit.HID_Offer_Exchange;
with HID_Offer_Payload;
with I2C_Channel_Badges;
with GPIO_Channel_Badges;
with Devmgr_HID_Offers;

procedure Main is
   package RS renames CCL.Resource_Sections;
   package AW renames Devmgr_ACPI_Windows;
   use type AW.Outcome;
   use type AW.Window;
   Checks, Failed : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failed := Failed + 1;
         Put_Line ("FAIL " & Name);
      end if;
   end Check;

   KiB_64 : constant := 16#1_0000#;
   Page   : constant := RS.PAGE_BYTES;
   --  The IdeaPad's GPI0 _CRS: four 64 KiB Memory32Fixed, communities
   --  0, 1, 4, 5 (SBRG + 0x6E/6D/6A/69 0000), all read-write.
   IdeaPad : constant AW.Admitted :=
     (Count => 4,
      Items => [1 => (16#FD6E_0000#, KiB_64, True), 2 => (16#FD6D_0000#, KiB_64, True),
                3 => (16#FD6A_0000#, KiB_64, True), 4 => (16#FD69_0000#, KiB_64, True),
                others => <>]);
   ACPI : constant RS.Basis_Info := (Kind => RS.ACPI_Basis, Values => [others => 0]);

   function Memory (Index : Unsigned_32; Bytes : Unsigned_64;
                    Rights : RS.Rights_Kind := RS.Read_Write) return RS.Resource is
     ((Kind => RS.Device_Memory, Rights => Rights, Index => Index, Amount => Bytes, Extra => 0));

   function Plan_Of (Items : RS.Resource_Array; Count : RS.Entry_Count;
                     Basis : RS.Basis_Info := ACPI) return RS.Section_Plan is
     ((Basis => Basis, Count => Count, Entries => Items));

   Entries : RS.Resource_Array := [others => (others => <>)];
   P : AW.Plan_Result;
begin
   --  The gpio manifest's shape: four communities, max_bytes above the span.
   for K in 1 .. 4 loop
      Entries (K) := Memory (Unsigned_32 (K - 1), 4 * 1024 * 1024);
   end loop;
   P := AW.Plan (IdeaPad, Plan_Of (Entries, 4));
   Check (P.Status = AW.Granted and then P.Total = 4, "four requests, four windows");
   for K in 1 .. 4 loop
      Check (P.Windows (K).First = IdeaPad.Items (K).Base and then P.Windows (K).Bytes = KiB_64
             and then not P.Windows (K).Read_Only and then P.Windows (K).From_Span = K,
             "window" & K'Image & " is exactly span" & K'Image);
   end loop;

   --  Index order, not request order: request 1 names span 4, request 2 span 1;
   --  a smaller max_bytes narrows the window; read-only stays read-only.
   Entries := [1 => Memory (3, Page, RS.Read_Only), 2 => Memory (0, 2 * Page), others => <>];
   P := AW.Plan (IdeaPad, Plan_Of (Entries, 2));
   Check (P.Status = AW.Granted and then P.Total = 2
          and then P.Windows (1) = (16#FD69_0000#, Page, True, 4)
          and then P.Windows (2) = (16#FD6E_0000#, 2 * Page, False, 1),
          "the index names the span; max_bytes narrows it");

   --  Scheduling beside device memory is skipped; no device resource at
   --  all (none, or only scheduling) launches with no windows.
   Entries := [1 => (Kind => RS.Scheduling, Rights => RS.Read_Only, Index => 0, Amount => 1500,
                     Extra => 5000),
               2 => Memory (1, KiB_64), others => <>];
   P := AW.Plan (IdeaPad, Plan_Of (Entries, 2));
   Check (P.Status = AW.Granted and then P.Total = 1 and then P.Windows (1).From_Span = 2,
          "scheduling is not a window");
   P := AW.Plan (IdeaPad, Plan_Of (Entries, 1, (Kind => RS.No_Basis, Values => [others => 0])));
   Check (P.Status = AW.Granted and then P.Total = 0, "only scheduling: no windows");
   P := AW.Plan (IdeaPad, Plan_Of (Entries, 0, (Kind => RS.No_Basis, Values => [others => 0])));
   Check (P.Status = AW.Granted and then P.Total = 0, "no resources: no windows");

   --  More spans asked for than were admitted: the whole launch is refused,
   --  naming the request, even though the first request could be met.
   Entries := [1 => Memory (0, KiB_64), 2 => Memory (4, KiB_64), others => <>];
   P := AW.Plan (IdeaPad, Plan_Of (Entries, 2));
   Check (P.Status = AW.Not_Admitted and then P.Request = 2 and then P.Total = 0,
          "a fifth span of four is refused whole (request 2)");
   P := AW.Plan ((Count => 0, others => <>), Plan_Of (Entries, 1));
   Check (P.Status = AW.Not_Admitted and then P.Request = 1, "nothing admitted: refused");

   --  A read-write request for a span _CRS declares read-only.
   declare
      Read_Only_Span : AW.Admitted := IdeaPad;
   begin
      Read_Only_Span.Items (2).Writable := False;
      Entries := [1 => Memory (0, KiB_64), 2 => Memory (1, KiB_64), others => <>];
      P := AW.Plan (Read_Only_Span, Plan_Of (Entries, 2));
      Check (P.Status = AW.Span_Read_Only and then P.Request = 2 and then P.Total = 0,
             "read-write on a read-only span is refused");
      Entries (2) := Memory (1, KiB_64, RS.Read_Only);
      P := AW.Plan (Read_Only_Span, Plan_Of (Entries, 2));
      Check (P.Status = AW.Granted and then P.Windows (2).Read_Only, "read-only on it is granted");
   end;

   --  Resources relative to something else, or not device memory.
   Entries := [1 => Memory (0, KiB_64), others => <>];
   P := AW.Plan (IdeaPad, Plan_Of (Entries, 1, (Kind => RS.PCI_Basis, Values => [others => 0])));
   Check (P.Status = AW.Not_ACPI_Resources and then P.Request = 0 and then P.Total = 0,
          "PCI-relative resources refused");
   Entries := [1 => Memory (0, KiB_64),
               2 => (Kind => RS.DMA, Rights => RS.Read_Write, Index => 0, Amount => Page, Extra => 0),
               others => <>];
   P := AW.Plan (IdeaPad, Plan_Of (Entries, 2));
   Check (P.Status = AW.Not_Device_Memory and then P.Request = 2, "a DMA request refused");

   --  The parent line: one platform line at resource 0, one vector.
   declare
      function Line (Mode : RS.Interrupt_Mode; Index : Unsigned_32 := 0;
                     Vectors : Unsigned_64 := 1) return RS.Resource is
        ((Kind => RS.Interrupt, Rights => RS.Read_Only, Index => Index, Amount => Vectors,
          Extra => Unsigned_64 (RS.Interrupt_Mode'Enum_Rep (Mode))));
   begin
      Entries := [1 => Memory (0, KiB_64), 2 => Line (RS.Platform_Line), 3 => Memory (1, KiB_64),
                  others => <>];
      P := AW.Plan (IdeaPad, Plan_Of (Entries, 3));
      Check (P.Status = AW.Granted and then P.Parent_Line and then P.Total = 2
             and then P.Windows (2).First = IdeaPad.Items (2).Base,
             "the parent line is planned beside the windows");
      P := AW.Plan (IdeaPad, Plan_Of (Entries, 1));
      Check (P.Status = AW.Granted and then not P.Parent_Line, "no interrupt request: no parent line");
      Entries (3) := Line (RS.Platform_Line);
      P := AW.Plan (IdeaPad, Plan_Of (Entries, 3));
      Check (P.Status = AW.Not_Parent_Line and then P.Request = 3 and then not P.Parent_Line
             and then P.Total = 0, "a second parent line is refused");
      Entries (3) := Memory (1, KiB_64);
      Entries (2) := Line (RS.Platform_Line, Index => 1);
      P := AW.Plan (IdeaPad, Plan_Of (Entries, 3));
      Check (P.Status = AW.Not_Parent_Line and then P.Request = 2, "a platform line past resource 0 is refused");
      Entries (2) := Line (RS.Platform_Line, Vectors => 2);
      P := AW.Plan (IdeaPad, Plan_Of (Entries, 3));
      Check (P.Status = AW.Not_Parent_Line and then P.Request = 2, "two parent vectors are refused");
      Entries (2) := Line (RS.MSI);
      P := AW.Plan (IdeaPad, Plan_Of (Entries, 3));
      Check (P.Status = AW.Not_Parent_Line and then P.Request = 2, "an MSI request is refused");
      Entries (2) := Line (RS.Line);
      P := AW.Plan (IdeaPad, Plan_Of (Entries, 3));
      Check (P.Status = AW.Not_Parent_Line and then P.Request = 2, "a PCI line request is refused");
   end;

   --  Sixteen admitted spans, seventeen windows asked: too many.
   declare
      Full : AW.Admitted := (Count => AW.Max_Spans, others => <>);
   begin
      for K in AW.Span_Index loop
         Full.Items (K) := (Unsigned_64 (K) * KiB_64 * 2, KiB_64, True);
      end loop;
      Entries := [others => Memory (0, Page)];
      for K in 1 .. AW.Max_Spans loop
         Entries (K) := Memory (Unsigned_32 (K - 1), Page);
      end loop;
      P := AW.Plan (Full, Plan_Of (Entries, AW.Max_Spans));
      Check (P.Status = AW.Granted and then P.Total = AW.Max_Spans
             and then P.Windows (AW.Max_Spans).First = Full.Items (AW.Max_Spans).Base,
             "sixteen spans, sixteen windows");
      P := AW.Plan (Full, Plan_Of (Entries, AW.Max_Spans + 1));
      Check (P.Status = AW.Too_Many_Windows and then P.Request = AW.Max_Spans + 1,
             "a seventeenth window refused");
   end;

   --  Consumer_Initialization_Complete (Devmgr_Phase_Declarations): the
   --  ACPI agent's review cases. A Completed releases only the sender's
   --  records observed in exactly that epoch; epoch 0 is malformed; the
   --  first declaration per (sender, epoch) stands; only the canonical
   --  frame is decided at all.
   declare
      package PD renames Devmgr_Phase_Declarations;
      package CX renames CuBit.Consumer_Initialization_Exchange;
      use type CX.Receipt;
      use type CX.Phase_Status;
      ACPI_Svc : constant := 16#100_001E#;
      Other    : constant := 16#100_0020#;
      Epoch_A  : constant := 1;
      Epoch_B  : constant := 2;
      Completed : constant Unsigned_64 := CX.Phase_Status'Enum_Rep (CX.Completed);
      Incomplete : constant Unsigned_64 := CX.Phase_Status'Enum_Rep (CX.Incomplete);
      None : constant PD.Declaration_Array (1 .. 0) := [others => <>];
      V : PD.Verdict;
      Plain : constant PD.Word_Array (0 .. 3) := [1, Epoch_A, Completed, 0];
   begin
      --  Issue 1: admitted under epoch A, Completed for epoch B.
      V := PD.Decide (None, True, ACPI_Svc, 1, Epoch_B, Completed);
      Check (V.Answer = CX.Received and then V.Release, "Completed for epoch B is received and releases");
      Check (not PD.Releases (ACPI_Svc, Epoch_A, ACPI_Svc, Epoch_B),
             "but not a record observed in epoch A");
      V := PD.Decide ([(ACPI_Svc, Epoch_B, CX.Completed)], True, ACPI_Svc, 2, Epoch_A, Completed);
      Check (V.Answer = CX.Received and then V.Release and then PD.Releases (ACPI_Svc, Epoch_A, ACPI_Svc, Epoch_A),
             "then Completed for epoch A releases the epoch-A record");
      Check (not PD.Releases (ACPI_Svc, Epoch_A, Other, Epoch_A), "another sender releases nothing");
      --  Issue 2: epoch 0.
      V := PD.Decide (None, True, ACPI_Svc, 1, PD.No_Epoch, Completed);
      Check (V.Answer = CX.Malformed and then not V.Record_It and then not V.Release, "epoch 0 is MALFORMED");
      Check (not PD.Releases (ACPI_Svc, PD.No_Epoch, ACPI_Svc, PD.No_Epoch), "epoch 0 releases nothing");
      V := PD.Decide (None, True, ACPI_Svc, 0, Epoch_A, Completed);
      Check (V.Answer = CX.Malformed, "request id 0 is MALFORMED");
      V := PD.Decide (None, True, ACPI_Svc, 1, Epoch_A, 5);
      Check (V.Answer = CX.Malformed, "an unknown status is MALFORMED");
      V := PD.Decide (None, False, ACPI_Svc, 1, Epoch_A, Completed);
      Check (V.Answer = CX.Not_Approved and then not V.Release, "without the badge: NOT_APPROVED");
      --  First declaration per (sender, epoch).
      V := PD.Decide ([(ACPI_Svc, Epoch_A, CX.Incomplete)], True, ACPI_Svc, 3, Epoch_A, Completed);
      Check (V.Answer = CX.Conflicting and then V.Earlier = CX.Incomplete and then not V.Release
             and then not V.Record_It, "INCOMPLETE then COMPLETED is CONFLICTING");
      V := PD.Decide ([(ACPI_Svc, Epoch_A, CX.Completed)], True, ACPI_Svc, 4, Epoch_A, Completed);
      Check (V.Answer = CX.Received and then not V.Release and then not V.Record_It,
             "COMPLETED again for the epoch changes nothing");
      V := PD.Decide (None, True, ACPI_Svc, 5, Epoch_A, Incomplete);
      Check (V.Answer = CX.Received and then V.Record_It and then not V.Release, "INCOMPLETE releases nothing");
      --  User decision 2026-11-13: node failures as ACPICA (log, continue):
      --  a walk that ran to its end with errors still opens the launch.
      V := PD.Decide (None, True, ACPI_Svc, 6, Epoch_A, CX.Phase_Status'Enum_Rep (CX.Completed_With_Errors));
      Check (V.Answer = CX.Received and then V.Record_It and then V.Release,
             "COMPLETED_WITH_ERRORS is received and releases");
      V := PD.Decide (None, True, ACPI_Svc, 7, Epoch_A, CX.Phase_Status'Enum_Rep (CX.Failed));
      Check (V.Answer = CX.Received and then V.Record_It and then not V.Release, "FAILED releases nothing");
      V := PD.Decide ([(ACPI_Svc, Epoch_A, CX.Completed_With_Errors)], True, ACPI_Svc, 8, Epoch_A, Completed);
      Check (V.Answer = CX.Conflicting and then not V.Release,
             "COMPLETED after COMPLETED_WITH_ERRORS for one epoch is CONFLICTING, never a second release");
      --  Issue 3: the frame.
      Check (PD.Canonical (CX.Request_Words, 0, True, 0, Plain), "the canonical frame");
      Check (not PD.Canonical (CX.Request_Words + 1, 0, True, 0, Plain), "another length refused");
      Check (not PD.Canonical (CX.Request_Words, 1, True, 0, Plain), "protocol flags refused");
      Check (not PD.Canonical (CX.Request_Words, 0, False, 0, Plain), "IPC flags (an offered capability) refused");
      Check (not PD.Canonical (CX.Request_Words, 0, True, 1, Plain), "reserved bits refused");
      Check (not PD.Canonical (CX.Request_Words, 0, True, 0, [1, Epoch_A, Completed, 7]), "a stray word refused");
   end;

   --  HID offers (Devmgr_HID_Offers, CuBit.HID_Offer_Exchange 16#0252#):
   --  the frame, the request words, the receipt key (sender, epoch,
   --  request id), the controller from devmgr's own inventory, the GPIO
   --  controller from its verified records of the same sender and epoch,
   --  one queued offer per device path, both channel badges, and the
   --  deferred launch's order.
   declare
      package HO renames Devmgr_HID_Offers;
      package HX renames CuBit.HID_Offer_Exchange;
      package HP renames HID_Offer_Payload;
      use type HX.Disposition;
      use type HO.Controller_Outcome;
      use type HO.Launch_Step;
      ACPI_Svc : constant := 16#100_001E#;
      Other    : constant := 16#100_0020#;
      Epoch    : constant := 7;
      Later    : constant := 8;
      Touchpad_Address : constant := 16#15#;
      Fast_Hz  : constant := 400_000;
      Pin      : constant := 16#4C#;
      --  The IdeaPad touchpad's controller at 00:15.1 (_ADR 0x00150001).
      Controller_ADR : constant := 16#15_0001#;
      Controller : constant HP.PCI_Location := (Segment => 0, Bus => 0, Device => 16#15#, Func => 1);
      Host_Bridge : constant HP.PCI_Location := (Segment => 0, Bus => 0, Device => 0, Func => 0);
      Inventory : constant HO.PCI_Function_Array :=
        [(Location => Host_Bridge, Driver_Bound => False), (Location => Controller, Driver_Bound => True)];
      function Path (A, B : String; C : String := ""; D : String := "") return HP.Canonical_Path is
         Result : HP.Canonical_Path;
      begin
         Result.Names (1) := A;
         Result.Names (2) := B;
         Result.Count := 2;
         if C'Length /= 0 then
            Result.Names (3) := C;
            Result.Count := 3;
         end if;
         if D'Length /= 0 then
            Result.Names (4) := D;
            Result.Count := 4;
         end if;
         return Result;
      end Path;
      GPIO_Path : constant HP.Canonical_Path := Path ("_SB_", "GPI0");
      GPIO : constant HO.GPIO_Record :=
        (Owner => ACPI_Svc, Epoch => Epoch, HID => "INTC1057", UID_Kind => HP.Integer_UID, UID => 0,
         Path => GPIO_Path);
      GPIOs : constant HO.GPIO_Record_Array := [GPIO];
      No_Queue : constant HO.Queued_Offer_Array (1 .. 0) := [others => <>];
      Offer : HP.Request;
      O : HP.Request;
      V : HO.Verdict;
      Plain : constant HO.Word_Array (0 .. 3) := [1, 16#1_0000_0002#, 600, 0];
      Receipts : constant HO.Receipt_Array :=
        [(Owner => ACPI_Svc, Epoch => Epoch, Request => 1, Answer => HX.Queued, Detail => 0, Position => 1),
         (Owner => ACPI_Svc, Epoch => Epoch, Request => 2, Answer => HX.Duplicate_Device, Detail => 0,
          Position => 0)];
   begin
      Offer.Epoch := Epoch;
      Offer.Observation_Sequence := 1;
      Offer.PCI := (Has_SEG => True, Has_BBN => True, Has_ADR => True, SEG => 0, BBN => 0, ADR => Controller_ADR);
      Offer.Maximum_Hz := Fast_Hz;
      Offer.Address := Touchpad_Address;
      Offer.Pin := Pin;
      Offer.IRQ_Polarity := HP.Active_Low;
      Offer.GPIO_UID_Kind := HP.Integer_UID;
      Offer.GPIO_HID := "INTC1057";
      Offer.Device_Path := Path ("_SB_", "PC00", "I2C1", "TPD0");
      Offer.I2C_Path := Path ("_SB_", "PC00", "I2C1");
      Offer.GPIO_Path := GPIO_Path;
      --  Frame and request words.
      Check (HO.Canonical (HX.Request_Words, 0, True, 0, Plain), "hid offer: the canonical frame");
      Check (not HO.Canonical (HX.Request_Words + 1, 0, True, 0, Plain), "hid offer: another length refused");
      Check (not HO.Canonical (HX.Request_Words, 1, True, 0, Plain), "hid offer: protocol flags refused");
      Check (not HO.Canonical (HX.Request_Words, 0, False, 0, Plain), "hid offer: an offered capability refused");
      Check (not HO.Canonical (HX.Request_Words, 0, True, 1, Plain), "hid offer: reserved bits refused");
      Check (not HO.Canonical (HX.Request_Words, 0, True, 0, [1, 2, 3, 9]), "hid offer: a stray word refused");
      Check (HO.Check_Words (1, True, 600).Valid, "hid offer: request words valid");
      Check (not HO.Check_Words (0, True, 600).Valid, "hid offer: request id 0 malformed");
      Check (not HO.Check_Words (1, False, 600).Valid, "hid offer: a non-canonical reference malformed");
      Check (not HO.Check_Words (1, True, 0).Valid, "hid offer: length 0 malformed");
      Check (not HO.Check_Words (1, True, HP.Max_Payload_Bytes + 1).Valid
             and then HO.Check_Words (1, True, HP.Max_Payload_Bytes + 1).Detail = HP.Length_Offset,
             "hid offer: a length past the largest payload is malformed at the length field");
      Check (HO.Check_Words (1, True, HP.Max_Payload_Bytes).Valid, "hid offer: the largest payload is valid");
      --  Receipts: the key is (sender, epoch, request id).
      Check (HO.Find_Receipt (Receipts, ACPI_Svc, Epoch, 1) = 1, "hid offer: a recorded key is found");
      Check (HO.Find_Receipt (Receipts, ACPI_Svc, Later, 1) = 0, "hid offer: another epoch is another key");
      Check (HO.Find_Receipt (Receipts, Other, Epoch, 1) = 0, "hid offer: another sender is another key");
      Check (HO.Find_Receipt (Receipts, ACPI_Svc, Epoch, 3) = 0, "hid offer: another request id is another key");
      Check (HO.Recorded (HX.Queued) and then HO.Recorded (HX.Unknown_Controller)
             and then HO.Recorded (HX.Controller_Mismatch) and then HO.Recorded (HX.GPIO_Not_Admitted)
             and then HO.Recorded (HX.Duplicate_Device),
             "hid offer: decided answers are recorded");
      Check (not HO.Recorded (HX.Malformed) and then not HO.Recorded (HX.Not_Approved)
             and then not HO.Recorded (HX.Copy_Failed) and then not HO.Recorded (HX.Capacity),
             "hid offer: undecided answers are not recorded");
      --  The canonical offer is queued with both badges.
      V := HO.Decide (Offer, ACPI_Svc, Inventory, GPIOs, No_Queue);
      Check (V.Answer = HX.Queued and then V.Controller = 2 and then V.GPIO = 1 and then V.Detail = 0,
             "hid offer: the canonical offer is QUEUED");
      Check (V.I2C_Badge = I2C_Channel_Badges.Canonical
               ((Mode => I2C_Channel_Badges.Seven_Bit, Address => Touchpad_Address, Maximum_Hz => Fast_Hz)),
             "hid offer: the I2C badge is the offer's address, mode and speed");
      Check (V.I2C_Badge = 16#4932_1015_0006_1A80#, "hid offer: the I2C badge's golden word (0x15 at 400 kHz)");
      Check (V.GPIO_Badge = GPIO_Channel_Badges.Canonical
               ((Pin => Pin, Trigger => GPIO_Channel_Badges.Level_Triggered,
                 Active => GPIO_Channel_Badges.Active_Low)),
             "hid offer: the GPIO badge is the offer's pin, trigger and polarity");
      --  The controller.
      O := Offer;
      O.PCI.Has_BBN := False;
      O.PCI.BBN := 0;
      Check (HO.Decide (O, ACPI_Svc, Inventory, GPIOs, No_Queue).Answer = HX.Unknown_Controller,
             "hid offer: an unresolved _BBN is UNKNOWN_CONTROLLER (no guessed bus 0)");
      for Mask in 0 .. 6 loop
         O := Offer;
         O.PCI.Has_SEG := Mask mod 2 = 1;
         O.PCI.Has_BBN := (Mask / 2) mod 2 = 1;
         O.PCI.Has_ADR := Mask / 4 = 1;
         O.PCI.SEG := 0;
         O.PCI.BBN := 0;
         O.PCI.ADR := (if O.PCI.Has_ADR then Controller_ADR else 0);
         Check (HO.Decide (O, ACPI_Svc, Inventory, GPIOs, No_Queue).Answer = HX.Unknown_Controller,
                "hid offer: known mask" & Mask'Image & " is UNKNOWN_CONTROLLER");
      end loop;
      O := Offer;
      O.PCI.ADR := 16#11_0000#;
      Check (HO.Decide (O, ACPI_Svc, Inventory, GPIOs, No_Queue).Answer = HX.Unknown_Controller,
             "hid offer: an absent function is UNKNOWN_CONTROLLER");
      O := Offer;
      O.PCI.SEG := 1;
      Check (HO.Decide (O, ACPI_Svc, Inventory, GPIOs, No_Queue).Answer = HX.Unknown_Controller,
             "hid offer: another segment is UNKNOWN_CONTROLLER");
      O := Offer;
      O.PCI.ADR := 0;
      V := HO.Decide (O, ACPI_Svc, Inventory, GPIOs, No_Queue);
      Check (V.Answer = HX.Controller_Mismatch and then V.Controller = 1,
             "hid offer: a present function no driver binds is CONTROLLER_MISMATCH");
      Check (HO.Match_Controller (Offer.PCI, Inventory).Outcome = HO.Bound_Controller,
             "hid offer: the bound controller matches");
      O := Offer;
      O.I2C_Path := Path ("_SB_", "PC00", "I2C9");
      Check (HO.Decide (O, ACPI_Svc, Inventory, GPIOs, No_Queue).Answer = HX.Queued,
             "hid offer: the source path is provenance only");
      --  The GPIO controller.
      Check (HO.Decide (Offer, Other, Inventory, GPIOs, No_Queue).Answer = HX.GPIO_Not_Admitted,
             "hid offer: another sender's GPIO record is GPIO_NOT_ADMITTED");
      O := Offer;
      O.Epoch := Later;
      Check (HO.Decide (O, ACPI_Svc, Inventory, GPIOs, No_Queue).Answer = HX.GPIO_Not_Admitted,
             "hid offer: another epoch's GPIO record is GPIO_NOT_ADMITTED");
      O := Offer;
      O.GPIO_HID := "INTC1056";
      Check (HO.Decide (O, ACPI_Svc, Inventory, GPIOs, No_Queue).Answer = HX.GPIO_Not_Admitted,
             "hid offer: another GPIO _HID is GPIO_NOT_ADMITTED");
      O := Offer;
      O.GPIO_UID := 1;
      Check (HO.Decide (O, ACPI_Svc, Inventory, GPIOs, No_Queue).Answer = HX.GPIO_Not_Admitted,
             "hid offer: another GPIO _UID is GPIO_NOT_ADMITTED");
      O := Offer;
      O.GPIO_UID_Kind := HP.String_UID;
      Check (HO.Decide (O, ACPI_Svc, Inventory, GPIOs, No_Queue).Answer = HX.GPIO_Not_Admitted,
             "hid offer: another GPIO _UID kind is GPIO_NOT_ADMITTED");
      O := Offer;
      O.GPIO_Path := Path ("_SB_", "GPI1");
      Check (HO.Decide (O, ACPI_Svc, Inventory, GPIOs, No_Queue).Answer = HX.GPIO_Not_Admitted,
             "hid offer: another GPIO path is GPIO_NOT_ADMITTED");
      O := Offer;
      O.Pin := HO.No_Pin;
      Check (HO.Decide (O, ACPI_Svc, Inventory, GPIOs, No_Queue).Answer = HX.GPIO_Not_Admitted,
             "hid offer: pin 0xFFFF names no input: GPIO_NOT_ADMITTED");
      Check (HO.Decide (Offer, ACPI_Svc, Inventory, [1 .. 0 => <>], No_Queue).Answer = HX.GPIO_Not_Admitted,
             "hid offer: no verified GPIO controller: GPIO_NOT_ADMITTED");
      --  One queued offer per device path per (sender, epoch).
      Check (HO.Decide (Offer, ACPI_Svc, Inventory, GPIOs,
                        [(Owner => ACPI_Svc, Epoch => Epoch, Device_Path => Offer.Device_Path)]).Answer
               = HX.Duplicate_Device,
             "hid offer: a second offer of a queued device is DUPLICATE_DEVICE");
      Check (HO.Decide (Offer, ACPI_Svc, Inventory, GPIOs,
                        [(Owner => ACPI_Svc, Epoch => Later, Device_Path => Offer.Device_Path),
                         (Owner => Other, Epoch => Epoch, Device_Path => Offer.Device_Path),
                         (Owner => ACPI_Svc, Epoch => Epoch, Device_Path => Path ("_SB_", "PC00", "I2C1", "TPD1"))])
               .Answer = HX.Queued,
             "hid offer: another epoch, sender or device path is not a duplicate");
      --  Ten-bit, edge, both polarities.
      O := Offer;
      O.Addressing := HP.Ten_Bit;
      O.Address := 16#3FF#;
      O.IRQ_Trigger := HP.Edge;
      O.IRQ_Polarity := HP.Active_Both;
      V := HO.Decide (O, ACPI_Svc, Inventory, GPIOs, No_Queue);
      Check (V.Answer = HX.Queued
             and then V.I2C_Badge = I2C_Channel_Badges.Canonical
                                      ((Mode => I2C_Channel_Badges.Ten_Bit, Address => 16#3FF#,
                                        Maximum_Hz => Fast_Hz))
             and then V.GPIO_Badge = GPIO_Channel_Badges.Canonical
                                       ((Pin => Pin, Trigger => GPIO_Channel_Badges.Edge_Triggered,
                                         Active => GPIO_Channel_Badges.Active_Both)),
             "hid offer: ten-bit address and an edge on both polarities");
      --  The deferred launch: the GPIO driver registers first, then the
      --  controller's; a decline launches nothing.
      Check (HO.Next_Step (HO.Not_Started, HO.Serving) = HO.Await_GPIO_Driver,
             "hid offer: the HID driver waits for the GPIO driver");
      Check (HO.Next_Step (HO.Starting, HO.Serving) = HO.Await_GPIO_Driver,
             "hid offer: a launched GPIO driver must report ready first");
      Check (HO.Next_Step (HO.Serving, HO.Starting) = HO.Await_Controller_Driver,
             "hid offer: then for the controller's driver");
      Check (HO.Next_Step (HO.Serving, HO.Serving) = HO.Launch_HID, "hid offer: both serve: launch");
      Check (HO.Next_Step (HO.Declined, HO.Serving) = HO.Abandon
             and then HO.Next_Step (HO.Serving, HO.Declined) = HO.Abandon,
             "hid offer: a declined driver abandons the launch");

      --  Controller-correct bindings (SUP-001): the IdeaPad's two I2C
      --  controllers, 00:15.0 and 00:15.1, each with its own driver; the
      --  registry would name only the last to register. The binding names
      --  the driver launched for the offer's controller.
      declare
         First_Controller : constant HP.PCI_Location := (Segment => 0, Bus => 0, Device => 16#15#, Func => 0);
         Second_Controller : constant HP.PCI_Location := (Segment => 0, Bus => 0, Device => 16#15#, Func => 1);
         Absent : constant HP.PCI_Location := (Segment => 0, Bus => 0, Device => 16#16#, Func => 0);
         Driver_A : constant Unsigned_64 := 16#1_0000_0021#;
         Driver_B : constant Unsigned_64 := 16#1_0000_0022#;
         Both : constant HO.Launched_Driver_Array :=
           [(Process => Driver_A, Location => First_Controller, Serving => True),
            (Process => Driver_B, Location => Second_Controller, Serving => True)];
         Reversed : constant HO.Launched_Driver_Array := [Both (2), Both (1)];
         Not_Ready : constant HO.Launched_Driver_Array :=
           [(Process => Driver_A, Location => First_Controller, Serving => False), Both (2)];
         Twice : constant HO.Launched_Driver_Array :=
           [Both (1), (Process => Driver_B, Location => First_Controller, Serving => True)];
         None : constant HO.Launched_Driver_Array (1 .. 0) := [others => <>];
      begin
         Check (HO.Provider_At (Both, First_Controller) = Driver_A
                and then HO.Provider_At (Both, Second_Controller) = Driver_B,
                "binding: each controller's own driver");
         Check (HO.Provider_At (Reversed, First_Controller) = Driver_A,
                "binding: launch order does not matter (not the last registered)");
         Check (HO.Provider_At (Not_Ready, First_Controller) = HO.No_Process_Word,
                "binding: a driver that has not reported ready is no provider");
         Check (HO.Provider_At (Twice, First_Controller) = HO.No_Process_Word,
                "binding: two drivers for one function: none (never guess)");
         Check (HO.Provider_At (Both, Absent) = HO.No_Process_Word
                and then HO.Provider_At (None, First_Controller) = HO.No_Process_Word,
                "binding: no driver at the function: none");
      end;
   end;

   Put_Line ("acpi-windows:" & Checks'Image & " checks," & Failed'Image & " failures");
   if Failed /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
