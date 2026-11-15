------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  ACPI hardware windows regression (coordination/acpi-region-boundary.md,
--  slice 1): the grant procmgr gives an approved Request.ACPI_Hardware.
--
--  @description
--  Checks that the grant is non-empty, sorted and disjoint; that each
--  window's kernel capability covers exactly its stated range, read-write,
--  with no grant right; that the windows are exactly the FADT/ECDT fixed
--  blocks this program computes itself from its own firmware snapshot
--  (CuBit.ACPI_Fixed_Windows; on q35 no other holder overlaps them); that
--  the PM timer, read twice through its window, advances; and that a port
--  just past a window is refused.
--
--  Slice 2, the firmware-memory windows: at least one; sorted and
--  disjoint; each handle a firmware-memory capability covering exactly its
--  whole pages, read-write without grant, that cannot be copied on, whose
--  kind and cache mode in the grant are exactly what its kernel tag says
--  (CuBit.ACPI_Memory_Classes); one of them holds the FACS page this
--  program finds in its own FADT, write-back (on q35 it is the control
--  kind: SeaBIOS has no NVS), and mapping it shows the FACS signature; the
--  page just past (and just before) each window is refused.
--
--  The SCI (ACPI's System Control Interrupt) comes with the grant: one
--  read-only interrupt handle naming exactly the line this program's own
--  FADT and MADT give (CuBit.ACPI_SCI_Route; q35: SCI_INT 9, overridden to
--  GSI 9 level / active high). It enables the power button event
--  (PWRBTN_EN in PM1_EN), says "SCI armed" (the harness sends QEMU's
--  system_powerdown), and checks the level-triggered protocol
--  (CuBit.Handles.Lines): the interrupt arrives; no second one while it has
--  not acknowledged (the kernel holds the line masked: no storm);
--  acknowledging without clearing PWRBTN_STS delivers it again at once
--  (nothing lost); a second press while the line is held masked latches
--  with no delivery and is delivered at the acknowledgement (nothing lost);
--  clearing then acknowledging ends it (no further interrupt).
--  Acknowledging is ENABLE_IRQ's owner form (no system call of its own);
--  with no line handle, or for another line, it is refused.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles; use CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Control;
with CuBit.Handles.Startup;
with CuBit.Handles.Devices;
with CuBit.ACPI_Windows; use CuBit.ACPI_Windows;
with CuBit.ACPI_Windows.Launch;
with CuBit.ACPI_Fixed_Windows;
with CuBit.ACPI_Memory_Classes;
with CuBit.Firmware_Snapshot;
with CuBit.Firmware_Snapshot.Whole_Tables;
with CuBit.ACPI_SCI_Route;
with CuBit.Device_Interrupts;
with CuBit.Handles.Lines;
with Interrupt_Line_ABI;
with Startup;

procedure Main is
   use ASCII;
   package IPC renames CuBit.Handles.IPC;
   package FS renames CuBit.Firmware_Snapshot;
   package AF renames CuBit.ACPI_Fixed_Windows;
   use type ABI.Kind;
   use type FS.Outcome, FS.Byte_Count;
   use type CuBit.ACPI_Memory_Classes.Memory_Kind, CuBit.ACPI_Memory_Classes.Cache_Mode;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("acpi-hardware-check: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         debugPrint ("acpi-hardware-check: " & Name & " FAIL" & LF);
      end if;
   end Check;

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   --  The FADT's 32-bit PM timer block address (ACPI 6.5, 5.2.9).
   PM_Timer_Offset : constant := AF.PM_Timer_Offset;
   PM_Timer_Mask   : constant Unsigned_64 := 16#FF_FFFF#;   --  24-bit counter
   Timer_Gap_Ms    : constant := 2;
   Table_Bytes     : constant := 512;
   Probe_Label     : constant Unsigned_32 := 16#AC35#;     --  the stand-in's probe
   REPLY_OK        : constant Unsigned_32 := 16#F000#;
   Probe_Deadline_Ms : constant := 2_000;
   --  The FADT's FACS addresses (ACPI 6.5, 5.2.9): FIRMWARE_CTRL (32-bit)
   --  and X_FIRMWARE_CTRL (64-bit, when the FADT reaches it).
   Firmware_Ctrl_Offset   : constant := 36;
   X_Firmware_Ctrl_Offset : constant := 132;
   Page_Bytes             : constant := 4096;
   --  Where this program maps firmware pages (its own address space).
   Window_Virtual         : constant := 16#6000_0000_0000#;
   --  "FACS" as the little-endian signature word.
   FACS_Signature         : constant Unsigned_32 := 16#5343_4146#;
   --  Any other tag a holder might try to stamp on its copy.
   Forged_Tag      : constant Unsigned_64 := 16#5245_4241_4447_4521#;

   Tables : constant FS.Authority := Startup.Tables;
   Grant_Value : constant CuBit.ACPI_Windows.Grant := Startup.Hardware;

   --  The first Table_Bytes of the snapshot's table named Signature.
   procedure Read_Table (Signature : String; Into : out AF.Byte_Array; Length : out Natural) is
      Catalog : FS.Catalog_Info;
      D : FS.Table_Description;
      Status : FS.Outcome;
   begin
      Into := [others => 0];
      Length := 0;
      FS.Info (Tables, Catalog, Status);
      if Status /= FS.Ok then
         return;
      end if;
      for T in 1 .. FS.Table_ID (Catalog.Table_Count) loop
         FS.Describe (Tables, T, D, Status);
         if Status = FS.Ok and then
           (for all I in FS.Signature_Index =>
              D.Signature (I) = Character'Pos (Signature (Signature'First + I - 1)))
         then
            declare
               Wanted : constant Natural := Natural (FS.Byte_Count'Min (D.Extent, Table_Bytes));
               Chunk : FS.Byte_Array (1 .. Wanted);
               Copied : FS.Byte_Count;
            begin
               FS.Copy_Part (Tables, T, 0, Chunk, Copied, Status);
               if Status = FS.Ok and then Copied = FS.Byte_Count (Wanted) then
                  for I in 1 .. Wanted loop
                     Into (I) := Chunk (I);
                  end loop;
                  Length := Wanted;
               end if;
            end;
            return;
         end if;
      end loop;
   end Read_Table;

   FADT, ECDT : AF.Byte_Array (1 .. Table_Bytes);
   FADT_Length, ECDT_Length : Natural;
   Expected : AF.Window_List;
   G : CuBit.ACPI_Windows.Grant renames Grant_Value;
begin
   debugPrint ("acpi-hardware-check: starting" & LF);
   Check (G.IO_Used > 0, "grant has I/O windows");
   Check (G.Memory_Used > 0, "grant has memory windows");
   Check (CuBit.ACPI_Windows.Launch.Well_Formed (G), "windows sorted and disjoint");

   for K in 1 .. G.IO_Used loop
      declare
         W : constant IO_Window := G.IO (K);
         D : constant IPC.Description := IPC.Describe (W.Ports);
      begin
         debugPrint ("acpi-hardware-check: window " & Image (Unsigned_64 (W.First)) & " +" &
                     Image (Unsigned_64 (W.Count)) & LF);
         Check (D.Current and then D.Kind = ABI.IO_Ports
                and then D.Object = Unsigned_64 (W.First)
                and then D.Parameter = Unsigned_64 (W.Count),
                "window " & Image (Unsigned_64 (K)) & " handle covers exactly its range");
         Check (D.Current and then D.Held.Read and then D.Held.Write and then not D.Held.Grant,
                "window " & Image (Unsigned_64 (K)) & " read-write without grant");
         --  The port just past the window is not the handle's to touch.
         if Unsigned_32 (W.First) + W.Count < Port_Count'Last then
            Check (CuBit.Handles.Devices.In_8
                     (W.Ports, Unsigned_16 (Unsigned_32 (W.First) + W.Count)) = Unsigned_64'Last,
                   "window " & Image (Unsigned_64 (K)) & " refuses the next port");
         end if;
      end;
   end loop;

   --  Independently: the fixed blocks of this program's own FADT/ECDT copy.
   Read_Table (AF.FADT_Signature, FADT, FADT_Length);
   Read_Table (AF.ECDT_Signature, ECDT, ECDT_Length);
   Check (FADT_Length > 0, "FADT read");
   Check (ECDT_Length = 0, "q35 has no ECDT");
   AF.Compute (FADT (1 .. FADT_Length), ECDT (1 .. ECDT_Length), Expected);
   Check (Expected.Count = G.IO_Used
          and then (for all K in 1 .. Expected.Count =>
                      Expected.Windows (K).First = G.IO (K).First
                      and then Expected.Windows (K).Count = G.IO (K).Count),
          "windows are exactly the FADT fixed blocks");

   --  The firmware-memory windows.
   for K in 1 .. G.Memory_Used loop
      declare
         W : constant Memory_Window := G.Memory (K);
         D : constant IPC.Description := IPC.Describe (W.Memory);
         Past : constant Unsigned_64 := (W.First_Page + W.Pages) * Page_Bytes;
      begin
         debugPrint ("acpi-hardware-check: memory window page " & Image (W.First_Page) & " +" &
                     Image (W.Pages) & LF);
         Check (D.Current and then D.Kind = ABI.Device_Memory
                and then D.Object = W.First_Page * Page_Bytes
                and then D.Parameter = W.Pages * Page_Bytes,
                "memory window " & Image (Unsigned_64 (K)) & " handle covers exactly its pages");
         Check (D.Current and then CuBit.ACPI_Memory_Classes.Firmware_Tag (D.Authority_Tag),
                "memory window " & Image (Unsigned_64 (K)) & " is firmware memory");
         --  The grant's kind and cache mode are the kernel tag's, not the
         --  launcher's.
         Check (D.Current and then CuBit.ACPI_Memory_Classes.Firmware_Tag (D.Authority_Tag)
                and then W.Kind = CuBit.ACPI_Memory_Classes.Kind_Of (D.Authority_Tag)
                and then W.Cache = CuBit.ACPI_Memory_Classes.Cache_Of (W.Kind)
                and then W.Cache = CuBit.ACPI_Memory_Classes.Write_Back,
                "memory window " & Image (Unsigned_64 (K)) & " kind and cache mode from its kernel tag");
         Check (D.Current and then D.Held.Read and then D.Held.Write and then not D.Held.Grant,
                "memory window " & Image (Unsigned_64 (K)) & " read-write without grant");
         Check (not Valid (Device_Memory'(CuBit.Handles.Control.Derive (Copy (W.Memory, Read_Only))))
                and then not Valid (Device_Memory'(CuBit.Handles.Control.Derive
                                       (Narrowed (W.Memory, Read_Only, W.First_Page * Page_Bytes,
                                                  Page_Bytes)))),
                "memory window " & Image (Unsigned_64 (K)) & " cannot be copied on");
         Check (not CuBit.Handles.Devices.Map_Device (W.Memory, Past, Window_Virtual, 1, True),
                "memory window " & Image (Unsigned_64 (K)) & " refuses the next page");
         if W.First_Page > 0 then
            Check (not CuBit.Handles.Devices.Map_Device
                         (W.Memory, (W.First_Page - 1) * Page_Bytes, Window_Virtual, 1, True),
                   "memory window " & Image (Unsigned_64 (K)) & " refuses the page before");
         end if;
      end;
   end loop;

   --  The FACS page, from this program's own FADT, is in a window and maps.
   declare
      FACS : Unsigned_64 := 0;
      Found : Natural := 0;
   begin
      for I in 0 .. 3 loop
         FACS := FACS or Shift_Left (Unsigned_64 (FADT (Firmware_Ctrl_Offset + 1 + I)), 8 * I);
      end loop;
      if FADT_Length >= X_Firmware_Ctrl_Offset + 8 then
         declare
            Extended : Unsigned_64 := 0;
         begin
            for I in 0 .. 7 loop
               Extended := Extended or
                 Shift_Left (Unsigned_64 (FADT (X_Firmware_Ctrl_Offset + 1 + I)), 8 * I);
            end loop;
            if Extended /= 0 then
               FACS := Extended;
            end if;
         end;
      end if;
      debugPrint ("acpi-hardware-check: FACS at " & Image (FACS) & LF);
      for K in 1 .. G.Memory_Used loop
         if FACS / Page_Bytes >= G.Memory (K).First_Page
           and then FACS / Page_Bytes - G.Memory (K).First_Page < G.Memory (K).Pages
         then
            Found := K;
         end if;
      end loop;
      Check (FACS /= 0 and then Found /= 0, "FACS page inside a memory window");
      Check (Found /= 0 and then G.Memory (Found).Cache = CuBit.ACPI_Memory_Classes.Write_Back
             and then (G.Memory (Found).Kind = CuBit.ACPI_Memory_Classes.ACPI_NVS
                       or else G.Memory (Found).Pages = 1),
             "FACS window is write-back (an NVS run, or the FACS page alone)");
      if Found /= 0 then
         declare
            Page : constant Unsigned_64 := FACS - FACS mod Page_Bytes;
            Mapped : constant Boolean := CuBit.Handles.Devices.Map_Device
              (G.Memory (Found).Memory, Page, Window_Virtual, 1, True);
         begin
            Check (Mapped, "FACS page maps through its window");
            if Mapped then
               declare
                  Signature : Unsigned_32 with Import, Volatile,
                    Address => System'To_Address (Window_Virtual + FACS mod Page_Bytes);
               begin
                  Check (Signature = FACS_Signature, "FACS signature read through its window");
               end;
            end if;
         end;
      end if;
   end;

   --  The PM timer advances when read twice through its window.
   declare
      Timer : Unsigned_64 := 0;
      Found : Natural := 0;
      First_Read, Second_Read : Unsigned_64;
      Ignore : Unsigned_64;
   begin
      for I in 0 .. 3 loop
         Timer := Timer or Shift_Left (Unsigned_64 (FADT (PM_Timer_Offset + 1 + I)), 8 * I);
      end loop;
      for K in 1 .. G.IO_Used loop
         if Timer >= Unsigned_64 (G.IO (K).First)
           and then Timer + 4 <= Unsigned_64 (G.IO (K).First) + Unsigned_64 (G.IO (K).Count)
         then
            Found := K;
         end if;
      end loop;
      Check (Timer /= 0 and then Found /= 0, "PM timer inside a window");
      if Found /= 0 then
         First_Read := CuBit.Handles.Devices.In_32 (G.IO (Found).Ports, Unsigned_16 (Timer));
         Ignore := syscall (SYSCALL_SLEEP, Timer_Gap_Ms);
         Second_Read := CuBit.Handles.Devices.In_32 (G.IO (Found).Ports, Unsigned_16 (Timer));
         debugPrint ("acpi-hardware-check: PM timer " & Image (First_Read and PM_Timer_Mask) &
                     " then " & Image (Second_Read and PM_Timer_Mask) & LF);
         Check (First_Read /= Unsigned_64'Last and then Second_Read /= Unsigned_64'Last
                and then (First_Read and PM_Timer_Mask) /= (Second_Read and PM_Timer_Mask),
                "PM timer advances through its window");
      end if;
   end;

   --  The SCI line and the level-triggered acknowledgement protocol.
   declare
      package SR renames CuBit.ACPI_SCI_Route;
      package WT renames CuBit.Firmware_Snapshot.Whole_Tables;
      package LA renames Interrupt_Line_ABI;
      use type WT.Read_Status, LA.Trigger_Mode, LA.Pin_Polarity;
      --  ACPI 6.5, 4.8.4.1: PM1 status at the event block, enable in its
      --  second half; the power button is bit 8 of both (status: write 1
      --  to clear).
      PWRBTN_Bit : constant Unsigned_16 := 2 ** 8;
      --  How long it waits for the injected event, for silence, and for a
      --  re-delivery.
      Event_Deadline_Ms    : constant := 20_000;
      Silence_Ms           : constant := 300;
      Redelivery_Ms        : constant := 2_000;
      --  How often it looks at PWRBTN_STS while the line is held masked.
      Poll_Ms              : constant := 20;

      --  The whole table named Signature (empty when absent).
      function Whole (Signature : String) return SR.Byte_Array is
         Table : FS.Table_ID;
         Bytes : WT.Table_Bytes;
         Status : WT.Read_Status;
      begin
         WT.Find (Tables, Signature, Table, Bytes, Status);
         if Status /= WT.Found then
            return [1 .. 0 => 0];
         end if;
         declare
            Into : SR.Byte_Array (1 .. Bytes);
         begin
            WT.Copy (Tables, Table, Into, Status);
            return (if Status = WT.Found then Into else [1 .. 0 => 0]);
         end;
      end Whole;

      --  Whether the IRQ doorbell rings before Milliseconds pass (other
      --  messages are ignored).
      function Doorbell_Within (Milliseconds : Unsigned_64) return Boolean is
         Deadline : constant Unsigned_64 := Deadline_After (Milliseconds);
         From : Process_ID;
         Msg : Message;
         Received : Boolean;
      begin
         loop
            receiveUntil (Deadline, From, Msg, Received);
            if not Received then
               return False;
            elsif CuBit.Device_Interrupts.Is_Doorbell (Msg) then
               return True;
            end if;
         end loop;
      end Doorbell_Within;

      SCI : constant CuBit.ACPI_Windows.SCI_Line := G.SCI;
      Own_FADT : constant SR.Byte_Array := Whole (SR.FADT_Signature);
      Own_MADT : constant SR.Byte_Array := Whole (SR.MADT_Signature);
      Expected_Route : constant SR.Resolution := SR.Resolve_SCI (Own_FADT, Own_MADT);
      PM1_Base, PM1_Length : Unsigned_64 := 0;
      PM1 : Natural := 0;   --  the window holding the PM1 event block
   begin
      Check (SCI.Present, "SCI line granted");
      Check (Expected_Route.Routed, "SCI resolves from this program's own FADT and MADT");
      if Expected_Route.Routed then
         debugPrint ("acpi-hardware-check: SCI is GSI" & Unsigned_32'Image (Expected_Route.Route.GSI) &
                     (if Expected_Route.Route.Trigger = LA.Level then " level" else " edge") &
                     (if Expected_Route.Route.Polarity = LA.Active_Low then " active-low" else " active-high") &
                     (if Expected_Route.Route.Overridden then " (override)" else " (default)") & LF);
         Check (Expected_Route.Route.GSI = 9 and then Expected_Route.Route.Overridden
                and then Expected_Route.Route.Trigger = LA.Level
                and then Expected_Route.Route.Polarity = LA.Active_High,
                "q35 SCI is the MADT override's GSI 9, level, active high");
      end if;
      if SCI.Present then
         declare
            D : constant IPC.Description := IPC.Describe (SCI.Line);
         begin
            Check (D.Current and then D.Kind = ABI.Interrupt and then D.Parameter = 1
                   and then D.Object = Unsigned_64 (SCI.Vector)
                   and then Expected_Route.Routed
                   and then SCI.Vector = Expected_Route.Vector,
                   "SCI handle names exactly the resolved line");
            Check (D.Current and then D.Held.Read and then not D.Held.Grant,
                   "SCI handle is read-only without grant");
         end;
      end if;
      declare
         use type LA.Route_Outcome;
         Refused : constant CuBit.Handles.Lines.Acknowledgement :=
           CuBit.Handles.Lines.Acknowledge (No_Interrupt, LA.Line_Vector'First);
      begin
         Check (not Refused.Accepted and then Refused.Why = LA.No_Line_Authority,
                "acknowledging no line handle is refused");
      end;
      if SCI.Present then
         declare
            use type LA.Route_Outcome;
            Other : constant LA.Line_Vector :=
              (if SCI.Vector = LA.Line_Vector'Last then LA.Line_Vector'First else SCI.Vector + 1);
            Refused : constant CuBit.Handles.Lines.Acknowledgement :=
              CuBit.Handles.Lines.Acknowledge (SCI.Line, Other);
         begin
            Check (not Refused.Accepted and then Refused.Why = LA.Malformed_Acknowledge,
                   "acknowledging another line with the SCI handle is refused");
         end;
      end if;

      --  The PM1a event block, from this program's own FADT.
      for I in 0 .. 3 loop
         PM1_Base := PM1_Base or
           Shift_Left (Unsigned_64 (FADT (AF.PM1a_Event_Offset + 1 + I)), 8 * I);
      end loop;
      PM1_Length := Unsigned_64 (FADT (AF.PM1_Event_Length_Offset + 1));
      for K in 1 .. G.IO_Used loop
         if PM1_Base >= Unsigned_64 (G.IO (K).First)
           and then PM1_Base + PM1_Length <= Unsigned_64 (G.IO (K).First) + Unsigned_64 (G.IO (K).Count)
         then
            PM1 := K;
         end if;
      end loop;
      Check (PM1 /= 0 and then PM1_Length >= 4, "PM1 event block inside a window");

      if SCI.Present and then PM1 /= 0 and then PM1_Length >= 4 then
         declare
            Ports : constant IO_Ports := G.IO (PM1).Ports;
            Status_Port : constant Unsigned_16 := Unsigned_16 (PM1_Base);
            Enable_Port : constant Unsigned_16 := Unsigned_16 (PM1_Base + PM1_Length / 2);
            Enabled : constant Unsigned_64 := CuBit.Handles.Devices.In_16 (Ports, Enable_Port);
            Ack : CuBit.Handles.Lines.Acknowledgement;
            Ignore : Boolean;
         begin
            --  Start clean: clear a stale press, then enable the event.
            Ignore := CuBit.Handles.Devices.Out_16 (Ports, Status_Port, PWRBTN_Bit);
            Check (Enabled /= Unsigned_64'Last
                   and then CuBit.Handles.Devices.Out_16
                              (Ports, Enable_Port, Unsigned_16 (Enabled and 16#FFFF#) or PWRBTN_Bit),
                   "power button event enabled through the PM1 window");
            debugPrint ("acpi-hardware-check: SCI armed" & LF);

            Check (Doorbell_Within (Event_Deadline_Ms), "SCI delivered for the power button");
            Check ((CuBit.Handles.Devices.In_16 (Ports, Status_Port) and Unsigned_64 (PWRBTN_Bit)) /= 0,
                   "PWRBTN_STS set");
            Check (not Doorbell_Within (Silence_Ms),
                   "SCI held masked until acknowledged (no storm)");

            --  Acknowledge without clearing: still asserted, so it comes
            --  again at once.
            Ack := CuBit.Handles.Lines.Acknowledge (SCI.Line, SCI.Vector);
            Check (Ack.Accepted and then Ack.Delivered = 1,
                   "first acknowledgement accepted after one delivery");
            Check (Doorbell_Within (Redelivery_Ms),
                   "uncleared SCI delivered again after acknowledgement (nothing lost)");

            --  A second event while the line is held masked: clear the first
            --  press but do not acknowledge; the harness presses again. The
            --  status latches with no delivery; the acknowledgement then
            --  delivers it at once.
            Ignore := CuBit.Handles.Devices.Out_16 (Ports, Status_Port, PWRBTN_Bit);
            Check ((CuBit.Handles.Devices.In_16 (Ports, Status_Port) and Unsigned_64 (PWRBTN_Bit)) = 0,
                   "PWRBTN_STS cleared");
            debugPrint ("acpi-hardware-check: SCI second press wanted" & LF);
            declare
               Deadline : constant Unsigned_64 := Deadline_After (Event_Deadline_Ms);
               Latched, Delivered_Early : Boolean := False;
            begin
               loop
                  if Doorbell_Within (Poll_Ms) then
                     Delivered_Early := True;
                  end if;
                  Latched := (CuBit.Handles.Devices.In_16 (Ports, Status_Port)
                              and Unsigned_64 (PWRBTN_Bit)) /= 0;
                  exit when Latched or else Deadline_After (0) >= Deadline;
               end loop;
               Check (Latched, "second press latched in PWRBTN_STS");
               Check (not Delivered_Early, "second press not delivered while the line is held masked");
            end;
            Ack := CuBit.Handles.Lines.Acknowledge (SCI.Line, SCI.Vector);
            Check (Ack.Accepted and then Ack.Delivered = 2,
                   "acknowledged with the second press pending, two deliveries so far");
            Check (Doorbell_Within (Redelivery_Ms),
                   "second SCI delivered at the acknowledgement (not lost)");

            --  Clear the source, then acknowledge: it ends.
            Ignore := CuBit.Handles.Devices.Out_16 (Ports, Status_Port, PWRBTN_Bit);
            Ack := CuBit.Handles.Lines.Acknowledge (SCI.Line, SCI.Vector);
            Check (Ack.Accepted and then Ack.Delivered = 3,
                   "acknowledged after clearing, three deliveries in all");
            Check (not Doorbell_Within (Silence_Ms),
                   "no SCI after clearing and acknowledgement");
            Ack := CuBit.Handles.Lines.Acknowledge (SCI.Line, SCI.Vector);
            Check (Ack.Accepted and then Ack.Delivered = 3,
                   "the line stayed quiet: still three deliveries");
            Ignore := CuBit.Handles.Devices.Out_16
              (Ports, Enable_Port, Unsigned_16 (Enabled and 16#FFFF#));
            debugPrint ("acpi-hardware-check: SCI done" & LF);
         end;
      end if;
   end;

   if Failures = 0 then
      debugPrint ("TEST: PASS acpi-hardware" & LF);
   else
      debugPrint ("TEST: FAIL acpi-hardware" & Natural'Image (Failures) & LF);
   end if;
end Main;
