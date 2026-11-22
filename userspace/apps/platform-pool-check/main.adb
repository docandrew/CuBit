------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The platform-register pool regression (ACPI-POOL-001,
--  Platform_Pool_ABI, CuBit.Platform_Pool_Exchange), standing in for the
--  ACPI backend.
--
--  @description
--  On q35 with QEMU's edu device (devmgr's declared test profile: edu's
--  BAR0's first 128 KiB stands in for the host bridge's MCHBAR, the rest
--  for a P2SB sideband window with one 64 KiB GPIO community at +0xC0000,
--  each its own member) and before edu's fixture
--  driver binds (generic-check.drv starts once logstore runs, after this
--  program in the plan):
--    * acquires PCI segment 0 bus 0's read-only configuration window from
--      devmgr's named PCI broker (since the ACPI PCI-windows cohort there
--      are no startup PCI configuration pregrants: the backend, and so
--      this validated com.cubit.acpi stand-in, asks the broker for
--      a bus's window) and finds edu's BAR0 through it;
--    * asks devmgr for the pool through the firmware-data badge, gets it
--      with the pool tag, read-write without grant, and cannot copy it on;
--    * maps edu's first register page through it: plain loads and stores
--      work (the identification register, the liveness register's
--      inversion), with no system call per access;
--    * the laptop's image-10 witness shape (two pages at MCHBAR+0x7000,
--      AML's MBAR = MCHBAR + 0x7110, 4 KiB) maps; a request across the
--      MCHBAR stand-in's end is refused (members never join); a page of
--      the community and of the window between map;
--    * a page past the member and a RAM page are refused, typed;
--    * maps the community page at Growth_Mappings distinct addresses, so
--      the kernel's record of its mappings grows (charged to this
--      program's quota);
--    * then waits for the driver to bind and claim the whole BAR. The
--      MCHBAR and window stand-ins are Device_BAR members: mapping edu
--      again is refused CLAIMED, naming the driver and its pages (the
--      kernel logs the revocation of those mappings). The community
--      stand-in is Chipset_Shared: the claim is shared (kernel log
--      "platform-pool: shared"), the community stays mapped and readable,
--      and a new mapping of it is still admitted (the path a GPIO driver
--      binding one community takes);
--    * says TEST: PASS platform-pool, then touches the revoked mapping,
--      which must fault (the harness checks the kill).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Console_Route;
with Wait_ABI;
with System;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles; use CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Control;
with CuBit.Handles.Devices;
with CuBit.Handles.Startup;
with CuBit.Handles.Platform_Pools;
with CuBit.ACPI_Memory_Classes;
with CuBit.Firmware_Data_Exchange;
with CuBit.Platform_Pool_Exchange;
with CuBit.GPIO_Bootstrap_Exchange;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with GPIO_Bootstrap_Payload;
with CuBit.Deadlines.Clock;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Process_IDs;
with Platform_Pool_ABI;
with ACPI_PCI_Window_Client;
with Startup;
with CuBit.Monotonic;

procedure Main is
   use ASCII;
   package IPC renames CuBit.Handles.IPC;
   package HS renames CuBit.Handles.Startup;
   package AMC renames CuBit.ACPI_Memory_Classes;
   package FDX renames CuBit.Firmware_Data_Exchange;
   package PX renames CuBit.Platform_Pool_Exchange;
   package PA renames Platform_Pool_ABI;
   package Client renames ACPI_PCI_Window_Client;
   package Q renames Client.Q;
   use type ABI.Kind;
   use type PX.Disposition;
   use type Client.Outcome;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("platform-pool-check: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         debugPrint ("platform-pool-check: " & Name & " FAIL" & LF);
      end if;
   end Check;

   Hex_Digits : constant String := "0123456789ABCDEF";
   Nibble_Bits : constant := 4;
   Nibble_Mask : constant := 16#F#;
   function Hex (Value : Unsigned_64) return String is
      Result : String (1 .. 16);
      V : Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Hex_Digits (Hex_Digits'First + Natural (V and Nibble_Mask));
         V := Shift_Right (V, Nibble_Bits);
      end loop;
      return Result;
   end Hex;

   Page_Bytes : constant := FDX.Page_Bytes;
   --  Where this program maps things (its own address space).
   Edu_Virtual    : constant := 16#6000_0000_0000#;
   Again_Virtual  : constant := 16#6000_0010_0000#;
   Probe_Virtual  : constant := 16#6000_0020_0000#;
   Witness_Virtual   : constant := 16#6000_0030_0000#;
   Community_Virtual : constant := 16#6000_0040_0000#;
   Window_Virtual    : constant := 16#6000_0050_0000#;
   Shared_Virtual    : constant := 16#6000_0060_0000#;
   Growth_Virtual    : constant := 16#6000_0100_0000#;
   --  Distinct mappings of one community page: past one page of the
   --  kernel's mapping records (about a hundred), so the table grows twice.
   Growth_Mappings   : constant := 300;
   PCI_Virtual    : constant := 16#6000_1000_0000#;
   Request_Deadline_Ms : constant := 2_000;
   --  How long the driver may take to bind (logstore starts after this
   --  program; devmgr then launches it), and how often to look.
   Bind_Deadline_Ms : constant := 30_000;
   Poll_Us          : constant := 50_000;
   --  Usable RAM in every headless guest (16 MiB in).
   RAM_Page : constant := 16#0100_0000#;
   PCI_Bus_Bytes : constant := AMC.PCI_Bus_Bytes;

   --  PCI configuration header (PCI Local Bus 3.0, 6.1).
   PCI_Devices   : constant := AMC.PCI_Devices_Per_Bus;
   PCI_Functions : constant := AMC.PCI_Functions_Per_Device;
   Vendor_Mask   : constant Unsigned_32 := 16#FFFF#;
   Vendor_Shift  : constant := 16;
   BAR0_Offset   : constant := 16#10#;
   BAR_Address_Mask : constant Unsigned_32 := 16#FFFF_FFF0#;
   --  QEMU's edu device (docs/specs/edu.rst): BAR0 is 1 MiB of registers;
   --  0x00 identification 0xRRrr00ED (version 1.0), 0x04 liveness check
   --  (reads back the inverse of what was written).
   Edu_Vendor        : constant Unsigned_32 := 16#1234#;
   Edu_Device        : constant Unsigned_32 := 16#11E8#;
   Edu_BAR0_Bytes    : constant := 16#10_0000#;
   --  devmgr's test profile (Devmgr_Platform_Pool): the MCHBAR stand-in
   --  is BAR0's first 128 KiB, the community stand-in 64 KiB at +0xC0000.
   MCHBAR_Bytes      : constant := 16#2_0000#;
   Community_Offset  : constant := 16#C_0000#;
   --  The IdeaPad's image-10 witness: TcssSsdt MBAR = MCHBAR + 0x7110,
   --  0x1000 bytes, so pages +0x7000 and +0x8000.
   Witness_Offset    : constant := 16#7000#;
   Witness_Pages     : constant := 2;
   Edu_Identity_Low  : constant Unsigned_32 := 16#ED#;
   Edu_Identity_Mask : constant Unsigned_32 := 16#FF#;
   Edu_Liveness      : constant := 16#04#;
   Liveness_Probe    : constant Unsigned_32 := 16#1234_5678#;

   Data_Endpoint : constant HS.Optional_Endpoint :=
     HS.Optional_Endpoint_Named (FDX.Data_Label ("hardware"));

   --  edu's BAR0 through the read-only configuration window at
   --  PCI_Virtual; 0 when absent.
   function Edu_BAR0 return Unsigned_64 is
      function Config (Device, Offset : Natural) return Unsigned_32 is
         Word : Unsigned_32 with Import, Volatile,
           Address => System'To_Address
             (PCI_Virtual + Unsigned_64 (Device * PCI_Functions) * Page_Bytes + Unsigned_64 (Offset));
      begin
         return Word;
      end Config;
   begin
      for Device in 0 .. PCI_Devices - 1 loop
         if (Config (Device, 0) and Vendor_Mask) = Edu_Vendor
           and then Shift_Right (Config (Device, 0), Vendor_Shift) = Edu_Device
         then
            return Unsigned_64 (Config (Device, BAR0_Offset) and BAR_Address_Mask);
         end if;
      end loop;
      return 0;
   end Edu_BAR0;

   procedure Pause is
      Now_Us : constant Unsigned_64 :=
        CuBit.Monotonic.Microseconds;
      Ignore : Unsigned_64;
   begin
      Ignore := CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Wait, Wait_ABI.Timer_Sources, Wait_ABI.Forever, Now_Us + Poll_Us);
   end Pause;

   --  devmgr's named PCI broker (the manifest's optional "pci-windows").
   Broker : constant HS.Optional_Endpoint := Startup.Pci_Windows;
   Bus_Request : constant Q.Request :=
     (ID => 1, Key => (Segment => 0, Bus => 0), Mode => Q.Read_Only);
   Run : Client.Context;
   Broker_Result : Client.Report;

   Pool : Device_Memory := No_Device_Memory;
   Why  : PA.Refusal_Record;
   Edu_Base : Unsigned_64 := 0;
   Bus_Zero : Unsigned_64 := 0;   --  PCI segment 0 bus 0's ECAM base
   First_Mapped : Boolean := False;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   debugPrint ("platform-pool-check: starting" & LF);
   if not Data_Endpoint.Present then
      debugPrint ("platform-pool-check: no firmware-data endpoint" & LF);
      debugPrint ("TEST: FAIL platform-pool" & LF);
      return;
   end if;

   --  The pool, through the firmware-data badge.
   declare
      Msg : Message := NULL_MESSAGE;
      Tag : MessageTag;
      Got : IPC.Carried;
      Request : constant Unsigned_64 := 1;
   begin
      Msg.tag := (label => PX.Pool_Request, length => PX.Request_Words,
                  protocol => 0, ipc => <>, reserved => 0);
      Msg.words (PX.Request_Word) := Request;
      Tag := IPC.Call_Accepting
        (Data_Endpoint.Value, Msg, CuBit.Deadlines.Clock.Now + Request_Deadline_Ms, Got);
      Check (Tag.label = PX.Pool_Request and then Tag.length = PX.Reply_Words
             and then Msg.words (PX.Reply_Request_Word) = Request
             and then Msg.words (PX.Disposition_Word) = PX.Disposition'Enum_Rep (PX.Granted),
             "devmgr grants the pool");
      debugPrint ("platform-pool-check: pool members" &
                  Unsigned_64'Image (Msg.words (PX.Members_Word)) & LF);
      declare
         Taken : constant IPC.Accepted_Device_Memory := IPC.Accept_Device_Memory (Got);
      begin
         if Taken.OK then
            Pool := Taken.Value;
         end if;
      end;
   end;
   --  The GPIO bootstrap admission (CuBit.GPIO_Bootstrap_Exchange) over
   --  the same badge, end to end: the payload lent as a read-only grant,
   --  copied and decoded by devmgr. A defective payload is MALFORMED at its
   --  byte; a whole one is NO_DISCOVERY_FAILURE here (the test profile's
   --  quirk row reads no P2SB, so there is nothing to fall back from), and
   --  the same request id asked again gets the same answer.
   declare
      package GX renames CuBit.GPIO_Bootstrap_Exchange;
      package GP renames GPIO_Bootstrap_Payload;
      Page : GP.Byte_Array (0 .. Page_Bytes - 1) := [others => 0]
      with Alignment => Page_Bytes;
      R : GP.Request;
      Length : constant Natural := GP.Encoded_Length (1);
      Grant : CuBit.Memory_Grants.Grant_Reference;
      Lent, Revoked : Boolean;

      function Ask (Request : Unsigned_64; Answer : GX.Disposition; Detail : Unsigned_64;
                    Name : String) return Boolean is
         Msg : Message := NULL_MESSAGE;
         Tag : MessageTag;
      begin
         Msg.tag := (label => GX.Admission_Request, length => GX.Request_Words,
                     protocol => 0, ipc => <>, reserved => 0);
         Msg.words (GX.Request_Word) := Request;
         Msg.words (GX.Reference_Word) := CuBit.Grant_References.Encode (Grant);
         Msg.words (GX.Length_Word) := Unsigned_64 (Length);
         Tag := IPC.Call (Data_Endpoint.Value, Msg,
                          CuBit.Deadlines.Clock.Now + GX.Admission_Budget_Milliseconds);
         debugPrint ("platform-pool-check: gpio bootstrap request" & Unsigned_64'Image (Request) &
                     " disposition" & Unsigned_64'Image (Msg.words (GX.Disposition_Word)) &
                     " detail" & Unsigned_64'Image (Msg.words (GX.Detail_Word)) & LF);
         Check (Tag.label = GX.Admission_Request and then Tag.length = GX.Reply_Words
                and then Msg.words (GX.Reply_Request_Word) = Request
                and then Msg.words (GX.Disposition_Word) = GX.Disposition'Enum_Rep (Answer)
                and then Msg.words (GX.Detail_Word) = Detail, Name);
         return True;
      end Ask;
      Ignore : Boolean;
   begin
      R.Spans := 1;
      R.Segments := 2;
      R.HID := "INTC1057";
      R.STA := 16#0F#;
      R.Table := "DSDT";
      R.Table_ID := "CB-01   ";
      R.Path (1) := "_SB_";
      R.Path (2) := "GPI0";
      R.Items (1) := (Base => 16#FD6E_0000#, Length => 16#1_0000#,
                      From => GP.Memory32_Fixed, Writable => True);
      GP.Encode (R, Page (0 .. Length - 1));
      Page (GP.Version_Offset) := GP.Format_Version + 1;   --  first a defective payload
      CuBit.Memory_Grants.Create_Via_Capability
        (Data_Endpoint.Value, Page'Address, 1, False, Grant, Lent);
      Check (Lent, "the bootstrap payload is lent read-only");
      if Lent then
         Ignore := Ask (1, GX.Malformed, GP.Version_Offset,
                        "gpio bootstrap: a wrong version is MALFORMED at its byte");
         Page (GP.Version_Offset) := GP.Format_Version;
         Ignore := Ask (2, GX.No_Discovery_Failure, 0,
                        "gpio bootstrap: no P2SB discovery here: NO_DISCOVERY_FAILURE");
         Page (GP.Version_Offset) := GP.Format_Version + 1;   --  a repeat is answered from the record
         Ignore := Ask (2, GX.No_Discovery_Failure, 0,
                        "gpio bootstrap: the same request id gets the same answer");
         CuBit.Memory_Grants.Revoke (Grant, Revoked);
      end if;
   end;

   Check (Valid (Pool), "the pool capability is carried");
   if Valid (Pool) then
      declare
         D : constant IPC.Description := IPC.Describe (Pool);
      begin
         Check (D.Current and then D.Kind = ABI.Device_Memory
                and then D.Authority_Tag = PA.Platform_Pool_Tag,
                "its kernel tag is the platform pool");
         Check (D.Current and then D.Held.Read and then D.Held.Write and then not D.Held.Grant,
                "read-write without grant");
      end;
      Check (not Valid (Device_Memory'(CuBit.Handles.Control.Derive (Copy (Pool, Read_Only)))),
             "cannot be copied on");
   end if;

   --  Find edu through bus 0's read-only PCI configuration window, from
   --  the broker (no startup PCI configuration pregrant since the ACPI
   --  PCI-windows cohort).
   if Broker.Present then
      declare
         D : constant IPC.Description := IPC.Describe (Broker.Value);
      begin
         if D.Current then
            Client.Acquire (Run, Broker.Value, CuBit.Process_IDs.From_Word (D.Object),
              Bus_Request, CuBit.Deadlines.Clock.Now + Request_Deadline_Ms, Broker_Result);
         end if;
      end;
   end if;
   Check (Broker.Present and then Broker_Result.Status = Client.Acquired
          and then Client.Ready (Run),
          "bus 0 acquired from the PCI broker");
   if not Client.Ready (Run) then
      debugPrint ("platform-pool-check: broker result " & Broker_Result.Status'Image & LF);
   elsif CuBit.Handles.Devices.Map_Device
           (Client.Memory (Run), Client.Description (Run).First,
            PCI_Virtual, Q.Bus_Bytes / Page_Bytes, True)
   then
      Edu_Base := Edu_BAR0;
      Bus_Zero := Client.Description (Run).First;
   end if;
   debugPrint ("platform-pool-check: edu BAR0 at " & Hex (Edu_Base) & LF);
   Check (Edu_Base /= 0, "edu found");

   if Valid (Pool) and then Edu_Base /= 0 then
      --  The register block maps; loads and stores are plain accesses.
      declare
         Answer : constant Unsigned_64 :=
           CuBit.Handles.Platform_Pools.Map (Pool, Edu_Base, Edu_Virtual, 1, False, Why);
      begin
         First_Mapped := Answer = 0;
         Check (First_Mapped, "edu's register page maps through the pool");
         if not First_Mapped then
            debugPrint ("platform-pool-check: refused" & Unsigned_64'Image (Answer) &
                        " claimant " & Hex (Why (PA.Claimant)) & LF);
         end if;
      end;
      if First_Mapped then
         declare
            Identity : Unsigned_32 with Import, Volatile,
              Address => System'To_Address (Edu_Virtual);
            Liveness : Unsigned_32 with Import, Volatile,
              Address => System'To_Address (Edu_Virtual + Edu_Liveness);
            Seen : Unsigned_32;
         begin
            Seen := Identity;
            debugPrint ("platform-pool-check: edu identification " & Hex (Unsigned_64 (Seen)) & LF);
            Check ((Seen and Edu_Identity_Mask) = Edu_Identity_Low, "a plain load reads the register");
            Liveness := Liveness_Probe;
            Seen := Liveness;
            Check (Seen = not Liveness_Probe, "a plain store reaches the register");
         end;
      end if;

      Check (CuBit.Handles.Platform_Pools.Map
               (Pool, Edu_Base + Witness_Offset, Witness_Virtual, Witness_Pages, True, Why) = 0,
             "two pages at MCHBAR+0x7000 map (image-10 MBAR witness)");
      Check (CuBit.Handles.Platform_Pools.Map
               (Pool, Edu_Base + MCHBAR_Bytes - Page_Bytes, Probe_Virtual, 2, True, Why)
             = PA.Refusal_Word (PA.Outside_Pool),
             "a request across the MCHBAR block's end refused (OUTSIDE_POOL)");
      Check (CuBit.Handles.Platform_Pools.Map
               (Pool, Edu_Base + Community_Offset, Community_Virtual, 1, False, Why) = 0,
             "a GPIO community page maps through the pool");
      Check (CuBit.Handles.Platform_Pools.Map
               (Pool, Edu_Base + MCHBAR_Bytes, Window_Virtual, 1, True, Why) = 0,
             "a sideband window page between members maps");
      declare
         Mapped : Natural := 0;
      begin
         for K in 0 .. Growth_Mappings - 1 loop
            if CuBit.Handles.Platform_Pools.Map
                 (Pool, Edu_Base + Community_Offset,
                  Growth_Virtual + Unsigned_64 (K) * Page_Bytes, 1, True, Why) = 0
            then
               Mapped := Mapped + 1;
            end if;
         end loop;
         debugPrint ("platform-pool-check: community mapped at" & Natural'Image (Mapped) &
                     " addresses" & LF);
         Check (Mapped = Growth_Mappings,
                "300 mappings of one community page: the kernel's records grow");
      end;
      Check (CuBit.Handles.Platform_Pools.Map
               (Pool, Edu_Base + Community_Offset - Page_Bytes, Probe_Virtual, 2, True, Why)
             = PA.Refusal_Word (PA.Outside_Pool),
             "a request across the community's start refused (OUTSIDE_POOL)");
      Check (CuBit.Handles.Platform_Pools.Map
               (Pool, Edu_Base + Edu_BAR0_Bytes, Probe_Virtual, 1, True, Why)
             = PA.Refusal_Word (PA.Outside_Pool),
             "the page past the member refused (OUTSIDE_POOL)");
      Check (CuBit.Handles.Platform_Pools.Map (Pool, RAM_Page, Probe_Virtual, 1, True, Why)
             = PA.Refusal_Word (PA.Not_Covered),
             "a RAM page refused (NOT_COVERED)");
      --  PCI configuration space is firmware-reserved but never pool
      --  memory: bus 1's first function (regression: devmgr first put
      --  the MCFG allocation past bus 0 in the pool).
      Check (Bus_Zero /= 0
             and then CuBit.Handles.Platform_Pools.Map
                        (Pool, Bus_Zero + PCI_Bus_Bytes, Probe_Virtual, 1, True, Why)
                      = PA.Refusal_Word (PA.Not_Covered),
             "PCI configuration of bus 1 refused (NOT_COVERED)");
      --  NOT_COVERED, not OUTSIDE_POOL: PCI configuration pages are never
      --  pool-mappable (Physical_Ranges.Pool_Mappable_Bytes), so the map-time
      --  check refuses them before pool membership is consulted.

      --  The fixture driver binds edu and claims the BAR: from then on the
      --  same request is refused, naming it.
      declare
         Started : constant Unsigned_64 := CuBit.Deadlines.Clock.Now;
         Answer : Unsigned_64 := 0;
      begin
         loop
            Answer := CuBit.Handles.Platform_Pools.Map
              (Pool, Edu_Base, Again_Virtual, 1, True, Why);
            exit when Answer /= 0
              or else CuBit.Deadlines.Clock.Now - Started > Bind_Deadline_Ms;
            Pause;
         end loop;
         debugPrint ("platform-pool-check: after the bind: claimant " & Hex (Why (PA.Claimant)) &
                     " holder " & Hex (Why (PA.Holder)) &
                     " claim " & Hex (Why (PA.Claim_First_Page) * Page_Bytes) &
                     " pages" & Unsigned_64'Image (Why (PA.Claim_Pages)) & LF);
         Check (Answer = PA.Refusal_Word (PA.Claimed)
                and then Why (PA.Code) = PA.Refusal_Word (PA.Claimed)
                and then Why (PA.Claimant) /= 0 and then Why (PA.Claimant) /= Why (PA.Holder)
                and then Why (PA.Claim_First_Page) * Page_Bytes <= Edu_Base
                and then Why (PA.Member_First_Page) * Page_Bytes = Edu_Base,
                "a bound driver's BAR is refused, naming both claimants (CLAIMED)");
         --  The community is a Chipset_Shared member: the driver's claim
         --  over it is shared. ACPI's mapping stays (a revoked page would
         --  fault here and kill this program) and new ones are admitted.
         declare
            Pad : Unsigned_32 with Import, Volatile,
              Address => System'To_Address (Community_Virtual);
            Seen : constant Unsigned_32 := Pad;
         begin
            debugPrint ("platform-pool-check: community still mapped, reads " &
                        Hex (Unsigned_64 (Seen)) & LF);
            Check (True, "the shared community stays mapped after the bind");
         end;
         Answer := CuBit.Handles.Platform_Pools.Map
           (Pool, Edu_Base + Community_Offset, Shared_Virtual, 1, True, Why);
         Check (Answer = 0,
                "the community under the bound driver is still admitted (shared)");
      end;
   end if;

   if Failures = 0 then
      debugPrint ("TEST: PASS platform-pool" & LF);
   else
      debugPrint ("TEST: FAIL platform-pool" & LF);
   end if;

   --  The first mapping was revoked when the driver claimed the BAR: a
   --  touch now faults (the harness checks the kill).
   if First_Mapped then
      debugPrint ("platform-pool-check: touching the revoked mapping" & LF);
      declare
         Identity : Unsigned_32 with Import, Volatile,
           Address => System'To_Address (Edu_Virtual);
         Seen : constant Unsigned_32 := Identity;
      begin
         debugPrint ("platform-pool-check: revoked mapping still readable " &
                     Hex (Unsigned_64 (Seen)) & LF);
      end;
   end if;
end Main;
