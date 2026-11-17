------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The ACPI launch regression's stand-in for acpi.svc (docs/device-
--  matching.md "ACPI launch"; tests/headless, gpio-launch).
--
--  @description
--  On the gpio-launch test profile (q35 + edu with the CBTEST/GPIOBOOT
--  firmware identity: devmgr's quirk table runs P2SB discovery, which
--  does not find on q35, and names the test controller CBIT1057), it
--  sends devmgr the GPIO bootstrap observation acpi.svc would send: the
--  controller \_SB_.GPI0, _HID CBIT1057, two 64 KiB Memory32Fixed spans in
--  a device-memory root. devmgr must admit both and answer at once; a repeat of
--  the request id gets the same answer, another request is
--  ALREADY_ADMITTED. devmgr records the controller but launches nothing
--  yet: the driver waits for Consumer_Initialization_Complete (acpi.svc's
--  device _INI phase is over; CuBit.Consumer_Initialization_Exchange).
--  This program waits Initialization_Pause_Us (logstore is up by then, so
--  a launch could have happened), then declares: an unknown status
--  (MALFORMED), Incomplete for epoch 2 (recorded, launches nothing),
--  Completed for epoch 2 (CONFLICTING: no promotion), then Completed for
--  epoch 1 and its repeat (one receipt, one launch). Before those:
--  non-canonical frames (four words, a stray word, reserved bits, a
--  frame accepting a capability) and epoch 0 are MALFORMED, and Completed
--  for epoch 3 (nothing observed there) releases nothing. devmgr then launches gpio-launch-check.drv from its service
--  loop with exactly those spans; the harness checks the order and the
--  driver's lines.
------------------------------------------------------------------------------
pragma Ada_2022;
with Wait_ABI;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles.IPC;
with CuBit.Handles.Startup;
with CuBit.Firmware_Data_Exchange;
with CuBit.GPIO_Bootstrap_Exchange;
with CuBit.Consumer_Initialization_Exchange;
with CuBit.HID_Offer_Exchange;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CuBit.Deadlines.Clock;
with GPIO_Bootstrap_Payload;
with HID_Offer_Payload;

procedure Main is
   use ASCII;
   package IPC renames CuBit.Handles.IPC;
   package HS renames CuBit.Handles.Startup;
   package FDX renames CuBit.Firmware_Data_Exchange;
   package GX renames CuBit.GPIO_Bootstrap_Exchange;
   package CX renames CuBit.Consumer_Initialization_Exchange;
   package GP renames GPIO_Bootstrap_Payload;
   package HX renames CuBit.HID_Offer_Exchange;
   package HP renames HID_Offer_Payload;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("gpio-bootstrap-check: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         debugPrint ("gpio-bootstrap-check: " & Name & " FAIL" & LF);
      end if;
   end Check;

   Page_Bytes : constant := FDX.Page_Bytes;
   Span_Count : constant := 2;
   --  Two 64 KiB spans in a device-memory root of the 256 MiB q35 guest,
   --  above RAM and below ECAM (0xB0000000) and the PCI window, so they
   --  touch no RAM, BAR, bridge window or pool member (descending, as the
   --  IdeaPad's communities 0 and 1).
   Span_Bytes  : constant := 16#1_0000#;
   First_Base  : constant := 16#2001_0000#;
   Second_Base : constant := 16#2000_0000#;
   Observed_Epoch : constant := 1;
   --  How long "device _INI" takes here: long enough for logstore to run,
   --  so a launch not held back by the completion would show before it.
   Initialization_Pause_Us : constant := 3_000_000;
   Unknown_Status : constant := 16#99#;
   Other_Epoch : constant := 2;
   Unobserved_Epoch : constant := 3;
   No_Epoch : constant := 0;
   Completed_Word : constant Unsigned_64 := CX.Phase_Status'Enum_Rep (CX.Completed);

   Data_Endpoint : constant HS.Optional_Endpoint :=
     HS.Optional_Endpoint_Named (FDX.Data_Label ("hardware"));

   Page : GP.Byte_Array (0 .. Page_Bytes - 1) := [others => 0]
   with Alignment => Page_Bytes;
   R : GP.Request;
   Length : constant Natural := GP.Encoded_Length (Span_Count);
   Grant : CuBit.Memory_Grants.Grant_Reference;
   Lent, Revoked : Boolean;

   procedure Ask (Request : Unsigned_64; Answer : GX.Disposition; Detail : Unsigned_64;
                  Name : String) is
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
      debugPrint ("gpio-bootstrap-check: request" & Unsigned_64'Image (Request) &
                  " disposition" & Unsigned_64'Image (Msg.words (GX.Disposition_Word)) &
                  " detail" & Unsigned_64'Image (Msg.words (GX.Detail_Word)) & LF);
      Check (Tag.label = GX.Admission_Request and then Tag.length = GX.Reply_Words
             and then Msg.words (GX.Reply_Request_Word) = Request
             and then Msg.words (GX.Disposition_Word) = GX.Disposition'Enum_Rep (Answer)
             and then Msg.words (GX.Detail_Word) = Detail, Name);
   end Ask;
   --  How a frame departs from the canonical one (the negative cases).
   type Frame_Defect is (Canonical, Long, Stray_Word, Reserved_Bits, Accepting);

   --  No_Launches: what every receipt but the releasing one reports.
   No_Launches : constant Unsigned_64 := 0;
   One_Launch  : constant Unsigned_64 := 1;
   procedure Complete (Request, Epoch, Status : Unsigned_64; Answer : CX.Receipt; Name : String;
                       Defect : Frame_Defect := Canonical; Released : Unsigned_64 := No_Launches) is
      Stray : constant := 7;
      Msg : Message := NULL_MESSAGE;
      Tag : MessageTag;
      Ignore : IPC.Carried;
   begin
      Msg.tag := (label => CX.Consumer_Initialization_Complete,
                  length => (if Defect = Long then CX.Request_Words + 1 else CX.Request_Words),
                  protocol => 0, ipc => <>, reserved => (if Defect = Reserved_Bits then 1 else 0));
      Msg.words (CX.Request_Word) := Request;
      Msg.words (CX.Epoch_Word) := Epoch;
      Msg.words (CX.Status_Word) := Status;
      if Defect = Stray_Word then
         Msg.words (CX.Request_Words) := Stray;
      end if;
      if Defect = Accepting then
         Tag := IPC.Call_Accepting (Data_Endpoint.Value, Msg,
                                    CuBit.Deadlines.Clock.Now + CX.Completion_Budget_Milliseconds, Ignore);
      else
         Tag := IPC.Call (Data_Endpoint.Value, Msg,
                          CuBit.Deadlines.Clock.Now + CX.Completion_Budget_Milliseconds);
      end if;
      Check (Tag.label = CX.Consumer_Initialization_Complete and then Tag.length = CX.Reply_Words
             and then Msg.words (CX.Reply_Request_Word) = Request
             and then Msg.words (CX.Receipt_Word) = CX.Receipt'Enum_Rep (Answer)
             and then Msg.words (CX.Released_Word) = Released, Name);
   end Complete;

   procedure Pause is
      Now_Us : constant Unsigned_64 :=
        CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Read_Monotonic_Microseconds);
      Ignore : Unsigned_64;
   begin
      Ignore := CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Wait, Wait_ABI.Timer_Sources, Wait_ABI.Forever, Now_Us + Initialization_Pause_Us);
   end Pause;
   ---------------------------------------------------------------------------
   --  HID offers (CuBit.HID_Offer_Exchange, 16#0252#): acpi.svc's offer of
   --  a PNP0C50 candidate. The touchpad stand-in sits on the edu device
   --  (run.sh places it at 00:15.0, bound to generic-check.drv), its
   --  GpioInt on the test GPIO controller CBIT1057 \_SB_.GPI0 admitted in
   --  epoch 1 above.
   ---------------------------------------------------------------------------
   Controller_ADR : constant := 16#15_0000#;   --  edu at 00:15.0
   Absent_ADR     : constant := 16#1E_0000#;   --  no function at 00:1E.0 on q35
   Host_Bridge_ADR : constant := 0;            --  00:00.0: present, no driver
   Touchpad_Address : constant := 16#15#;
   Touchpad_Hz      : constant := 400_000;
   Touchpad_Pin     : constant := 16#4C#;
   Descriptor_Register : constant := 16#20#;
   Bad_Version : constant := 2;
   No_Grant : constant Unsigned_64 :=
     CuBit.Grant_References.Encode ((slot => CuBit.Grant_References.Maximum_Slot,
                                     generation => CuBit.Grant_References.Maximum_Generation));
   Offer_Page : HP.Byte_Array (0 .. Page_Bytes - 1) := [others => 0]
   with Alignment => Page_Bytes;

   function Segments (A, B : String; C : String := ""; D : String := "") return HP.Canonical_Path is
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
   end Segments;

   function Touchpad return HP.Request is
      T : HP.Request;
   begin
      T.Epoch := Observed_Epoch;
      T.Catalog_Revision := 1;
      T.Observation_Sequence := 1;
      T.PCI := (Has_SEG => True, Has_BBN => True, Has_ADR => True, SEG => 0, BBN => 0, ADR => Controller_ADR);
      T.Maximum_Hz := Touchpad_Hz;
      T.Address := Touchpad_Address;
      T.Descriptor_Register := Descriptor_Register;
      T.Pin := Touchpad_Pin;
      T.IRQ_Polarity := HP.Active_Low;
      T.GPIO_UID_Kind := HP.Integer_UID;
      T.GPIO_UID := 0;
      T.GPIO_HID := "CBIT1057";
      T.Table := "SSDT";
      T.Table_ID := "GPIOBOOT";
      T.Device_Path := Segments ("_SB_", "PCI0", "I2C1", "TPD0");
      T.I2C_Path := Segments ("_SB_", "PCI0", "I2C1");
      T.GPIO_Path := Segments ("_SB_", "GPI0");
      return T;
   end Touchpad;

   --  How the payload departs from the encoded one (the malformed cases).
   type Payload_Defect is (Whole, Version, Length_Field, Compatible);

   procedure Offer (Request : Unsigned_64; Item : HP.Request; Answer : HX.Disposition;
                    Detail, Position : Unsigned_64; Name : String;
                    Defect : Frame_Defect := Canonical; Damage : Payload_Defect := Whole;
                    Reference : Unsigned_64 := 0) is
      Stray : constant := 7;
      Size : constant Natural := HP.Encoded_Length (Item);
      Msg : Message := NULL_MESSAGE;
      Tag : MessageTag;
      Offer_Grant : CuBit.Memory_Grants.Grant_Reference;
      Lent_Offer, Gone : Boolean;
      Ignore : IPC.Carried;
   begin
      Offer_Page := [others => 0];
      HP.Encode (Item, Offer_Page (0 .. Size - 1));
      case Damage is
         when Whole => null;
         when Version => Offer_Page (HP.Version_Offset) := Bad_Version;
         when Length_Field => Offer_Page (HP.Length_Offset) := Offer_Page (HP.Length_Offset) + 1;
         when Compatible => Offer_Page (HP.Compatible_Offset) := Character'Pos ('X');
      end case;
      CuBit.Memory_Grants.Create_Via_Capability
        (Data_Endpoint.Value, Offer_Page'Address, 1, False, Offer_Grant, Lent_Offer);
      if not Lent_Offer then
         Check (False, Name & " (the offer page is lent)");
         return;
      end if;
      Msg.tag := (label => HX.Offer_Request,
                  length => (if Defect = Long then HX.Request_Words + 1 else HX.Request_Words),
                  protocol => 0, ipc => <>, reserved => (if Defect = Reserved_Bits then 1 else 0));
      Msg.words (HX.Request_Word) := Request;
      Msg.words (HX.Reference_Word) :=
        (if Reference /= 0 then Reference else CuBit.Grant_References.Encode (Offer_Grant));
      Msg.words (HX.Length_Word) := Unsigned_64 (Size);
      if Defect = Stray_Word then
         Msg.words (HX.Request_Words) := Stray;
      end if;
      if Defect = Accepting then
         Tag := IPC.Call_Accepting (Data_Endpoint.Value, Msg,
                                    CuBit.Deadlines.Clock.Now + HX.Offer_Budget_Milliseconds, Ignore);
      else
         Tag := IPC.Call (Data_Endpoint.Value, Msg, CuBit.Deadlines.Clock.Now + HX.Offer_Budget_Milliseconds);
      end if;
      CuBit.Memory_Grants.Revoke (Offer_Grant, Gone);
      debugPrint ("gpio-bootstrap-check: offer" & Unsigned_64'Image (Request) &
                  " disposition" & Unsigned_64'Image (Msg.words (HX.Disposition_Word)) &
                  " detail" & Unsigned_64'Image (Msg.words (HX.Detail_Word)) &
                  " position" & Unsigned_64'Image (Msg.words (HX.Queue_Position_Word)) & LF);
      Check (Tag.label = HX.Offer_Request and then Tag.length = HX.Reply_Words
             and then Msg.words (HX.Reply_Request_Word) = Request
             and then Msg.words (HX.Disposition_Word) = HX.Disposition'Enum_Rep (Answer)
             and then Msg.words (HX.Detail_Word) = Detail
             and then Msg.words (HX.Queue_Position_Word) = Position, Name);
   end Offer;

   procedure Offer_HIDs is
      T : HP.Request;
   begin
      --  Refused before anything is decided, never recorded.
      Offer (40, Touchpad, HX.Malformed, 0, 0, "a four-word offer frame is MALFORMED", Long);
      Offer (41, Touchpad, HX.Malformed, 0, 0, "a stray fourth offer word is MALFORMED", Stray_Word);
      Offer (42, Touchpad, HX.Malformed, 0, 0, "reserved offer tag bits are MALFORMED", Reserved_Bits);
      Offer (43, Touchpad, HX.Malformed, 0, 0, "an offer frame accepting a capability is MALFORMED", Accepting);
      Offer (0, Touchpad, HX.Malformed, 0, 0, "offer request id 0 is MALFORMED");
      Offer (44, Touchpad, HX.Malformed, HP.Version_Offset, 0, "offer version 2 is MALFORMED at byte 0",
             Damage => Version);
      Offer (45, Touchpad, HX.Malformed, HP.Length_Offset, 0, "a wrong length field is MALFORMED at byte 4",
             Damage => Length_Field);
      Offer (46, Touchpad, HX.Malformed, HP.Compatible_Offset, 0,
             "another compatible ID is MALFORMED at byte 160", Damage => Compatible);
      T := Touchpad;
      T.Epoch := No_Epoch;
      Offer (47, T, HX.Malformed, HP.Epoch_Offset, 0, "an offer in epoch 0 is MALFORMED at byte 8");
      Offer (48, Touchpad, HX.Copy_Failed, 0, 0, "an offer whose grant is not the sender's is COPY_FAILED",
             Reference => No_Grant);
      --  Decided and recorded.
      T := Touchpad;
      T.PCI := (Has_SEG => True, Has_BBN => True, Has_ADR => False, SEG => 0, BBN => 0, ADR => 0);
      Offer (31, T, HX.Unknown_Controller, 0, 0, "an unresolved _ADR is UNKNOWN_CONTROLLER");
      T := Touchpad;
      T.PCI.ADR := Absent_ADR;
      Offer (32, T, HX.Unknown_Controller, 0, 0, "an absent PCI function is UNKNOWN_CONTROLLER");
      T := Touchpad;
      T.PCI.ADR := Host_Bridge_ADR;
      Offer (33, T, HX.Controller_Mismatch, 0, 0, "a function no driver binds is CONTROLLER_MISMATCH");
      T := Touchpad;
      T.GPIO_HID := "CBIT1058";
      Offer (34, T, HX.GPIO_Not_Admitted, 0, 0, "another GPIO controller is GPIO_NOT_ADMITTED");
      T := Touchpad;
      T.Epoch := Other_Epoch;
      Offer (35, T, HX.GPIO_Not_Admitted, 0, 0, "an epoch with no admitted GPIO controller is GPIO_NOT_ADMITTED");
      debugPrint ("gpio-bootstrap-check: offering the touchpad" & LF);
      Offer (30, Touchpad, HX.Queued, 0, 1, "the touchpad offer is QUEUED at position 1");
      Offer (30, Touchpad, HX.Queued, 0, 1, "the same offer again gets the same answer");
      Offer (36, Touchpad, HX.Duplicate_Device, 0, 0, "a second offer of the touchpad is DUPLICATE_DEVICE");
      Offer (31, Touchpad, HX.Unknown_Controller, 0, 0,
             "a recorded request id keeps its answer (never decided twice)");
   end Offer_HIDs;

begin
   debugPrint ("gpio-bootstrap-check: starting" & LF);
   if not Data_Endpoint.Present then
      debugPrint ("gpio-bootstrap-check: no firmware-data endpoint" & LF);
      debugPrint ("TEST: FAIL gpio-launch" & LF);
      return;
   end if;
   R.Spans := Span_Count;
   R.Segments := 2;
   R.HID := "CBIT1057";
   R.UID_Type := GP.Integer_UID;
   R.UID := 0;
   R.STA := 16#0F#;
   R.Table := "DSDT";
   R.Table_ID := "GPIOBOOT";
   R.Epoch := Observed_Epoch;
   R.Path (1) := "_SB_";
   R.Path (2) := "GPI0";
   --  The controller's parent line: GSI 10 (free on q35), level, active
   --  low, shared, as a GPIO controller's _CRS Interrupt declares it.
   R.IRQ := (Present => True, Mode => GP.Level, Level_Of => GP.Active_Low, Share => GP.Shared,
             Wakes => GP.Not_Wake_Capable, GSI => 10);
   R.Items (1) := (Base => First_Base, Length => Span_Bytes, From => GP.Memory32_Fixed, Writable => True);
   R.Items (2) := (Base => Second_Base, Length => Span_Bytes, From => GP.Memory32_Fixed, Writable => True);
   GP.Encode (R, Page (0 .. Length - 1));
   CuBit.Memory_Grants.Create_Via_Capability
     (Data_Endpoint.Value, Page'Address, 1, False, Grant, Lent);
   Check (Lent, "the bootstrap payload is lent read-only");
   if Lent then
      Ask (1, GX.Admitted, Span_Count, "devmgr admits the test controller's two spans");
      Ask (1, GX.Admitted, Span_Count, "the same request id gets the same answer");
      Ask (2, GX.Already_Admitted, 0, "a second admission is ALREADY_ADMITTED");
      CuBit.Memory_Grants.Revoke (Grant, Revoked);
   end if;
   --  "Device _INI" runs; the controller's driver must not start.
   Pause;
   --  Non-canonical frames, each carrying Completed for the observed
   --  epoch: refused before anything is decided, so they release nothing.
   Complete (20, Observed_Epoch, Completed_Word, CX.Malformed, "a four-word frame is MALFORMED", Long);
   Complete (21, Observed_Epoch, Completed_Word, CX.Malformed, "a stray fourth word is MALFORMED", Stray_Word);
   Complete (22, Observed_Epoch, Completed_Word, CX.Malformed, "reserved tag bits are MALFORMED", Reserved_Bits);
   Complete (23, Observed_Epoch, Completed_Word, CX.Malformed,
             "a frame accepting a capability is MALFORMED", Accepting);
   Complete (24, No_Epoch, Completed_Word, CX.Malformed, "COMPLETED for epoch 0 is MALFORMED");
   Complete (10, Observed_Epoch, Unknown_Status, CX.Malformed,
             "a completion with an unknown status is MALFORMED");
   Complete (12, Other_Epoch, CX.Phase_Status'Enum_Rep (CX.Incomplete), CX.Received,
             "INCOMPLETE is RECEIVED and launches nothing");
   Complete (13, Other_Epoch, CX.Phase_Status'Enum_Rep (CX.Completed), CX.Conflicting,
             "COMPLETED after INCOMPLETE for one epoch is CONFLICTING");
   --  Admitted in epoch 1: COMPLETED for epoch 3 releases nothing.
   Complete (14, Unobserved_Epoch, Completed_Word, CX.Received,
             "COMPLETED for an epoch nothing was observed in is RECEIVED, releases nothing");
   debugPrint ("gpio-bootstrap-check: sending consumer initialization complete" & LF);
   Complete (11, Observed_Epoch, CX.Phase_Status'Enum_Rep (CX.Completed), CX.Received,
             "consumer initialization COMPLETED is RECEIVED and reports one launch released",
             Released => One_Launch);
   Complete (11, Observed_Epoch, CX.Phase_Status'Enum_Rep (CX.Completed), CX.Received,
             "the same completion again gets the same receipt and the same released count",
             Released => One_Launch);
   Offer_HIDs;
   if Failures = 0 then
      debugPrint ("gpio-bootstrap-check: admitted, released and offered; devmgr launches the driver" & LF);
   else
      debugPrint ("TEST: FAIL gpio-launch" & LF);
   end if;
end Main;
