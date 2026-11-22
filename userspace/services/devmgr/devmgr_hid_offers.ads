------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  devmgr's decision on a HID offer (CuBit.HID_Offer_Exchange, 16#0252#,
--  wire docs/hid-offer-wire.md): its frame, its receipt, its disposition,
--  the I2C target and GPIO input badges a queued offer carries, and when
--  the deferred launch may go on.
--
--  @description
--  The caller (devmgr's Serve_HID_Offer) refuses anything not in the
--  canonical frame (Canonical) and anything not through the firmware-data
--  badge before it copies the payload; it copies the read-only grant into
--  a zeroed buffer, returns the acquisition, then decodes it
--  (HID_Offer_Payload.Decode). Only then is the receipt key known:
--  (authenticated sender, payload epoch, request id) (Find_Receipt). A new
--  key is decided here (Decide), on facts the caller gathers:
--    * its PCI inventory (segment 0, each function with whether a driver
--      manifest binds it): the offer's I2C controller must be fully
--      resolved (_SEG, _BBN and _ADR known) and present, else
--      Unknown_Controller; present but bound to no driver (or ambiguously)
--      is Controller_Mismatch;
--    * the GPIO controllers it verified for use: the offer's GpioInt
--      controller (_HID, _UID kind and value, canonical path) must equal
--      one recorded for the same sender and epoch, else GPIO_Not_Admitted;
--      a pin that names no input (16#FFFF#) is GPIO_Not_Admitted too;
--    * the offers it queued: a second offer of one canonical device path
--      for one (sender, epoch) is Duplicate_Device.
--  A Queued verdict carries the inventory index, the GPIO record index and
--  both channel badges (I2C_Channel_Badges, GPIO_Channel_Badges). Which
--  dispositions are recorded is Recorded: a decided answer is never decided
--  again; Malformed, Not_Approved, Copy_Failed and Capacity decide nothing
--  and are not recorded (no key, or no room for it).
--  The deferred launch (Next_Step) waits for the GPIO controller's driver
--  to report ready, then for the I2C controller's driver; timing never
--  comes from the offer. Pure and SPARK; the caller launches.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.HID_Offer_Exchange;
with HID_Offer_Payload;
with I2C_Channel_Badges;
with GPIO_Channel_Badges;

package Devmgr_HID_Offers with Pure, SPARK_Mode is
   package HX renames CuBit.HID_Offer_Exchange;
   package HP renames HID_Offer_Payload;
   use type HX.Disposition;
   use type HP.UID_Kind;
   use type HP.PCI_Location;

   No_Request : constant := 0;

   --  The canonical request frame: exactly Request_Words words, no
   --  protocol or IPC flags (nothing offered, nothing accepted), the
   --  reserved field zero, every word past the request zero.
   type Word_Array is array (Natural range <>) of Unsigned_64;
   function Canonical
     (Length : Natural; Protocol : Unsigned_8; Flags_Clear : Boolean; Reserved : Unsigned_16;
      Words : Word_Array) return Boolean
   is (Length = HX.Request_Words and then Protocol = 0 and then Flags_Clear and then Reserved = 0
       and then (for all K in Words'Range => (if K >= HX.Request_Words then Words (K) = 0)));

   --  The request words, before anything is copied: a nonzero request id,
   --  a canonical grant reference, and a length the payload can have.
   --  Detail is the payload field Malformed names (the length field for a
   --  length past the largest payload, as the codec's own check).
   type Words_Verdict is record
      Valid  : Boolean := False;
      Detail : Unsigned_64 := 0;
   end record;
   function Check_Words
     (Request : Unsigned_64; Reference_Valid : Boolean; Length : Unsigned_64) return Words_Verdict
   is (if Request = No_Request or else not Reference_Valid or else Length = 0
       then (Valid => False, Detail => 0)
       elsif Length > HP.Max_Payload_Bytes
       then (Valid => False, Detail => HP.Length_Offset)
       else (Valid => True, Detail => 0));

   --  Which answers are recorded under the receipt key.
   function Recorded (Answer : HX.Disposition) return Boolean is
     (Answer in HX.Queued | HX.Unknown_Controller | HX.Controller_Mismatch
              | HX.GPIO_Not_Admitted | HX.Duplicate_Device);

   type Receipt is record
      Owner, Epoch, Request : Unsigned_64 := 0;
      Answer   : HX.Disposition := HX.Malformed;
      Detail   : Unsigned_64 := 0;
      Position : Unsigned_64 := 0;   --  nonzero only for Queued
   end record;
   type Receipt_Array is array (Positive range <>) of Receipt;

   --  The receipt recorded for (Owner, Epoch, Request), or 0.
   function Find_Receipt
     (Receipts : Receipt_Array; Owner, Epoch, Request : Unsigned_64) return Natural
   with Post =>
     (if Find_Receipt'Result /= 0 then
        Find_Receipt'Result in Receipts'Range
        and then Receipts (Find_Receipt'Result).Owner = Owner
        and then Receipts (Find_Receipt'Result).Epoch = Epoch
        and then Receipts (Find_Receipt'Result).Request = Request
      else
        (for all R of Receipts =>
           not (R.Owner = Owner and then R.Epoch = Epoch and then R.Request = Request)));

   --  One function of devmgr's PCI inventory (segment 0; devmgr enumerates
   --  no other segment) and whether a driver manifest binds it exactly.
   type PCI_Function is record
      Location     : HP.PCI_Location;
      Driver_Bound : Boolean := False;
   end record;
   type PCI_Function_Array is array (Positive range <>) of PCI_Function;

   type Controller_Outcome is
     (Bound_Controller,     --  present, and a driver binds it
      Unresolved_Location,  --  _SEG, _BBN or _ADR not known (mask other than 7)
      Absent_Location,      --  no such function in the inventory
      Unbound_Controller);  --  present, bound to no driver (or ambiguously)
   type Controller_Match is record
      Outcome     : Controller_Outcome := Unresolved_Location;
      At_Function : Natural := 0;   --  the inventory index (present outcomes)
   end record;

   function Match_Controller
     (PCI : HP.PCI_Observation; Inventory : PCI_Function_Array) return Controller_Match
   with Post =>
     (if Match_Controller'Result.Outcome in Bound_Controller | Unbound_Controller then
        Match_Controller'Result.At_Function in Inventory'Range
        and then HP.Fully_Resolved (PCI)
        and then Inventory (Match_Controller'Result.At_Function).Driver_Bound
                 = (Match_Controller'Result.Outcome = Bound_Controller))
     and then (if not HP.Fully_Resolved (PCI) then
                 Match_Controller'Result.Outcome = Unresolved_Location);

   --  A GPIO controller devmgr verified for use (its bootstrap admission),
   --  as acpi.svc reported it.
   type GPIO_Record is record
      Owner, Epoch : Unsigned_64 := 0;
      HID          : HP.Text_8 := [others => HP.NUL];
      UID_Kind     : HP.UID_Kind := HP.No_UID;
      UID          : Unsigned_64 := 0;
      Path         : HP.Canonical_Path;
   end record;
   type GPIO_Record_Array is array (Positive range <>) of GPIO_Record;

   function Same_Path (Left, Right : HP.Canonical_Path) return Boolean is
     (Left.Count = Right.Count
      and then (for all K in 1 .. Left.Count => Left.Names (K) = Right.Names (K)));

   function GPIO_Matches (Offer : HP.Request; Owner : Unsigned_64; Item : GPIO_Record) return Boolean is
     (Item.Owner = Owner and then Item.Epoch = Offer.Epoch
      and then Item.HID = Offer.GPIO_HID and then Item.UID_Kind = Offer.GPIO_UID_Kind
      and then Item.UID = Offer.GPIO_UID and then Same_Path (Item.Path, Offer.GPIO_Path));

   --  The verified GPIO controller the offer's GpioInt names, or 0.
   function Find_GPIO
     (Offer : HP.Request; Owner : Unsigned_64; Records : GPIO_Record_Array) return Natural
   with Post =>
     (if Find_GPIO'Result /= 0 then
        Find_GPIO'Result in Records'Range
        and then GPIO_Matches (Offer, Owner, Records (Find_GPIO'Result))
      else (for all R of Records => not GPIO_Matches (Offer, Owner, R)));

   --  An offer already queued (Duplicate_Device is per (sender, epoch,
   --  canonical device path)).
   type Queued_Offer is record
      Owner, Epoch : Unsigned_64 := 0;
      Device_Path  : HP.Canonical_Path;
   end record;
   type Queued_Offer_Array is array (Positive range <>) of Queued_Offer;

   function Same_Device (Item : Queued_Offer; Owner, Epoch : Unsigned_64; Path : HP.Canonical_Path)
     return Boolean
   is (Item.Owner = Owner and then Item.Epoch = Epoch and then Same_Path (Item.Device_Path, Path));

   --  The ACPI pin that names no input.
   No_Pin : constant HP.Half := GPIO_Channel_Badges.No_Pin;

   --  The interrupt input of the offer's GpioInt (Pin /= No_Pin).
   function GPIO_Input (Offer : HP.Request) return GPIO_Channel_Badges.Interrupt_Input
   with Pre => Offer.Pin /= No_Pin;

   --  The I2C target of the offer's I2cSerialBus.
   function I2C_Target (Offer : HP.Request) return I2C_Channel_Badges.Target
   with Pre => Offer.Address <= HP.Ten_Bit_Last;

   type Verdict is record
      Answer     : HX.Disposition := HX.Malformed;
      Detail     : Unsigned_64 := 0;
      Controller : Natural := 0;       --  inventory index (Queued, Controller_Mismatch)
      GPIO       : Natural := 0;       --  GPIO record index (Queued)
      I2C_Badge  : Unsigned_64 := 0;   --  I2C_Channel_Badges word (Queued)
      GPIO_Badge : Unsigned_64 := 0;   --  GPIO_Channel_Badges word (Queued)
   end record;

   --  A decoded offer through the firmware-data badge, whose key has no
   --  receipt yet. Malformed only when its I2C target cannot be a badge
   --  (the codec refuses such an offer first). Never Not_Approved,
   --  Capacity or Copy_Failed: those are the caller's.
   function Decide
     (Offer     : HP.Request;
      Owner     : Unsigned_64;
      Inventory : PCI_Function_Array;
      GPIOs     : GPIO_Record_Array;
      Queue     : Queued_Offer_Array) return Verdict
   with Post =>
     Decide'Result.Answer in HX.Queued | HX.Malformed | HX.Unknown_Controller
                           | HX.Controller_Mismatch | HX.GPIO_Not_Admitted | HX.Duplicate_Device
     and then (if not HP.Fully_Resolved (Offer.PCI) then
                 Decide'Result.Answer = HX.Unknown_Controller)
     and then (if Decide'Result.Answer = HX.Queued then
                 Decide'Result.Controller in Inventory'Range
                 and then Inventory (Decide'Result.Controller).Driver_Bound
                 and then Decide'Result.GPIO in GPIOs'Range
                 and then GPIO_Matches (Offer, Owner, GPIOs (Decide'Result.GPIO))
                 and then (for all Q of Queue =>
                             not Same_Device (Q, Owner, Offer.Epoch, Offer.Device_Path))
                 and then I2C_Channel_Badges.Valid_Word (Decide'Result.I2C_Badge)
                 and then GPIO_Channel_Badges.Valid_Word (Decide'Result.GPIO_Badge)
                 and then Decide'Result.Detail = 0);

   --  Where a queued offer's launch stands, from its two drivers' reports.
   type Driver_Report is
     (Not_Started,   --  not launched yet (or its launch is still queued)
      Starting,      --  launched, no report yet
      Serving,       --  reported OP_READY
      Declined);     --  reported OP_NOT_PRESENT, or its launch was refused
   type Launch_Step is
     (Await_GPIO_Driver,        --  the GPIO controller's driver has not registered
      Await_Controller_Driver,  --  the I2C controller's driver has not registered
      Launch_HID,               --  both serve: the HID driver may launch
      Abandon);                 --  a driver declined: the offer launches nothing (logged)
   function Next_Step (GPIO_Driver, Controller_Driver : Driver_Report) return Launch_Step is
     (if GPIO_Driver = Declined or else Controller_Driver = Declined then Abandon
      elsif GPIO_Driver /= Serving then Await_GPIO_Driver
      elsif Controller_Driver /= Serving then Await_Controller_Driver
      else Launch_HID);

   ---------------------------------------------------------------------------
   --  Which driver process a binding names (SUP-001). Several drivers may
   --  register one role (an I2C controller driver per controller: the
   --  laptop's 00:15.0 and 00:15.1); the kernel's registry keeps only the
   --  last. The I2C binding of an offer must reach the driver of THAT
   --  offer's controller, so devmgr names it from its own launch records:
   --  the one serving driver launched for the controller's location.
   ---------------------------------------------------------------------------
   No_Process_Word : constant Unsigned_64 := 0;
   type Launched_Driver is record
      Process  : Unsigned_64 := No_Process_Word;   --  its identity word
      Location : HP.PCI_Location;                  --  the function it was launched for
      Serving  : Boolean := False;                 --  it reported OP_READY
   end record;
   type Launched_Driver_Array is array (Positive range <>) of Launched_Driver;

   function Serves_At (Item : Launched_Driver; Location : HP.PCI_Location) return Boolean is
     (Item.Serving and then Item.Process /= No_Process_Word and then Item.Location = Location);

   --  The process serving Location: the one serving launch record for it;
   --  No_Process_Word when there is none, or more than one (never guess).
   function Provider_At
     (Drivers : Launched_Driver_Array; Location : HP.PCI_Location) return Unsigned_64
   with Post =>
     (if Provider_At'Result /= No_Process_Word then
        (for some I in Drivers'Range =>
           Serves_At (Drivers (I), Location) and then Drivers (I).Process = Provider_At'Result)
        and then
        (for all I in Drivers'Range =>
           (if Serves_At (Drivers (I), Location) then Drivers (I).Process = Provider_At'Result)));

end Devmgr_HID_Offers;
