------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The power-source contract (PWR-004, DEV-002 style): one typed record for
--  every source of power the machine has, and the wire that carries it from
--  power.svc, the aggregator, to the holders granted a read of it.
--
--  @description
--  A source is an internal battery or an AC adapter (ACPI today: _BIX/_BST
--  and ACPI0003 _PSR through acpi.svc), later a UPS (USB HID Power Device,
--  serial, NUT) or an external battery bank. Each reports, in its own
--  units, what the contract names here; power.svc maps it and serves the
--  result. Sources only report: nothing here, nor holding a read of it,
--  carries any power authority (shutdown, suspend, the UPS's own outlets).
--
--  Unknown is a value. A quantity's Known is False when the source did not
--  say, said "unknown", or its report could not be read, and its Value is
--  then always zero (the predicate): no zero ever stands in for a reading.
--  The states, flags, freshness and runtime have their own Unknown.
--
--  Units: energy mWh, power mW (Rate is a magnitude; State gives its
--  direction), voltage mV, charge and load per mille, runtime seconds. A
--  source reporting charge in mAh is converted through its design voltage
--  (energy) and its present voltage (rate) by its adapter, or left Unknown;
--  Charge and Left are taken in the source's own units, so they never
--  depend on a conversion.
--
--  Transport: power.svc's endpoint, badged Observer_Tag. procmgr mints the
--  badge for catalog role Observer_Role ("power-sources") only for programs
--  the trusted startup plan starts, and only for a manifest that requests
--  it. Holding it reads; it never asks power.svc to do anything else.
--
--  Summary_Label (Summary_Request_Words words, no capability carried):
--    Request_Word  the reader's request id (never zero)
--    Seen_Word     the generation the reader last saw (0 at first)
--    Mode_Word     Summary_Mode'Enum_Rep:
--      Read_Now: power.svc reads its sources again (query on read, the
--        sources' own freshness), then answers at once;
--      Watch: answered at once when power.svc's generation differs from
--        Seen, else kept (one per reader; a newer one answers the kept one
--        Superseded first) and answered when the snapshot changes. power.svc
--        never polls: the snapshot changes only when a source reports or a
--        reader asks Read_Now.
--  Reply, label Summary_Label, Reply_Words words:
--    Reply_Request_Word  the request id
--    Generation_Word     the snapshot's generation (never 0 once read)
--    Payload_Word        Summary_A
--    Payload_Word + 1    Summary_B
--
--  Source_Label (Source_Request_Words words): one page of one source.
--    Request_Word, Expected_Word (the summary's generation), Index_Word
--    (1 .. the summary's Sources), Page_Word (Page'Enum_Rep).
--  Reply, label Source_Label, Reply_Words words: the request id, the
--  generation, the page's two payload words (Page_Words). A reader reads
--  every page against one generation; a snapshot that changed meanwhile
--  is refused Stale_Generation, and the reader starts again.
--
--  Refusals: label Refused_Label, Refusal_Words words: the request id and
--  Refusal'Enum_Rep.
--
--  Every word on the wire is at most Integer_64'Last (a CCL integer).
--  Decoders are total: they accept exactly what the encoders write.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Power_Sources with Pure, SPARK_Mode is

   ---------------------------------------------------------------------------
   --  The contract.
   ---------------------------------------------------------------------------
   type Source_Kind is
     (Internal_Battery,   --  a battery inside the machine (ACPI PNP0C0A)
      AC_Adapter,         --  mains through an adapter (ACPI ACPI0003)
      UPS,                --  an uninterruptible power supply (later)
      External_Battery);  --  a battery bank outside the machine (later)

   type Source_State is
     (Unknown,
      Online,        --  a mains-side source supplying (adapter in use; UPS on line)
      Offline,       --  a mains-side source not supplying
      On_Battery,    --  a UPS feeding its load from its battery
      Charging,      --  a battery taking charge
      Discharging,   --  a battery supplying
      Full,          --  a battery neither charging nor discharging, at full charge
      Not_Charging); --  a battery neither charging nor discharging below full
                     --  (a charge limit, or the charger holding it)

   --  A condition the source reports, or Unknown when it cannot say.
   type Flag is (Unknown, Clear, Set);

   --  Whether the values are the source's latest within its age limit.
   --  Stale values are the last ones read, shown as such.
   type Freshness is (Unknown, Fresh, Stale);

   type Runtime_Kind is
     (Unknown,
      To_Empty,   --  how long the charge lasts at the present rate
      To_Full);   --  how long until full at the present rate

   --  Why a UPS last went to battery (UPS sources; Unknown elsewhere).
   type Transfer_Reason is
     (Unknown, None_Recorded, Input_Voltage, Input_Frequency, Self_Test, Other);

   --  What the machine runs on (the summary).
   type Supply is (Unknown, Mains, Battery);

   Maximum_Quantity : constant := 2 ** 31 - 1;
   subtype Quantity_Value is Unsigned_32 range 0 .. Maximum_Quantity;
   type Quantity is record
      Known : Boolean := False;
      Value : Quantity_Value := 0;
   end record
     with Predicate => Known or else Value = 0;
   Unknown_Quantity : constant Quantity := (Known => False, Value => 0);
   function Known_Quantity (Value : Quantity_Value) return Quantity is ((Known => True, Value => Value));

   Whole : constant := 1000;
   subtype Per_Mille_Value is Unsigned_32 range 0 .. Whole;
   type Per_Mille is record
      Known : Boolean := False;
      Value : Per_Mille_Value := 0;
   end record
     with Predicate => Known or else Value = 0;
   Unknown_Per_Mille : constant Per_Mille := (Known => False, Value => 0);

   subtype Seconds_Value is Unsigned_32 range 0 .. Maximum_Quantity;
   type Runtime is record
      Kind    : Runtime_Kind := Unknown;
      Seconds : Seconds_Value := 0;
   end record
     with Predicate => Kind /= Unknown or else Seconds = 0;
   Unknown_Runtime : constant Runtime := (Kind => Unknown, Seconds => 0);

   --  A stable name for the source ("battery-1", "ac-adapter"): printable
   --  ASCII, at most Name_Capacity characters, padded with spaces.
   Name_Capacity : constant := 16;
   subtype Name_Length is Natural range 0 .. Name_Capacity;
   subtype Name_Character is Character range ' ' .. '~';
   subtype Name_Position is Positive range 1 .. Name_Capacity;
   type Name_Text is array (Name_Position) of Character;
   type Source_Name is record
      Length : Name_Length := 0;
      Text   : Name_Text := [others => ' '];
   end record
     with Predicate => (for all C of Source_Name.Text => C in Name_Character);
   function Image (Name : Source_Name) return String is
     (String (Name.Text (1 .. Name.Length)));

   type Conditions is record
      Low, Replace, Overload, Bypass : Flag := Unknown;
   end record;

   type Source is record
      Kind       : Source_Kind := Internal_Battery;
      Name       : Source_Name;
      State      : Source_State := Unknown;
      Condition  : Conditions;
      Fresh      : Freshness := Unknown;
      Charge     : Per_Mille;
      Left       : Runtime;
      Energy_Now, Energy_Full, Energy_Design : Quantity;   --  mWh
      Rate       : Quantity;                               --  mW, a magnitude
      Voltage    : Quantity;                               --  mV, a battery's present voltage
      Load       : Per_Mille;                              --  a UPS's output load
      Input_Voltage, Output_Voltage : Quantity;            --  mV, a UPS's mains side
      Transfer   : Transfer_Reason := Unknown;
   end record;

   function Is_Battery (Kind : Source_Kind) return Boolean is
     (Kind in Internal_Battery | External_Battery);

   --  Named ceiling: the sources one snapshot holds.
   Maximum_Sources : constant := 32;
   subtype Source_Count is Natural range 0 .. Maximum_Sources;
   subtype Source_Index is Positive range 1 .. Maximum_Sources;
   type Source_Array is array (Source_Index) of Source;

   --  The machine's batteries together, and what it runs on.
   type Summary is record
      Sources   : Source_Count := 0;
      Batteries : Source_Count := 0;
      State     : Source_State := Unknown;   --  of the batteries together
      Runs_On   : Supply := Unknown;
      Low       : Flag := Unknown;
      Fresh     : Freshness := Unknown;      --  the oldest of the sources'
      Charge    : Per_Mille;
      Left      : Runtime;
   end record;

   ---------------------------------------------------------------------------
   --  Authority.
   ---------------------------------------------------------------------------
   --  The catalog role (native-runtime-services.ccl, "power-sources") and
   --  the badge procmgr mints for it: "PWRSRC" and a version. A fixed
   --  kernel stamp, so knowing the number grants nothing.
   Observer_Role : constant := 49;
   Observer_Tag  : constant Unsigned_64 := 16#5057_5253_5243_0001#;
   function Is_Observer (Tag : Unsigned_64) return Boolean is (Tag = Observer_Tag);

   ---------------------------------------------------------------------------
   --  The wire.
   ---------------------------------------------------------------------------
   --  The power block of labels (16#0270# .. 16#027F#; 16#0270# .. 16#0278#
   --  are CuBit.Power_Event_Exchange, CuBit.Power_Requests and
   --  CuBit.System_Stops).
   Summary_Label : constant Unsigned_32 := 16#0279#;
   Source_Label  : constant Unsigned_32 := 16#027A#;
   Refused_Label : constant Unsigned_32 := 16#027B#;

   subtype Word is Unsigned_64;
   Maximum_Word : constant Word := Word (Integer_64'Last);
   subtype Wire_Word is Word range 0 .. Maximum_Word;

   type Generation is new Unsigned_64 range 0 .. Unsigned_64 (Integer_64'Last);
   No_Generation : constant Generation := 0;

   type Summary_Mode is (Read_Now, Watch);
   for Summary_Mode use (Read_Now => 1, Watch => 2);

   type Refusal is
     (Not_Approved,       --  not the observer's badge
      Malformed,          --  framing, id zero, an unknown mode or page
      Stale_Generation,   --  the snapshot changed: read the summary again
      No_Such_Source,     --  index beyond the snapshot's sources
      Superseded,         --  a newer watch from the same reader
      No_Room);           --  no watch can be kept now: watch again later
   for Refusal use
     (Not_Approved => 1, Malformed => 2, Stale_Generation => 3, No_Such_Source => 4,
      Superseded => 5, No_Room => 6);

   --  A source's pages, each two payload words:
   --    Identity         bytes Kind, State, Fresh, Left.Kind, Transfer and
   --                     Name.Length (positions; bytes 6, 7 zero); then
   --                     bytes Low, Replace, Overload, Bypass, and in bits
   --                     32 .. 62 Left.Seconds
   --    Name_Page        the name's characters 1 .. 8, then 9 .. 16
   --    Charge_And_Load  Charge, Load (Per_Mille_Word)
   --    Energy           Energy_Now, Energy_Full (Quantity_Word)
   --    Design_And_Rate  Energy_Design, Rate
   --    Voltage_Page     Voltage, then zero (reserved)
   --    Mains            Input_Voltage, Output_Voltage
   type Page is
     (Identity, Name_Page, Charge_And_Load, Energy, Design_And_Rate, Voltage_Page, Mains);
   for Page use
     (Identity => 0, Name_Page => 1, Charge_And_Load => 2, Energy => 3, Design_And_Rate => 4,
      Voltage_Page => 5, Mains => 6);

   Request_Word    : constant := 0;
   Seen_Word       : constant := 1;
   Expected_Word   : constant := 1;
   Mode_Word       : constant := 2;
   Index_Word      : constant := 2;
   Page_Word       : constant := 3;
   Summary_Request_Words : constant := 3;
   Source_Request_Words  : constant := 4;

   Reply_Request_Word : constant := 0;
   Generation_Word    : constant := 1;
   Payload_Word       : constant := 2;
   Reply_Words        : constant := 4;
   Refusal_Word       : constant := 1;
   Refusal_Words      : constant := 2;

   --  The most a reader waits for an answer (its own deadline): Read_Now
   --  makes power.svc ask its sources, each within its own bound.
   Answer_Budget_Milliseconds : constant := 2_000;

   function Valid_Mode (W : Word) return Boolean is
     (for some M in Summary_Mode => Summary_Mode'Enum_Rep (M) = W);
   function To_Mode (W : Word) return Summary_Mode is
     (if W = Summary_Mode'Enum_Rep (Watch) then Watch else Read_Now)
   with Pre => Valid_Mode (W), Post => Summary_Mode'Enum_Rep (To_Mode'Result) = W;

   function Valid_Page (W : Word) return Boolean is (W <= Page'Enum_Rep (Page'Last));
   function To_Page (W : Word) return Page is
     (case W is
        when 0 => Identity, when 1 => Name_Page, when 2 => Charge_And_Load, when 3 => Energy,
        when 4 => Design_And_Rate, when 5 => Voltage_Page, when others => Mains)
   with Pre => Valid_Page (W), Post => Page'Enum_Rep (To_Page'Result) = W;

   function Valid_Refusal (W : Word) return Boolean is
     (for some R in Refusal => Refusal'Enum_Rep (R) = W);
   function To_Refusal (W : Word) return Refusal is
     (case W is
        when 1 => Not_Approved, when 2 => Malformed, when 3 => Stale_Generation,
        when 4 => No_Such_Source, when 5 => Superseded, when others => No_Room)
   with Pre => Valid_Refusal (W), Post => Refusal'Enum_Rep (To_Refusal'Result) = W;

   ---------------------------------------------------------------------------
   --  Quantities on the wire: the value, or Unknown_Word (above every value).
   ---------------------------------------------------------------------------
   Unknown_Word : constant Word := 2 ** 32;

   function Quantity_Word (Q : Quantity) return Wire_Word is
     (if Q.Known then Word (Q.Value) else Unknown_Word);
   function Valid_Quantity_Word (W : Word) return Boolean is
     (W <= Maximum_Quantity or else W = Unknown_Word);
   function To_Quantity (W : Word) return Quantity is
     (if W = Unknown_Word then Unknown_Quantity else (Known => True, Value => Quantity_Value (W)))
   with Pre => Valid_Quantity_Word (W), Post => Quantity_Word (To_Quantity'Result) = W;

   function Per_Mille_Word (P : Per_Mille) return Wire_Word is
     (if P.Known then Word (P.Value) else Unknown_Word);
   function Valid_Per_Mille_Word (W : Word) return Boolean is
     (W <= Whole or else W = Unknown_Word);
   function To_Per_Mille (W : Word) return Per_Mille is
     (if W = Unknown_Word then Unknown_Per_Mille else (Known => True, Value => Per_Mille_Value (W)))
   with Pre => Valid_Per_Mille_Word (W), Post => Per_Mille_Word (To_Per_Mille'Result) = W;

   ---------------------------------------------------------------------------
   --  Bytes in a word: byte 0 the least significant. Byte 7 stays below 128,
   --  so a packed word is a CCL integer.
   ---------------------------------------------------------------------------
   subtype Byte_Position is Natural range 0 .. 7;
   subtype Byte_Value is Word range 0 .. 255;
   Top_Byte_Limit : constant := 127;

   function Byte_Of (W : Word; P : Byte_Position) return Byte_Value is
     (Shift_Right (W, 8 * P) and 255);

   function Pack (B0, B1, B2, B3, B4, B5, B6, B7 : Byte_Value) return Wire_Word is
     (B0 or Shift_Left (B1, 8) or Shift_Left (B2, 16) or Shift_Left (B3, 24) or Shift_Left (B4, 32)
      or Shift_Left (B5, 40) or Shift_Left (B6, 48) or Shift_Left (B7, 56))
   with Pre => B7 <= Top_Byte_Limit;

   ---------------------------------------------------------------------------
   --  The summary's payload. Summary_A: bytes 0 .. 6 Sources, Batteries,
   --  State, Runs_On, Low, Fresh and Left.Kind (positions), byte 7 zero.
   --  Summary_B: bits 0 .. 15 the charge (per mille, or Unknown_Charge),
   --  bits 16 .. 48 the runtime's seconds (a Quantity_Word).
   ---------------------------------------------------------------------------
   Unknown_Charge : constant Word := 16#FFFF#;

   function Summary_A (S : Summary) return Wire_Word;
   function Summary_B (S : Summary) return Wire_Word;
   function Valid_Summary (A, B : Word) return Boolean;
   function To_Summary (A, B : Word) return Summary
   with Pre => Valid_Summary (A, B),
        Post => Summary_A (To_Summary'Result) = A and then Summary_B (To_Summary'Result) = B;

   ---------------------------------------------------------------------------
   --  A source's pages.
   ---------------------------------------------------------------------------
   type Payload is array (0 .. 1) of Wire_Word;

   function Page_Words (S : Source; P : Page) return Payload;

   --  Apply page P's payload to Into (its other fields kept); Valid False,
   --  and Into unchanged, when the payload is not one Page_Words writes.
   procedure Read_Page (P : Page; Data : Payload; Into : in out Source; Valid : out Boolean)
   with Post => (if Valid then Page_Words (Into, P) (0) = Data (0) and then Page_Words (Into, P) (1) = Data (1)
                 else Into = Into'Old);

end CuBit.Power_Sources;
