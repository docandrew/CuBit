------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What power.svc reads from acpi.svc about the AC adapter and the
--  batteries (PWR-004 slice 1), decoded from acpi.svc's observer pages, and
--  the requests that read them.
--
--  @description
--  acpi.svc serves copies of its cached samples on its observer endpoint
--  (ACPI_Protocol.Observer_Tag), operation Read_Metrics:
--    * the AC adapter, ACPI_Power_Observation_Protocol.Source_Page, its
--      Summary subpage: [generation, availability, state, time-known];
--      state 0 unknown, 1 offline, 2 online (ACPI0003 _PSR);
--    * the batteries, ACPI_Battery_Observation_Protocol.Battery_Page, by
--      selector slot * Selector_Stride + subpage: Inventory [generation,
--      availability, count, 0], then for each slot Entry_Summary [.., entry
--      status, reason], Information [.., source, units], Capacities
--      [.., design, last full], Design_Voltage, Dynamic_State [.., flow,
--      flags], Rate_And_Capacity [.., rate, remaining], Dynamic_Voltage
--      (_BIX / _BIF and _BST).
--  Every request is [page, maximum age in microseconds, selector, expected
--  generation]; a sample older than the age answers Too_Old with no values.
--  A quantity is its value, or Unknown_Quantity (2 ** 32): the same word
--  as CuBit.Power_Sources.Unknown_Word. Replies are 16#F000# with the four
--  words, or 16#F001# with [outcome, ...]: Stale (3) when the generation
--  moved on. The positions of Availability, Entry_Status, Unit_Kind and
--  Direction are acpi.svc's (ACPI_Cached_Power_Values,
--  ACPI_Cached_Battery_Values, ACPI_Battery_Status); tests/power-sources
--  encodes with acpi.svc's own codec and decodes with this.
--
--  Decoding is total: a word that is not one acpi.svc writes makes the
--  page unreadable, never a value.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Power_Sources;

package Power_ACPI_Facts with Pure, SPARK_Mode is
   package PS renames CuBit.Power_Sources;

   subtype Word is Unsigned_64;
   type Words is array (0 .. 3) of Word;

   --  acpi.svc's reply envelope (ACPI_Endpoint).
   Reply_OK    : constant Unsigned_32 := 16#F000#;
   Reply_Error : constant Unsigned_32 := 16#F001#;
   --  ACPI_Requests.Outcome'Pos (Stale): the generation changed.
   Stale_Outcome : constant Word := 3;

   --  The observer operation and pages.
   Read_Metrics : constant Unsigned_32 := 0;
   Source_Page  : constant Word := 21;
   Summary_Subpage : constant Word := 0;
   Battery_Page : constant Word := 22;
   Selector_Stride : constant Word := 16;
   Request_Words : constant := 4;

   type Battery_Subpage is
     (Inventory, Sample_Time, Entry_Summary, Information, Capacities, Design_Voltage, Dynamic_State,
      Rate_And_Capacity, Dynamic_Voltage);

   --  acpi.svc's sample cache can hold this many batteries; a reply naming
   --  more is not one it writes.
   Maximum_Batteries : constant := 16;
   subtype Battery_Count is Natural range 0 .. Maximum_Batteries;
   subtype Battery_Slot is Positive range 1 .. Maximum_Batteries;

   --  The age that admits any sample (its values, then marked Stale).
   Any_Age : constant Word := Word (Integer_64'Last);

   function Source_Request (Maximum_Age : Word) return Words is
     ([Source_Page, Maximum_Age, Summary_Subpage, 0]);
   function Inventory_Request (Maximum_Age : Word) return Words is
     ([Battery_Page, Maximum_Age, Battery_Subpage'Pos (Inventory), 0]);
   function Entry_Request (Maximum_Age : Word; Slot : Battery_Slot; Page : Battery_Subpage; Expected : Word)
     return Words is
     ([Battery_Page, Maximum_Age, Word (Slot) * Selector_Stride + Battery_Subpage'Pos (Page), Expected])
   with Pre => Page >= Entry_Summary;

   type Availability is
     (Not_Ready, Available, Missing, Failure, Too_Old, Clock_Unknown, Clock_Regressed, Counter_Exhausted);
   type Entry_Status is (Sampled, Known_Absent, Acquisition_Failed);
   type Unit_Kind is (Unspecified_Unit, Energy_And_Power, Charge_And_Current);
   type Direction is (Neither, Discharging, Charging);
   type AC_State is (Unknown, Offline, Online);

   Critical_Flag       : constant Word := 1;
   Charge_Limiting_Flag : constant Word := 2;
   Known_Flags         : constant Word := Critical_Flag + Charge_Limiting_Flag;

   function Valid_Availability (W : Word) return Boolean is (W <= Availability'Pos (Availability'Last));
   function To_Availability (W : Word) return Availability is (Availability'Val (W))
   with Pre => Valid_Availability (W);

   ---------------------------------------------------------------------------
   --  The facts.
   ---------------------------------------------------------------------------
   type AC_Facts is record
      Present : Boolean := False;       --  acpi.svc knows an ACPI0003 device
      State   : AC_State := Unknown;
      Fresh   : PS.Freshness := PS.Unknown;
   end record;
   No_AC : constant AC_Facts := (others => <>);

   type Battery_Facts is record
      Status : Entry_Status := Acquisition_Failed;
      Fresh  : PS.Freshness := PS.Unknown;
      Units  : Unit_Kind := Unspecified_Unit;
      Design, Last_Full, Design_Voltage : PS.Quantity;
      Dynamic_Known : Boolean := False;   --  _BST was read
      Flow : Direction := Neither;
      Critical, Charge_Limiting : Boolean := False;
      Rate, Remaining, Voltage : PS.Quantity;
   end record;
   Unread_Battery : constant Battery_Facts := (others => <>);
   type Battery_Array is array (Battery_Slot) of Battery_Facts;

   ---------------------------------------------------------------------------
   --  Pages: Valid is False for a reply acpi.svc never writes.
   ---------------------------------------------------------------------------
   function Valid_Quantity (W : Word) return Boolean renames PS.Valid_Quantity_Word;
   function To_Quantity (W : Word) return PS.Quantity renames PS.To_Quantity;

   --  The AC adapter's summary (an OK reply's words).
   procedure Decode_Source (Data : Words; Facts : out AC_Facts; Status : out Availability; Valid : out Boolean);

   --  The inventory: its generation, availability and count.
   procedure Decode_Inventory
     (Data : Words; Generation : out Word; Status : out Availability; Count : out Battery_Count;
      Valid : out Boolean)
   with Post => (if not Valid then Count = 0);

   --  One entry page applied to Facts (Data from an OK reply).
   procedure Decode_Entry
     (Page : Battery_Subpage; Data : Words; Facts : in out Battery_Facts; Valid : out Boolean)
   with Pre => Page >= Entry_Summary;
end Power_ACPI_Facts;
