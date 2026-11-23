------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  TEST FIXTURE (PWR-004 slice 1): acpi.svc's AC and battery observer pages
--  for a scripted machine, answered by acpi.svc's own codecs
--  (ACPI_Cached_Power_Codec, ACPI_Cached_Battery_Codec and their
--  dispatchers, userspace/services/acpi/power), so power.svc's reader meets
--  exactly the words acpi.svc writes. Used by the hosted tests and by the
--  QEMU stand-in (tests/power-sources/standin), never by a production
--  image. The envelope (16#F000# / 16#F001# and the outcome numbers) is
--  ACPI_Endpoint.Encode's over ACPI_Requests.Outcome.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Power_ACPI_Fixture is
   subtype Word is Unsigned_64;
   type Words is array (0 .. 3) of Word;

   Unknown : constant Word := 2 ** 32;   --  a quantity the battery does not report

   type AC_Kind is (No_Device, Not_Ready, Failed, Offline, Online);
   type Battery_Kind is (Sampled, Absent, Failed);
   type Units is (Unspecified, Energy, Charge);
   type Flow is (Neither, Discharging, Charging);

   type Battery is record
      Kind : Battery_Kind := Sampled;
      Unit : Units := Energy;
      Design, Last_Full, Design_Voltage : Word := Unknown;
      Direction : Flow := Neither;
      Critical, Charge_Limiting : Boolean := False;
      Rate, Remaining, Voltage : Word := Unknown;
   end record;

   Maximum : constant := 4;
   type Battery_List is array (1 .. Maximum) of Battery;

   --  A source's ACPI namespace path (page 23, acpi.svc's
   --  ACPI_Source_Paths.Chunk); empty: acpi.svc has none for it (Not_Ready).
   Path_Capacity : constant := 320;
   type Source_Path is record
      Length : Natural range 0 .. Path_Capacity := 0;
      Text   : String (1 .. Path_Capacity) := [others => ' '];
   end record;
   function Path_Of (Text : String) return Source_Path
   with Pre => Text'Length <= Path_Capacity;
   type Path_List is array (1 .. Maximum) of Source_Path;

   type Scenario is record
      AC : AC_Kind := No_Device;
      Batteries_Ready : Boolean := True;   --  False: the inventory is Not_Ready
      Count : Natural range 0 .. Maximum := 0;
      Batteries : Battery_List;
      --  How old each family's sample is, in microseconds.
      AC_Age, Battery_Age : Word := 0;
      Generation : Word := 1;
      --  The AC family's cache generation (page 21's summary).
      AC_Generation : Word := 1;
      AC_Path : Source_Path;
      Battery_Paths : Path_List;
   end record;

   procedure Set (S : Scenario);
   function Current return Scenario;

   --  After this many more battery and identity requests the battery
   --  inventory's generation
   --  moves on (0: never); the request after it is answered Stale.
   procedure Change_After (Requests : Natural);

   --  One Read_Metrics request (words) from an observer; Label and Reply
   --  as acpi.svc answers them. Handled False for a request that is not
   --  one of these pages.
   procedure Answer (Request : Words; Label : out Unsigned_32; Reply : out Words; Handled : out Boolean);

   --  How many identity (page 23) requests were answered since Set.
   function Identity_Reads return Natural;

   --  How many requests were answered (and how many battery inventory
   --  reads) since Set.
   function Answered return Natural;
   function Inventory_Reads return Natural;
end Power_ACPI_Fixture;
