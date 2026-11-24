--  Hosted tests of PWR-004 slice 1 (tests/power-sources/run.sh):
--    * the contract's wire (CuBit.Power_Sources): the summary and every
--      source page read back as written, unknowns as unknowns; words no
--      encoder writes are refused and change nothing;
--    * power.svc's read of acpi.svc (Power_ACPI_Reader over
--      Power_ACPI_Facts) against acpi.svc's own codecs (Power_ACPI_Fixture),
--      then the mapping (Power_Source_Mapping): discharging, charging, full,
--      not charging, zero and unknown rates, unknown capacities, absent and
--      failed batteries, mAh batteries, two batteries, stale samples, a
--      generation that moves on mid-read, AC states;
--    * the Deskbar's battery text (Desktop_Power_Text): percent, state and
--      "N h M min left/to full", and the icon's fill;
--    * the stable identities (Power_ACPI_Identity, Power_ACPI_Sources):
--      paths read in chunks from the fixture's page 23 (acpi.svc's own
--      ACPI_Source_Paths.Chunk), names from the last segment, duplicates,
--      unknown, overlong and refused paths, a family moving on mid-path,
--      slot reordering keeping each source's name;
--    * the watch (Power_ACPI_Watch.Decide) against acpi.svc's own watch
--      policy (ACPI_Power_Inventory_Watches over a model transport): every
--      status, refusals of answers acpi.svc never writes;
--    * a reading acpi.svc did not answer in full (Power_Source_Mapping.Aged);
--    * the numbers power.svc mirrors equal shared/acpi's protocols.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Power_Sources;
with Power_ACPI_Facts;
with Power_ACPI_Sources;
with Power_ACPI_Identity;
with Power_ACPI_Watch;
with Power_Source_Mapping;
with ACPI_Power_Watch_Protocol;
with ACPI_Power_Identity_Protocol;
with ACPI_Power_Inventory_Watches;
with ACPI_Protocol;
with Power_ACPI_Fixture;
with Desktop_Power_Text;

procedure Power_Sources_Tests is
   package PS renames CuBit.Power_Sources;
   package AF renames Power_ACPI_Facts;
   package PM renames Power_Source_Mapping;
   package FX renames Power_ACPI_Fixture;
   package DT renames Desktop_Power_Text;
   use type PS.Source, PS.Summary, PS.Source_State, PS.Supply, PS.Flag, PS.Freshness, PS.Runtime_Kind;
   use type PS.Source_Kind, PS.Quantity, PS.Per_Mille, PS.Runtime, PS.Transfer_Reason;

   Checks, Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   procedure Ask (Request : AF.Words; Label : out Unsigned_32; Reply : out AF.Words; Delivered : out Boolean) is
      R : FX.Words;
   begin
      FX.Answer ([for I in FX.Words'Range => Request (I)], Label, R, Delivered);
      Reply := [for I in AF.Words'Range => R (I)];
   end Ask;
   package Reading_Of is new Power_ACPI_Sources (Ask);
   package Reader renames Reading_Of.Reader;

   --  The protocol's own freshness bound (ACPI_Power_Query_Protocol).
   Age : constant AF.Word := 5_000_000;

   type Snapshot is record
      Sources : PS.Source_Array;
      Count   : PS.Source_Count := 0;
      Summary : PS.Summary;
      Requests : Natural := 0;
   end record;

   function Take (S : FX.Scenario) return Snapshot is
      R : Reading_Of.Reading;
      Result : Snapshot;
   begin
      FX.Set (S);
      Reading_Of.Read (Age, R);
      Result.Sources := R.Sources;
      Result.Count := R.Count;
      Result.Summary := PM.Summarize (Result.Sources, Result.Count);
      Result.Requests := R.Requests;
      return Result;
   end Take;

   function Q (V : PS.Quantity_Value) return PS.Quantity is (PS.Known_Quantity (V));
   function PM_Of (V : PS.Per_Mille_Value) return PS.Per_Mille is ((Known => True, Value => V));

   Discharging_Battery : constant FX.Battery :=
     (Kind => FX.Sampled, Unit => FX.Energy, Design => 57_000, Last_Full => 50_000, Design_Voltage => 11_550,
      Direction => FX.Discharging, Critical => False, Charge_Limiting => False,
      Rate => 10_000, Remaining => 36_500, Voltage => 12_100);

   function One (B : FX.Battery; AC : FX.AC_Kind := FX.Offline) return FX.Scenario is
     ((AC => AC, Batteries_Ready => True, Count => 1, Batteries => [1 => B, others => <>],
       AC_Age => 0, Battery_Age => 0, Generation => 7, others => <>));

   ---------------------------------------------------------------------------
   procedure Test_Discharging is
      S : constant Snapshot := Take (One (Discharging_Battery));
      B : PS.Source renames S.Sources (2);
   begin
      Check (S.Count = 2, "discharging: adapter and battery");
      Check (S.Sources (1).Kind = PS.AC_Adapter and then S.Sources (1).State = PS.Offline
             and then S.Sources (1).Fresh = PS.Fresh, "discharging: the adapter is offline, fresh");
      Check (PS.Image (S.Sources (1).Name) = "ac-adapter", "the adapter's name");
      Check (B.Kind = PS.Internal_Battery and then PS.Image (B.Name) = "battery-1", "the battery's name");
      Check (B.State = PS.Discharging, "discharging: state");
      Check (B.Charge = PM_Of (730), "discharging: 36500 of 50000 mWh is 730 per mille");
      Check (B.Left = (PS.To_Empty, 13_140), "discharging: 36500 mWh at 10000 mW lasts 3 h 39 min");
      Check (B.Energy_Now = Q (36_500) and then B.Energy_Full = Q (50_000) and then B.Energy_Design = Q (57_000),
             "discharging: energies as reported (mWh)");
      Check (B.Rate = Q (10_000) and then B.Voltage = Q (12_100), "discharging: rate and voltage");
      Check (B.Condition.Low = PS.Clear and then B.Condition.Replace = PS.Unknown
             and then B.Condition.Overload = PS.Unknown and then B.Condition.Bypass = PS.Unknown,
             "discharging: low is Clear; ACPI cannot say replace, overload or bypass");
      Check (not B.Load.Known and then not B.Input_Voltage.Known and then B.Transfer = PS.Unknown,
             "discharging: the UPS fields are Unknown");
      Check (S.Summary.Sources = 2 and then S.Summary.Batteries = 1, "summary: counts");
      Check (S.Summary.State = PS.Discharging and then S.Summary.Runs_On = PS.Battery,
             "summary: discharging, on battery");
      Check (S.Summary.Charge = PM_Of (730) and then S.Summary.Left = (PS.To_Empty, 13_140)
             and then S.Summary.Fresh = PS.Fresh and then S.Summary.Low = PS.Clear, "summary: charge, runtime");
      Check (DT.Percent_Text (S.Summary.Charge) = "73%", "text: 73%");
      Check (DT.Runtime_Text (S.Summary.Left) = "3 h 39 min left", "text: 3 h 39 min left");
      Check (DT.State_Text (S.Summary) = "Discharging", "text: Discharging");
      Check (DT.Row_Text (S.Summary) = "73%, discharging, 3 h 39 min left", "text: the panel's row");
   end Test_Discharging;

   procedure Test_Charging is
      B : FX.Battery := Discharging_Battery;
   begin
      B.Direction := FX.Charging;
      B.Remaining := 25_000;
      B.Rate := 12_500;
      declare
         S : constant Snapshot := Take (One (B, FX.Online));
      begin
         Check (S.Sources (1).State = PS.Online, "charging: the adapter is online");
         Check (S.Sources (2).State = PS.Charging and then S.Sources (2).Left = (PS.To_Full, 7_200),
                "charging: 25000 mWh to go at 12500 mW is 2 h to full");
         Check (S.Summary.State = PS.Charging and then S.Summary.Runs_On = PS.Mains, "charging: summary");
         Check (DT.Runtime_Text (S.Summary.Left) = "2 h 0 min to full", "text: 2 h 0 min to full");
         Check (DT.Mains_Mark (S.Summary), "the icon's bolt while charging");
      end;
   end Test_Charging;

   procedure Test_Full_And_Limited is
      B : FX.Battery := Discharging_Battery;
   begin
      B.Direction := FX.Neither;
      B.Remaining := 50_000;
      B.Rate := 0;
      declare
         S : constant Snapshot := Take (One (B, FX.Online));
      begin
         Check (S.Sources (2).State = PS.Full and then S.Sources (2).Charge = PM_Of (1000)
                and then S.Sources (2).Left.Kind = PS.Unknown, "full: state, 1000 per mille, no runtime");
         Check (S.Summary.State = PS.Full and then DT.Row_Text (S.Summary) = "100%, plugged in",
                "full on the adapter: the panel's row says plugged in");
         Check (DT.Mains_Mark (S.Summary), "full on the adapter: the bolt (image 37b)");
      end;
      declare
         S : constant Snapshot := Take (One (B));
      begin
         Check (S.Summary.State = PS.Full and then DT.Row_Text (S.Summary) = "100%",
                "full, adapter unplugged: the panel's row");
         Check (not DT.Mains_Mark (S.Summary), "full, adapter unplugged: no bolt");
      end;
      B.Remaining := 30_000;
      B.Charge_Limiting := True;
      declare
         S : constant Snapshot := Take (One (B, FX.Online));
      begin
         Check (S.Sources (2).State = PS.Not_Charging, "a charge limit below full: Not_Charging");
         Check (DT.Row_Text (S.Summary) = "60%, not charging, plugged in", "not charging: the panel's row");
      end;
   end Test_Full_And_Limited;

   procedure Test_Rates is
      B : FX.Battery := Discharging_Battery;
   begin
      B.Rate := 0;
      declare
         S : constant Snapshot := Take (One (B));
      begin
         Check (S.Sources (2).Rate = Q (0) and then S.Sources (2).Left = PS.Unknown_Runtime,
                "zero rate: a known zero, and no runtime (never a division by zero)");
         Check (S.Sources (2).State = PS.Discharging and then S.Sources (2).Charge = PM_Of (730),
                "zero rate: state and charge still known");
         Check (DT.Row_Text (S.Summary) = "73%, discharging", "zero rate: no runtime in the row");
      end;
      B.Rate := FX.Unknown;
      declare
         S : constant Snapshot := Take (One (B));
      begin
         Check (not S.Sources (2).Rate.Known and then S.Sources (2).Rate.Value = 0
                and then S.Sources (2).Left.Kind = PS.Unknown, "unknown rate: Unknown, never zero");
      end;
   end Test_Rates;

   procedure Test_Unknown_Capacities is
      B : FX.Battery := Discharging_Battery;
   begin
      B.Last_Full := FX.Unknown;
      B.Direction := FX.Neither;
      declare
         S : constant Snapshot := Take (One (B));
      begin
         Check (not S.Sources (2).Charge.Known and then S.Sources (2).State = PS.Unknown,
                "unknown last full: no charge, and Neither cannot tell full from not charging");
         Check (not S.Sources (2).Energy_Full.Known and then S.Sources (2).Energy_Now = Q (36_500),
                "unknown last full: that energy alone Unknown");
         Check (not S.Summary.Charge.Known and then DT.Percent_Text (S.Summary.Charge) = "--%",
                "unknown charge: --% (no number stands in)");
         Check (DT.Row_Text (S.Summary) = "--%, state unknown", "unknown: the panel's row");
      end;
      B := Discharging_Battery;
      B.Last_Full := 0;
      declare
         S : constant Snapshot := Take (One (B));
      begin
         Check (not S.Sources (2).Charge.Known, "a zero full capacity: no charge");
      end;
   end Test_Unknown_Capacities;

   procedure Test_Absent_And_Failed is
      Absent : constant FX.Battery := (Kind => FX.Absent, others => <>);
      Failed : constant FX.Battery := (Kind => FX.Failed, others => <>);
   begin
      declare
         S : constant Snapshot := Take (One (Absent));
      begin
         Check (S.Count = 1 and then S.Sources (1).Kind = PS.AC_Adapter, "absent battery: no battery source");
         Check (S.Summary.Batteries = 0 and then S.Summary.State = PS.Unknown
                and then not S.Summary.Charge.Known and then S.Summary.Runs_On = PS.Battery,
                "absent battery: nothing to show; the offline adapter still says battery");
         Check (not DT.Shown (S.Summary), "absent battery: the Deskbar item is hidden");
      end;
      declare
         S : constant Snapshot := Take (One (Failed, FX.No_Device));
      begin
         Check (S.Count = 1 and then S.Sources (1).Kind = PS.Internal_Battery
                and then S.Sources (1).State = PS.Unknown and then not S.Sources (1).Charge.Known
                and then not S.Sources (1).Energy_Now.Known and then S.Sources (1).Condition.Low = PS.Unknown,
                "failed battery: a source whose every value is Unknown");
         Check (S.Summary.Runs_On = PS.Unknown, "no adapter and no reading: runs on Unknown");
         Check (DT.Shown (S.Summary), "failed battery: the item shows (a battery exists)");
      end;
      declare
         B : FX.Battery := Discharging_Battery;
      begin
         B.Unit := FX.Unspecified;
         declare
            S : constant Snapshot := Take (One (B, FX.No_Device));
         begin
            Check (S.Count = 1 and then S.Sources (1).State = PS.Unknown,
                   "a sample acpi.svc's contract refuses (no units) reads as failed, not as values");
         end;
      end;
      declare
         S : constant Snapshot :=
           Take ((AC => FX.No_Device, Batteries_Ready => False, Count => 1,
                  Batteries => [1 => Discharging_Battery, others => <>], others => <>));
      begin
         Check (S.Count = 0 and then S.Summary.Sources = 0 and then S.Summary.Fresh = PS.Unknown,
                "a battery inventory not ready yet: no sources");
      end;
   end Test_Absent_And_Failed;

   procedure Test_Stale is
      Scenario : FX.Scenario := One (Discharging_Battery);
   begin
      Scenario.Battery_Age := Age + 1;
      Scenario.AC_Age := Age + 1;
      declare
         S : constant Snapshot := Take (Scenario);
      begin
         Check (S.Sources (1).Fresh = PS.Stale and then S.Sources (1).State = PS.Offline,
                "stale adapter: its last state, marked Stale");
         Check (S.Sources (2).Fresh = PS.Stale and then S.Sources (2).Charge = PM_Of (730),
                "stale battery: its last values, marked Stale");
         Check (S.Summary.Fresh = PS.Stale, "stale: the summary says so");
         Check (DT.Row_Text (S.Summary) = "73%, discharging, 3 h 39 min left, last reading",
                "stale: the row says it is the last reading");
      end;
      Scenario.Battery_Age := 0;
      declare
         S : constant Snapshot := Take (Scenario);
      begin
         Check (S.Summary.Fresh = PS.Stale and then S.Sources (2).Fresh = PS.Fresh,
                "the oldest source sets the summary's freshness");
      end;
      --  While acpi.svc's watch is live, power.svc reads the adapter at any
      --  age: its Notify keeps acpi.svc's copy current.
      declare
         R : Reading_Of.Reading;
      begin
         FX.Set (Scenario);
         Reading_Of.Read (Age, R, AC_Maximum_Age => AF.Any_Age);
         Check (R.Sources (1).Fresh = PS.Fresh and then R.Sources (1).State = PS.Offline,
                "an adapter kept current by its Notify is Fresh, whatever its sample's age");
      end;
   end Test_Stale;

   procedure Test_Charge_Units is
      B : constant FX.Battery :=
        (Kind => FX.Sampled, Unit => FX.Charge, Design => 4_200, Last_Full => 4_000, Design_Voltage => 11_550,
         Direction => FX.Discharging, Critical => True, Charge_Limiting => False,
         Rate => 1_500, Remaining => 3_000, Voltage => 12_000);
      S : constant Snapshot := Take (One (B));
      R : PS.Source renames S.Sources (2);
   begin
      Check (R.Charge = PM_Of (750), "mAh: 3000 of 4000 is 750 per mille");
      Check (R.Energy_Now = Q (34_650) and then R.Energy_Full = Q (46_200) and then R.Energy_Design = Q (48_510),
             "mAh: energies through the design voltage");
      Check (R.Rate = Q (18_000), "mA: the rate through the present voltage");
      Check (R.Left = (PS.To_Empty, 7_200), "mAh: runtime in the battery's own units");
      Check (R.Condition.Low = PS.Set and then S.Summary.Low = PS.Set, "critical: Low is Set");
      Check (DT.Row_Text (S.Summary) = "75%, discharging, 2 h 0 min left, low",
             "low: said in words, not colour alone");
   end Test_Charge_Units;

   procedure Test_Two_Batteries is
      Second : FX.Battery := Discharging_Battery;
      Scenario : FX.Scenario := One (Discharging_Battery);
   begin
      Second.Last_Full := 30_000;
      Second.Remaining := 3_000;
      Second.Direction := FX.Neither;
      Second.Rate := 0;
      Scenario.Count := 2;
      Scenario.Batteries (2) := Second;
      declare
         S : constant Snapshot := Take (Scenario);
      begin
         Check (S.Count = 3 and then PS.Image (S.Sources (3).Name) = "battery-2", "two batteries");
         Check (S.Sources (3).State = PS.Not_Charging, "the idle second battery");
         Check (S.Summary.Charge = PM_Of (493), "two batteries: 39500 of 80000 mWh is 493 per mille");
         Check (S.Summary.Left = (PS.To_Empty, 14_220), "two batteries: all their energy at the discharging rate");
         Check (S.Summary.State = PS.Discharging, "two batteries: discharging");
      end;
   end Test_Two_Batteries;

   procedure Test_Generation_Moves is
      Scenario : constant FX.Scenario := One (Discharging_Battery);
      R : Reader.Reading;
   begin
      FX.Set (Scenario);
      --  The adapter is one request; the battery's third request moves the
      --  generation on: the reader starts the batteries again.
      FX.Change_After (3);
      Reader.Read (Age, R);
      Check (R.Count = 1 and then R.Batteries (1).Remaining = Q (36_500),
             "a generation that moved on mid-read: read again, consistently");
      Check (FX.Inventory_Reads = 2, "the inventory was read twice");
      FX.Set (Scenario);
      FX.Change_After (2);
      Reader.Read (Age, R);
      Check (R.Count = 1, "moved on at the inventory's first entry page: read again");
   end Test_Generation_Moves;

   procedure Test_AC is
   begin
      declare
         S : constant Snapshot := Take ((AC => FX.Failed, others => <>));
      begin
         Check (S.Count = 1 and then S.Sources (1).State = PS.Unknown and then S.Sources (1).Fresh = PS.Unknown,
                "an adapter acpi.svc could not read: present, Unknown");
         Check (S.Summary.Runs_On = PS.Unknown and then not DT.Shown (S.Summary),
                "no battery: the item stays hidden");
      end;
      declare
         S : constant Snapshot := Take ((AC => FX.Not_Ready, others => <>));
      begin
         Check (S.Count = 0, "an adapter not ready: no source");
      end;
      declare
         S : constant Snapshot := Take ((AC => FX.Online, others => <>));
      begin
         Check (S.Count = 1 and then S.Summary.Runs_On = PS.Mains and then S.Summary.Batteries = 0,
                "a desktop on mains, no battery");
      end;
   end Test_AC;

   ---------------------------------------------------------------------------
   --  The wire.
   ---------------------------------------------------------------------------
   procedure Round_Trip (S : PS.Source; What : String) is
      Into : PS.Source;
      Valid : Boolean;
      All_Valid : Boolean := True;
   begin
      for P in PS.Page loop
         PS.Read_Page (P, PS.Page_Words (S, P), Into, Valid);
         --  Every word is a CCL integer: Wire_Word's range, checked.
         All_Valid := All_Valid and then Valid;
      end loop;
      Check (All_Valid, What & ": every page reads");
      Check (Into = S, What & ": the pages give the source back");
   end Round_Trip;

   procedure Test_Wire is
      S : constant Snapshot := Take (One (Discharging_Battery));
      Odd : PS.Source :=
        (Kind => PS.UPS, Name => PM.Battery_Name (32), State => PS.On_Battery,
         Condition => (PS.Set, PS.Clear, PS.Set, PS.Unknown), Fresh => PS.Stale,
         Charge => (True, 1000), Left => (PS.To_Empty, PS.Maximum_Quantity),
         Energy_Now => Q (PS.Maximum_Quantity), Energy_Full => PS.Unknown_Quantity,
         Energy_Design => Q (0), Rate => Q (1), Voltage => PS.Unknown_Quantity, Load => (True, 999),
         Input_Voltage => Q (230_000), Output_Voltage => Q (229_500), Transfer => PS.Input_Voltage);
      Into : PS.Source;
      Before : PS.Source;
      Valid : Boolean;
   begin
      Odd.Name.Text (16) := '~';
      Round_Trip (S.Sources (1), "the adapter");
      Round_Trip (S.Sources (2), "the battery");
      Round_Trip (Odd, "a UPS at every field's edge");
      Round_Trip ((others => <>), "the default (all Unknown)");
      declare
         Summaries : constant array (1 .. 3) of PS.Summary :=
           [S.Summary, (others => <>),
            (Sources => 32, Batteries => 31, State => PS.Not_Charging, Runs_On => PS.Mains, Low => PS.Set,
             Fresh => PS.Stale, Charge => (True, 0), Left => (PS.To_Full, PS.Maximum_Quantity))];
      begin
         for Y of Summaries loop
            Check (PS.Summary_A (Y) <= PS.Maximum_Word and then PS.Summary_B (Y) <= PS.Maximum_Word,
                   "summary words are CCL integers");
            Check (PS.Valid_Summary (PS.Summary_A (Y), PS.Summary_B (Y))
                   and then PS.To_Summary (PS.Summary_A (Y), PS.Summary_B (Y)) = Y, "the summary reads back");
         end loop;
      end;
      Check (not PS.Valid_Summary (PS.Summary_A (S.Summary) + 2 ** 56, PS.Summary_B (S.Summary)),
             "a summary with byte 7 set is refused");
      Check (not PS.Valid_Summary (PS.Summary_A (S.Summary), 1001), "a charge above 1000 is refused");
      Check (not PS.Valid_Summary (PS.Summary_A ((others => <>)), PS.Summary_B (S.Summary)),
             "an unknown runtime with seconds is refused");
      --  Words no encoder writes change nothing.
      Into := S.Sources (2);
      Before := Into;
      PS.Read_Page (PS.Energy, [PS.Unknown_Word + 1, 0], Into, Valid);
      Check (not Valid and then Into = Before, "a quantity above Unknown_Word is refused");
      PS.Read_Page (PS.Charge_And_Load, [1001, 0], Into, Valid);
      Check (not Valid and then Into = Before, "a charge above 1000 is refused");
      PS.Read_Page (PS.Name_Page, [16#7F#, 0], Into, Valid);
      Check (not Valid and then Into = Before, "a name byte outside printable ASCII is refused");
      PS.Read_Page (PS.Identity, [16#FF#, 0], Into, Valid);
      Check (not Valid and then Into = Before, "an unknown kind is refused");
      PS.Read_Page (PS.Voltage_Page, [5, 1], Into, Valid);
      Check (not Valid and then Into = Before, "the reserved word must be zero");
      Check (PS.Valid_Mode (1) and then PS.Valid_Mode (2) and then not PS.Valid_Mode (0)
             and then not PS.Valid_Mode (3), "modes");
      Check (PS.Is_Observer (PS.Observer_Tag) and then not PS.Is_Observer (0), "the observer badge");
   end Test_Wire;

   procedure Test_Glyph is
   begin
      Check (DT.Fill_Width ((False, 0), 14) = 0, "unknown charge: an empty icon");
      Check (DT.Fill_Width ((True, 0), 14) = 0 and then DT.Fill_Width ((True, 1000), 14) = 14
             and then DT.Fill_Width ((True, 500), 14) = 7, "the icon's fill follows the charge");
      Check (DT.Fill_Width ((True, 1), 14) = 1, "any charge shows at least one column");
      Check (DT.Runtime_Text ((PS.To_Empty, 45 * 60 + 59)) = "45 min left", "under an hour: minutes alone");
      Check (DT.Runtime_Text ((PS.To_Full, 0)) = "0 min to full", "zero seconds to full");
      Check (DT.Runtime_Text (PS.Unknown_Runtime) = "", "unknown runtime: nothing said");
      Check (DT.Percent_Text ((True, 999)) = "99%", "99.9% is not yet 100%");
      Check (DT.Tooltip ((Sources => 1, Batteries => 1, State => PS.Charging, Runs_On => PS.Mains, Low => PS.Clear,
                          Fresh => PS.Fresh, Charge => (True, 420), Left => (PS.To_Full, 5_400)))
             = "Battery 42%, charging, 1 h 30 min to full", "the item's accessible name");
   end Test_Glyph;

   ---------------------------------------------------------------------------
   --  The numbers power.svc mirrors (Power_ACPI_Watch, Power_ACPI_Identity)
   --  are shared/acpi's.
   ---------------------------------------------------------------------------
   package PW renames Power_ACPI_Watch;
   package ID renames Power_ACPI_Identity;
   package WP renames ACPI_Power_Watch_Protocol;
   package IP renames ACPI_Power_Identity_Protocol;
   use type PW.Step, ID.Result_Code, PS.Source_Name;

   procedure Test_Protocol_Numbers is
   begin
      Check (PW.Watch_Label = WP.Watch and then PW.Schema = WP.Schema
             and then PW.Status'Pos (PW.Status'Last) = WP.Status'Pos (WP.Status'Last),
             "the watch's label, schema and statuses are ACPI_Power_Watch_Protocol's");
      for S in PW.Status loop
         Check (PW.Status'Image (S) = WP.Status'Image (WP.Status'Val (PW.Status'Pos (S)))
                and then WP.Status'Enum_Rep (WP.Status'Val (PW.Status'Pos (S))) = PW.Status'Pos (S),
                "watch status " & PW.Status'Image (S) & " is the protocol's, by position");
      end loop;
      Check (ID.Identity_Page = IP.Identity_Page and then ID.Maximum_Battery_Index = IP.Maximum_Battery_Index
             and then ID.Maximum_Path_Characters = IP.Maximum_Path_Characters
             and then ID.Result_Code'Pos (ID.Result_Code'Last) = IP.Result_Code'Pos (IP.Result_Code'Last),
             "the identity page's numbers are ACPI_Power_Identity_Protocol's");
      for C in ID.Result_Code loop
         Check (ID.Result_Code'Image (C) = IP.Result_Code'Image (IP.Result_Code'Val (ID.Result_Code'Pos (C)))
                and then IP.Result_Code'Enum_Rep (IP.Result_Code'Val (ID.Result_Code'Pos (C))) = ID.Result_Code'Pos (C),
                "identity result " & ID.Result_Code'Image (C) & " is the protocol's, by position");
      end loop;
   end Test_Protocol_Numbers;

   ---------------------------------------------------------------------------
   --  Stable identities.
   ---------------------------------------------------------------------------
   function Named (S : Snapshot; I : PS.Source_Index) return String is (PS.Image (S.Sources (I).Name));

   procedure Test_Identities is
      Scenario : FX.Scenario := One (Discharging_Battery);
      Second : FX.Battery := Discharging_Battery;
   begin
      Scenario.AC_Path := FX.Path_Of ("\_SB_.ADP1");
      Scenario.Battery_Paths (1) := FX.Path_Of ("\_SB_.PCI0.LPCB.EC0_.BAT0");
      declare
         S : constant Snapshot := Take (Scenario);
      begin
         Check (Named (S, 1) = "ADP1" and then Named (S, 2) = "BAT0",
                "sources are named by their path's last segment (ADP1, BAT0)");
         Check (S.Sources (2).Charge = PM_Of (730), "named, the battery's values are its own");
         --  \_SB_.ADP1 is 10 characters (2 chunks), the battery's 25 (4).
         Check (FX.Identity_Reads = 6, "each path read in seven-character chunks");
      end;
      Scenario.AC_Path := FX.Path_Of ("\_SB_.AC__");
      Check (Named (Take (Scenario), 1) = "AC", "a segment's padding underscores are not the name's");

      --  Two batteries: the same last segment under different parents,
      --  then the same last two segments.
      Second.Remaining := 10_000;
      Scenario.Count := 2;
      Scenario.Batteries (2) := Second;
      Scenario.Battery_Paths (1) := FX.Path_Of ("\_SB_.PCI0.EC0_.BAT0");
      Scenario.Battery_Paths (2) := FX.Path_Of ("\_SB_.PCI0.EC1_.BAT0");
      declare
         S : constant Snapshot := Take (Scenario);
      begin
         Check (Named (S, 2) = "EC0.BAT0" and then Named (S, 3) = "EC1.BAT0",
                "a shared last segment: the last two name the sources");
      end;
      Scenario.Battery_Paths (1) := FX.Path_Of ("\_SB_.A___.EC0_.BAT0");
      Scenario.Battery_Paths (2) := FX.Path_Of ("\_SB_.B___.EC0_.BAT0");
      declare
         S : constant Snapshot := Take (Scenario);
      begin
         Check (Named (S, 2) = "battery-1" and then Named (S, 3) = "battery-2",
                "shared last two segments: the slot names stay");
      end;

      --  acpi.svc reorders its slots: each name stays with its values.
      Scenario.Battery_Paths (1) := FX.Path_Of ("\_SB_.BAT0");
      Scenario.Battery_Paths (2) := FX.Path_Of ("\_SB_.BAT1");
      declare
         Before : constant Snapshot := Take (Scenario);
         Swapped : FX.Scenario := Scenario;
      begin
         Swapped.Batteries (1) := Scenario.Batteries (2);
         Swapped.Batteries (2) := Scenario.Batteries (1);
         Swapped.Battery_Paths (1) := Scenario.Battery_Paths (2);
         Swapped.Battery_Paths (2) := Scenario.Battery_Paths (1);
         Swapped.Generation := Scenario.Generation + 1;
         declare
            After : constant Snapshot := Take (Swapped);
         begin
            Check (Named (Before, 2) = "BAT0" and then Named (After, 3) = "BAT0"
                   and then Before.Sources (2) = After.Sources (3) and then Before.Sources (3) = After.Sources (2),
                   "reordered slots: BAT0 keeps its name and its values");
         end;
      end;

      --  No path (acpi.svc answers Not_Ready for it): the slot name.
      Scenario.Battery_Paths (2) := (others => <>);
      Check (Named (Take (Scenario), 3) = "battery-2", "a source without a path keeps its slot name");

      --  The longest path acpi.svc writes: 64 segments, 320 characters,
      --  46 chunks.
      declare
         Long : String (1 .. 320);
         One_Battery : FX.Scenario := One (Discharging_Battery);
      begin
         for Segment in 0 .. 63 loop
            Long (Segment * 5 + 1 .. Segment * 5 + 5) := ".N" & Character'Val (Character'Pos ('A') + Segment mod 26) & "_Z";
         end loop;
         Long (1) := '\';
         Long (317 .. 320) := "BATZ";
         One_Battery.Battery_Paths (1) := FX.Path_Of (Long);
         declare
            S : constant Snapshot := Take (One_Battery);
         begin
            Check (Named (S, 2) = "BATZ", "a 320-character path is read whole (46 chunks)");
            Check (FX.Identity_Reads = 1 + 46, "46 chunks for 320 characters (and the adapter's one, Not_Ready)");
         end;
      end;

      --  The battery family moves on while its path is read: the whole
      --  reading starts again, and values and names agree.
      Scenario := One (Discharging_Battery);
      Scenario.AC_Path := FX.Path_Of ("\_SB_.ADP1");
      Scenario.Battery_Paths (1) := FX.Path_Of ("\_SB_.BAT0");
      FX.Set (Scenario);
      --  8 battery pages, 2 AC chunks, then the battery path's first.
      FX.Change_After (11);
      declare
         R : Reading_Of.Reading;
      begin
         Reading_Of.Read (Age, R);
         Check (R.Count = 2 and then PS.Image (R.Sources (2).Name) = "BAT0" and then FX.Inventory_Reads = 2,
                "a family that moved on mid-path: read again, named");
      end;
   end Test_Identities;

   procedure Test_Identity_Decoding is
      Generation : ID.Word;
      Code : ID.Result_Code;
      Total : ID.Path_Length;
      Valid : Boolean;
      P : ID.Path;
   begin
      ID.Decode ([3, 0, 25, 16#41#], Generation, Code, Total, Valid);
      Check (Valid and then Code = ID.OK and then Total = 25 and then Generation = 3, "an identity chunk");
      ID.Decode ([3, ID.Word (ID.Result_Code'Pos (ID.Result_Code'Last)) + 1, 0, 0], Generation, Code, Total, Valid);
      Check (not Valid, "a result code acpi.svc never writes");
      ID.Decode ([3, 1, 5, 0], Generation, Code, Total, Valid);
      Check (not Valid, "a refusal carrying a length");
      ID.Decode ([3, 0, 321, 0], Generation, Code, Total, Valid);
      Check (not Valid, "a length beyond 320");
      ID.Decode ([ID.Word (Integer_64'Last) + 1, 0, 1, 0], Generation, Code, Total, Valid);
      Check (not Valid, "a generation beyond a CCL integer");
      P := ID.Unknown_Path;
      ID.Append (P, 3, Character'Pos ('\') + Character'Pos ('A') * 256 + Character'Pos ('b') * 65536, Valid);
      Check (not Valid and then P.Length = 0, "a character no path has: refused, nothing kept");
      ID.Append (P, 2, Character'Pos ('\') + Character'Pos ('A') * 256 + Character'Pos ('B') * 65536, Valid);
      Check (not Valid and then P.Length = 0, "a byte past the path's end: refused");
      ID.Append (P, 2, Character'Pos ('\') + Character'Pos ('A') * 256, Valid);
      Check (Valid and then P.Length = 2, "a short last chunk");
      Check (ID.Short_Name (P).Length = 0, "a segment that is not four characters names nothing");
      Check (ID.Short_Name (ID.Unknown_Path).Length = 0 and then ID.Long_Name (ID.Unknown_Path).Length = 0,
             "no path, no name");
   end Test_Identity_Decoding;

   ---------------------------------------------------------------------------
   --  The watch, against acpi.svc's own policy.
   ---------------------------------------------------------------------------
   subtype Token is Natural;
   Next_Token : Token := 0;
   Last_Answer : WP.Status := WP.Unavailable;
   Last_Id, Last_Generation : WP.Word := 0;
   procedure Save_Current (T : out Token; Saved : out Boolean) is
   begin
      Next_Token := Next_Token + 1;
      T := Next_Token;
      Saved := True;
   end Save_Current;
   procedure Answer_Saved (T : in out Token; Id : WP.Word; Status : WP.Status; Generation : WP.Word;
                           Delivered : out Boolean) is
   begin
      Last_Id := Id;
      Last_Answer := Status;
      Last_Generation := Generation;
      T := 0;
      Delivered := True;
   end Answer_Saved;
   package Policy is new ACPI_Power_Inventory_Watches (Token, 0, Save_Current => Save_Current,
                                                       Answer_Saved => Answer_Saved);
   package Short_Policy is new ACPI_Power_Inventory_Watches (Token, 0, Maximum_Generation => 2,
                                                             Save_Current => Save_Current,
                                                             Answer_Saved => Answer_Saved);
   Observer : constant WP.Word := WP.Word (ACPI_Protocol.Observer_Tag);

   function Wire (Id : WP.Word; Status : WP.Status; Generation : WP.Word) return PW.Words is
     ([Id, WP.Word (WP.Status'Enum_Rep (Status)), Generation, WP.Schema]);

   procedure Decided (Words : PW.Words; Id : PW.Word; Seen : PW.Generation; Expected : PW.Step;
                      Expected_Seen : PW.Generation; What : String; Label : Unsigned_32 := PW.Watch_Label;
                      Length : Natural := PW.Request_Words) is
      Next : PW.Step;
      New_Seen : PW.Generation;
      Why : PW.Status;
      Answered : Boolean;
   begin
      PW.Decide (Label, Length, Words, Id, Seen, Next, New_Seen, Why, Answered);
      Check (Next = Expected and then New_Seen = Expected_Seen, What);
   end Decided;

   procedure Test_Watch is
      Item : Policy.State;
      Reply : Policy.Response;
      Deferred : Boolean;
      Previous : Policy.Delivery;
      Result : Policy.Delivery;
      use type Policy.Delivery, WP.Status;
      function Ask (Sender, Id, Seen : WP.Word; Authority : WP.Word := Observer;
                    Length : WP.Word := 4) return Policy.Request is
        ((Authority => Authority, Sender => Sender, Length => Length, ID => Id, Seen => Seen, others => 0));
   begin
      --  Seen 0: the initial generation at once.
      Policy.Watch (Item, Ask (1, 5, 0), True, Reply, Deferred, Previous);
      Check (not Deferred, "seen 0 is answered at once");
      Decided (Wire (Reply.ID, Reply.Status, Reply.Generation), 5, 0, PW.Read_Then_Watch, 1,
               "the first answer: read, then watch from generation 1");
      --  Kept, then answered when the inventory moves on (a Notify).
      Policy.Watch (Item, Ask (1, 6, 1), True, Reply, Deferred, Previous);
      Check (Deferred, "a watch from the current generation is kept");
      Policy.Changed (Item);
      Policy.Advance (Item, True, Result);
      Check (Result = Policy.Answered, "a change answers the kept watch");
      Decided (Wire (Last_Id, Last_Answer, Last_Generation), 6, 1, PW.Read_Then_Watch, 2,
               "a Notify: read, then watch from generation 2");
      --  acpi.svc not serving its runtime yet.
      Policy.Watch (Item, Ask (1, 7, 2), False, Reply, Deferred, Previous);
      Decided (Wire (Reply.ID, Reply.Status, Reply.Generation), 7, 2, PW.Retry_Later, 2,
               "not serving yet: watch again later, nothing read");
      --  An earlier acpi.svc's generation.
      Policy.Watch (Item, Ask (1, 8, 9), True, Reply, Deferred, Previous);
      Decided (Wire (Reply.ID, Reply.Status, Reply.Generation), 8, 9, PW.Resynchronize, 0,
               "a generation from an earlier acpi.svc: watch from 0");
      --  Not the observer's badge; a request acpi.svc refuses as malformed.
      Policy.Watch (Item, Ask (1, 9, 2, Authority => 0), True, Reply, Deferred, Previous);
      Decided (Wire (Reply.ID, Reply.Status, Reply.Generation), 9, 2, PW.Give_Up, 2, "denied: the watch ends");
      Policy.Watch (Item, Ask (1, 10, 2, Length => 3), True, Reply, Deferred, Previous);
      Decided (Wire (Reply.ID, Reply.Status, Reply.Generation), 10, 2, PW.Give_Up, 2, "malformed: the watch ends");
      --  The same sender again: its kept watch is answered Superseded.
      Policy.Watch (Item, Ask (1, 11, 2), True, Reply, Deferred, Previous);
      Policy.Watch (Item, Ask (1, 12, 2), True, Reply, Deferred, Previous);
      Check (Previous = Policy.Answered and then Last_Answer = WP.Superseded, "a newer watch supersedes");
      Decided (Wire (Last_Id, Last_Answer, Last_Generation), 11, 2, PW.Watch_Again, 2,
               "superseded: watch again, nothing read");
      --  Eight senders kept: the ninth is Full.
      for Sender in 2 .. 8 loop
         Policy.Watch (Item, Ask (WP.Word (Sender), 20, 2), True, Reply, Deferred, Previous);
      end loop;
      Policy.Watch (Item, Ask (9, 21, 2), True, Reply, Deferred, Previous);
      Decided (Wire (Reply.ID, Reply.Status, Reply.Generation), 21, 2, PW.Retry_Later, 2,
               "eight watches kept: the ninth watches again later");
      --  Stopping answers every kept watch.
      Policy.Begin_Stop (Item);
      Policy.Advance (Item, True, Result);
      Decided (Wire (Last_Id, Last_Answer, Last_Generation), Last_Id, 2, PW.Give_Up, 2,
               "acpi.svc stopping: the watch ends");
      --  The generation exhausted.
      declare
         Short : Short_Policy.State;
         R : Short_Policy.Response;
         Short_Result : Short_Policy.Delivery;
      begin
         Short_Policy.Changed (Short);
         Short_Policy.Changed (Short);
         Short_Policy.Watch (Short, (Authority => Observer, Sender => 1, Length => 4, ID => 3, Seen => 2, others => 0),
                             True, R, Deferred, Short_Result);
         Decided (Wire (R.ID, R.Status, R.Generation), 3, 2, PW.Give_Up, 2, "an exhausted generation: the watch ends");
      end;
      --  Answers acpi.svc never writes.
      Decided (Wire (5, WP.Changed, 3), 5, 3, PW.Give_Up, 3, "a Changed that does not move on");
      Decided (Wire (6, WP.Changed, 3), 5, 2, PW.Give_Up, 2, "another watch's id");
      Decided ([5, 9, 3, 1], 5, 2, PW.Give_Up, 2, "a status acpi.svc never writes");
      Decided ([5, 0, 3, 2], 5, 2, PW.Give_Up, 2, "another schema");
      Decided (Wire (5, WP.Changed, 3), 5, 2, PW.Give_Up, 2, "another label", Label => 30);
      Decided (Wire (5, WP.Changed, 3), 5, 2, PW.Give_Up, 2, "another length", Length => 3);
      Decided ([5, 0, PW.Word (Integer_64'Last) + 1, 1], 5, 2, PW.Give_Up, 2, "a generation beyond a CCL integer");
   end Test_Watch;

   procedure Test_Aged is
      S : constant Snapshot := Take (One (Discharging_Battery));
      Unknown_AC : PS.Source_Array := S.Sources;
      Aged : PS.Source_Array;
   begin
      Unknown_AC (1).Fresh := PS.Unknown;
      Aged := PM.Aged (Unknown_AC);
      Check (Aged (2).Fresh = PS.Stale and then Aged (1).Fresh = PS.Unknown,
             "a reading not answered in full: Fresh becomes Stale, Unknown stays");
      Check (Aged (2).Charge = S.Sources (2).Charge and then Aged (2).Name = S.Sources (2).Name
             and then Aged (2).State = S.Sources (2).State, "nothing else changes");
   end Test_Aged;
begin
   Test_Discharging;
   Test_Charging;
   Test_Full_And_Limited;
   Test_Rates;
   Test_Unknown_Capacities;
   Test_Absent_And_Failed;
   Test_Stale;
   Test_Charge_Units;
   Test_Two_Batteries;
   Test_Generation_Moves;
   Test_AC;
   Test_Wire;
   Test_Glyph;
   Test_Protocol_Numbers;
   Test_Identities;
   Test_Identity_Decoding;
   Test_Watch;
   Test_Aged;
   Put_Line ("power-sources:" & Natural'Image (Checks) & " checks," & Natural'Image (Failures) & " failures");
   if Failures > 0 then
      raise Program_Error with "power-sources tests failed";
   end if;
end Power_Sources_Tests;
