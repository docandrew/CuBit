------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  ACPI's battery and AC facts as power sources, and the machine's summary
--  of them (PWR-004 slice 1): pure, without I/O.
--
--  @description
--  A battery (_BIX/_BIF, _BST; ACPI 6.6, 10.2.2):
--    * Charge: remaining over last full capacity, in its own units, at
--      most Whole; Unknown when either is unknown or the full capacity is
--      zero;
--    * State: _BST's direction. Neither is Full at or above the last full
--      capacity, else Not_Charging; Unknown without _BST, or when the
--      capacities cannot tell the two apart;
--    * Low: _BST's critical bit (Set or Clear; Unknown without _BST);
--      Replace, Overload and Bypass: Unknown (ACPI has no such report);
--    * Left: discharging, remaining / rate (To_Empty); charging, (last
--      full - remaining) / rate (To_Full); Unknown when the rate is unknown
--      or zero, or a capacity is;
--    * energies and rate: as reported for mWh/mW batteries; for mAh/mA ones
--      through the design voltage (energies) and the present voltage, else
--      the design voltage (rate); Unknown when there is no voltage, or the
--      product is beyond Maximum_Quantity.
--  A battery acpi.svc knows to be absent is no source; one it could not
--  read is a source with every value Unknown.
--  The AC adapter: Online or Offline from _PSR, Unknown when acpi.svc has
--  the device but not its state.
--
--  The summary: the batteries' combined charge (by energy, when every
--  battery's is known; a single battery's own otherwise), their combined
--  state, what the machine runs on (Mains while an adapter is Online,
--  Battery while a battery discharges or every adapter is Offline), Low
--  while any battery is, the oldest freshness, and the runtime.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Power_Sources;
with Power_ACPI_Facts;

package Power_Source_Mapping with Pure, SPARK_Mode is
   package PS renames CuBit.Power_Sources;
   package AF renames Power_ACPI_Facts;
   use type PS.Source_Kind, PS.Source_State, PS.Runtime_Kind, PS.Flag, PS.Freshness, PS.Source;
   use type AF.Entry_Status;

   Seconds_Per_Hour : constant := 3_600;
   Millis_Per_Unit  : constant := 1_000;   --  mAh * mV / 1000 = mWh

   --  "battery-N" (N the position among the batteries read) and "ac-adapter".
   function Battery_Name (Ordinal : PS.Source_Index) return PS.Source_Name;
   function AC_Name return PS.Source_Name;

   function Battery_Source (F : AF.Battery_Facts; Ordinal : PS.Source_Index) return PS.Source
   with Pre => F.Status /= AF.Known_Absent,
        Post => Battery_Source'Result.Kind = PS.Internal_Battery
          and then (if F.Status = AF.Acquisition_Failed then
                      Battery_Source'Result.State = PS.Unknown
                      and then not Battery_Source'Result.Charge.Known
                      and then Battery_Source'Result.Left.Kind = PS.Unknown)
          and then (if not F.Dynamic_Known then Battery_Source'Result.State = PS.Unknown)
          and then (if Battery_Source'Result.Left.Kind = PS.To_Empty then
                      Battery_Source'Result.State = PS.Discharging)
          and then (if Battery_Source'Result.Left.Kind = PS.To_Full then
                      Battery_Source'Result.State = PS.Charging);

   function AC_Source (F : AF.AC_Facts) return PS.Source
   with Pre => F.Present,
        Post => AC_Source'Result.Kind = PS.AC_Adapter
          and then AC_Source'Result.State in PS.Unknown | PS.Online | PS.Offline;

   --  Where a source came from: 0 the AC adapter, else its battery slot
   --  (acpi.svc's identity index).
   subtype Origin is Natural range 0 .. AF.Maximum_Batteries;
   type Origin_Array is array (PS.Source_Index) of Origin;

   --  The snapshot: the adapter first (when acpi.svc has one), then each
   --  battery that is not absent, in slot order; Origins says which.
   procedure Build
     (AC : AF.AC_Facts; Batteries : AF.Battery_Array; Battery_Count : AF.Battery_Count;
      Sources : out PS.Source_Array; Count : out PS.Source_Count; Origins : out Origin_Array)
   with Post => Count <= 1 + Battery_Count
     and then (for all I in 1 .. Count =>
                 (if Sources (I).Kind = PS.AC_Adapter then Origins (I) = 0
                  else Origins (I) in 1 .. Battery_Count
                       and then Batteries (Origins (I)).Status /= AF.Known_Absent))
     and then (for all I in 1 .. Count =>
                 Sources (I).Kind = (if I = 1 and then AC.Present then PS.AC_Adapter else PS.Internal_Battery));

   --  The snapshot kept when acpi.svc did not answer a reading in full:
   --  every Fresh source Stale, nothing else changed.
   function Fresh_No_Longer (F : PS.Freshness) return PS.Freshness is
     (if F = PS.Fresh then PS.Stale else F);
   function Aged (Sources : PS.Source_Array) return PS.Source_Array is
     ([for I in PS.Source_Index => (Sources (I) with delta Fresh => Fresh_No_Longer (Sources (I).Fresh))])
   with Post => (for all I in PS.Source_Index =>
                   Aged'Result (I) = (Sources (I) with delta Fresh => Fresh_No_Longer (Sources (I).Fresh))
                   and then Aged'Result (I).Fresh /= PS.Fresh);

   function Summarize (Sources : PS.Source_Array; Count : PS.Source_Count) return PS.Summary
   with Post => Summarize'Result.Sources = Count
     and then Summarize'Result.Batteries <= Count
     and then (if Summarize'Result.Batteries = 0 then
                 Summarize'Result.State = PS.Unknown and then not Summarize'Result.Charge.Known);
end Power_Source_Mapping;
