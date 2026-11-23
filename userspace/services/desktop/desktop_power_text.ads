------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What the Deskbar says about the battery (PWR-004 slice 1, UI-027): the
--  status item's accessible name, the panel's row and the icon's fill, from
--  power.svc's summary (CuBit.Power_Sources). Pure; tests/power-sources.
--
--  @description
--  Words carry every state, never colour alone: "73%, discharging, 3 h 39
--  min left", ", low" while a battery is low, ", last reading" while the
--  values are stale. An unknown charge is "--%", an unknown runtime is not
--  mentioned: no number stands in for one the battery did not give.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Power_Sources;

package Desktop_Power_Text with Pure, SPARK_Mode is
   package PS renames CuBit.Power_Sources;
   use type PS.Source_State, PS.Flag, PS.Freshness, PS.Runtime_Kind, PS.Supply;

   --  The Battery item exists while a battery does.
   function Shown (S : PS.Summary) return Boolean is (S.Batteries > 0);

   --  The icon's bolt (calm: drawn, never blinking): while the machine runs
   --  on mains, charging or not, so a full battery on its adapter still
   --  shows it is plugged in (image 37b: "100%, full" said nothing of the
   --  adapter). The row's words tell charging from plugged in.
   function Mains_Mark (S : PS.Summary) return Boolean is
     (S.State = PS.Charging or else S.Runs_On = PS.Mains);

   --  Whether the row says ", plugged in": on mains and not charging
   --  ("charging" already says it).
   function Plugged_Words (S : PS.Summary) return Boolean is
     (S.Runs_On = PS.Mains and then S.State /= PS.Charging);

   --  The icon's alarm mark: drawn while a battery is low.
   function Low_Mark (S : PS.Summary) return Boolean is (S.Low = PS.Set);

   --  "73%" (whole percent, rounded down: 100% only when full), "--%".
   function Percent_Text (C : PS.Per_Mille) return String
   with Post => Percent_Text'Result'Length in 2 .. 4;

   --  "Discharging", "Charging", "Full", "Not charging", ... "State unknown".
   function State_Text (S : PS.Summary) return String
   with Post => State_Text'Result'Length <= 13;

   --  "3 h 39 min left", "45 min left", "2 h 0 min to full"; "" when unknown.
   function Runtime_Text (R : PS.Runtime) return String
   with Post => Runtime_Text'Result'Length <= 32;

   --  The panel's row: percent, state (none at a known 100% full: the
   --  percent says it), runtime, then ", plugged in", ", low" and ", last
   --  reading" when they apply.
   Row_Length_Limit : constant := 96;
   function Row_Text (S : PS.Summary) return String
   with Post => Row_Text'Result'Length <= Row_Length_Limit;

   --  The status item's accessible name (its tooltip).
   function Tooltip (S : PS.Summary) return String is ("Battery " & Row_Text (S));

   --  Columns of a Width-wide fill: Unknown none, any charge at least one.
   function Fill_Width (C : PS.Per_Mille; Width : Natural) return Natural
   with Pre => Width <= 1_000, Post => Fill_Width'Result <= Width;
end Desktop_Power_Text;
