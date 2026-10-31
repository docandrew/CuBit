------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The channels the passive scan listens on in the world regulatory domain
--  (NET-WIFI-001 W1, docs/wifi.md "Regulatory domain", "World mode").
--
--  @description
--  wireless-regdb's world domain ("country 00"): 2402-2472 MHz (channels
--  1-13, 12 and 13 no-IR), 5170-5250 MHz (36-48, no-IR), 5250-5330 and
--  5490-5730 MHz (DFS), 5735-5835 MHz (149-165, no-IR). Listening is
--  allowed everywhere; we transmit nowhere (the scan is passive, no probe
--  request is ever sent). DFS channels are left out of the plan: no
--  radiation is initiated there by a client, and W1 does not need them.
--  Channel 14 (Japan only) is not in the world domain.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Wifi_Scan;

package Wifi_Channel_Plan with Pure, SPARK_Mode is
   package WS renames CuBit.Wifi_Scan;
   use type WS.Band;

   type Planned_Channel is record
      On_Band : WS.Band;
      Channel : WS.Channel_Number;
   end record;

   --  Centre frequency in MHz (IEEE 802.11-2020 Annex E: 2407 + 5n in
   --  2.4 GHz, 5000 + 5n in 5 GHz).
   subtype Frequency_MHz is Unsigned_16 range 2_412 .. 5_885;
   Base_2_4_MHz : constant := 2_407;
   Base_5_MHz   : constant := 5_000;
   Spacing_MHz  : constant := 5;

   function Frequency (P : Planned_Channel) return Frequency_MHz is
     (case P.On_Band is
        when WS.Band_2_4_GHz => Base_2_4_MHz + Spacing_MHz * Unsigned_16 (P.Channel),
        when WS.Band_5_GHz   => Base_5_MHz + Spacing_MHz * Unsigned_16 (P.Channel))
   with Pre => WS.Valid_Channel (P.On_Band, P.Channel)
               and then (if P.On_Band = WS.Band_2_4_GHz then P.Channel <= 13)
               and then (if P.On_Band = WS.Band_5_GHz then P.Channel <= 177);

   Plan_Length : constant := 22;
   subtype Plan_Index is Positive range 1 .. Plan_Length;
   type Plan_Array is array (Plan_Index) of Planned_Channel;

   World_Passive : constant Plan_Array :=
     [(WS.Band_2_4_GHz, 1),  (WS.Band_2_4_GHz, 2),  (WS.Band_2_4_GHz, 3),
      (WS.Band_2_4_GHz, 4),  (WS.Band_2_4_GHz, 5),  (WS.Band_2_4_GHz, 6),
      (WS.Band_2_4_GHz, 7),  (WS.Band_2_4_GHz, 8),  (WS.Band_2_4_GHz, 9),
      (WS.Band_2_4_GHz, 10), (WS.Band_2_4_GHz, 11), (WS.Band_2_4_GHz, 12),
      (WS.Band_2_4_GHz, 13),
      (WS.Band_5_GHz, 36),   (WS.Band_5_GHz, 40),   (WS.Band_5_GHz, 44),
      (WS.Band_5_GHz, 48),   (WS.Band_5_GHz, 149),  (WS.Band_5_GHz, 153),
      (WS.Band_5_GHz, 157),  (WS.Band_5_GHz, 161),  (WS.Band_5_GHz, 165)];

   --  Every planned channel is valid for its band (checked statically by
   --  the prover and by the hosted tests).
   function Plan_Valid return Boolean is
     (for all I in Plan_Index => WS.Valid_Channel (World_Passive (I).On_Band, World_Passive (I).Channel));

   Channels_On_2_4 : constant := 13;
   Channels_On_5   : constant := 9;
   pragma Compile_Time_Error (Channels_On_2_4 + Channels_On_5 /= Plan_Length, "plan length");

   --  How long the radio listens on each channel: two beacon intervals of
   --  the common 100 TU (102.4 ms), plus margin.
   Dwell_Milliseconds : constant := 220;

   --  Whether channel C on band B is one the plan listens on (a beacon's
   --  own DS channel outside the plan is still reported, as heard).
   function In_Plan (B : WS.Band; C : WS.Channel_Number) return Boolean is
     (for some I in Plan_Index => World_Passive (I).On_Band = B and then World_Passive (I).Channel = C);

end Wifi_Channel_Plan;
