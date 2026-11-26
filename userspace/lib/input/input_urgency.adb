------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Handles.Devices;
with Latency_Contract_ABI;

package body Input_Urgency is
   function Reserve (Authority : CuBit.Handles.Scheduling) return Boolean is
     (CuBit.Handles.Devices.Reserve_Interactive
        (Period_Us => Period_Us, Budget_Us => Budget_Us, Authority => Authority,
         Flags => Latency_Contract_ABI.Input_Wakes_Only) /= Unsigned_64'Last);
end Input_Urgency;
