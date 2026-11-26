------------------------------------------------------------------------------
--  CuBit OS
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  SET_LATENCY_CONTRACT's flags word (arg3), a set (docs/scheduler.md §4).
--
--  Input_Wakes_Only: an INTERACTIVE holder lends urgent turns only on its
--  input wakes: the events it publishes (SEND_EVENT) and the channel wakes
--  it marks Futex_ABI.Urgent_Hint (an input source ring's signal to
--  Desktop). Its other wakes (replies, calls, unmarked channel and futex
--  wakes) are ordinary. For a driver that serves more than input (xhci.drv
--  and its USB disks, hid-i2c.drv and its I2C bus transactions): only the
--  HID report path spends the urgent budget. Without it every wake the
--  holder's system calls make lends (Desktop: its replies are the input
--  path).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Latency_Contract_ABI with Pure, SPARK_Mode is
   No_Flags         : constant Unsigned_64 := 0;
   Input_Wakes_Only : constant Unsigned_64 := 1;
   --  Every flag this kernel knows; a contract naming another is refused.
   Defined_Flags    : constant Unsigned_64 := Input_Wakes_Only;

   function Defined (Flags : Unsigned_64) return Boolean is
     ((Flags and not Defined_Flags) = 0);
   function Input_Only (Flags : Unsigned_64) return Boolean is
     ((Flags and Input_Wakes_Only) /= 0);
end Latency_Contract_ABI;
