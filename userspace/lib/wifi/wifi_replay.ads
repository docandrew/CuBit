------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  CCMP replay protection on receive (IEEE 802.11-2020 12.5.3.4.4): one
--  counter per traffic class (QoS TIDs 0 .. 15) and one for non-QoS data,
--  per key (NET-WIFI-001 W4).
--
--  @description
--  A frame is accepted only when its packet number is above the last one
--  accepted in its class; a new key starts from its receive sequence
--  counter (the RSC handed over with it), below which nothing is
--  accepted.
--
--  Proved (level 2): no run-time errors; Accept_Frame accepts exactly the
--  packet numbers above the class's last, and then that number is the
--  class's last; a refused frame changes nothing.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Wifi_Replay with Pure, SPARK_Mode is

   subtype Packet_Number is Unsigned_64 range 0 .. 2 ** 48 - 1;
   Non_QoS : constant := 16;
   subtype Traffic_Class is Natural range 0 .. Non_QoS;

   type Last_Numbers is array (Traffic_Class) of Packet_Number;
   type Counters is record
      Last : Last_Numbers := [others => 0];
      Seen : Boolean := False;   --  a key is installed
   end record;

   --  A new key with receive sequence counter RSC: frames numbered RSC
   --  or below are replays (the AP's last transmitted number).
   function Start (RSC : Packet_Number) return Counters is ((Last => [others => RSC], Seen => True));

   procedure Accept_Frame (C : in out Counters; Class : Traffic_Class; PN : Packet_Number;
                           Accepted : out Boolean)
   with Post => Accepted = (C'Old.Seen and then PN > C'Old.Last (Class))
                and then (if Accepted then C.Last (Class) = PN
                                      and then (for all K in Traffic_Class =>
                                                  (if K /= Class then C.Last (K) = C'Old.Last (K)))
                                      and then C.Seen
                          else C = C'Old);

end Wifi_Replay;
