------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Wifi_Replay with SPARK_Mode is

   procedure Accept_Frame (C : in out Counters; Class : Traffic_Class; PN : Packet_Number;
                           Accepted : out Boolean) is
   begin
      Accepted := C.Seen and then PN > C.Last (Class);
      if Accepted then
         C.Last (Class) := PN;
      end if;
   end Accept_Frame;

end Wifi_Replay;
