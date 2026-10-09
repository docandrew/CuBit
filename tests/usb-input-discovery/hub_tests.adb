with Ada.Text_IO;
with Interfaces; use Interfaces;
with USB_Hubs; use USB_Hubs;
with XHCI_Topology;
procedure Hub_Tests is
   Data : Bytes (1 .. 9) := [9, 16#29#, 4, 1, 0, 50, 0, 0, 255];
   Value : Descriptor;
   Result : Decode_Result;
   Short_Eight_Port : constant Bytes :=
     [10, 16#29#, 8, 16#0A#, 0, 1, 0, 0, 0, 255];
   use type XHCI_Topology.Speed;
begin
   Decode (Data, Value, Result);
   pragma Assert (Result = Decoded and Value.Ports = 4);
   pragma Assert (Value.Power = Individual and Value.Power_Delay_MS = 100);
   for Length in 0 .. 7 loop
      Decode (Data (1 .. Length), Value, Result);
      pragma Assert (Result = Malformed);
   end loop;
   Data (3) := 0;
   Decode (Data, Value, Result);
   pragma Assert (Result = Malformed);
   --  QEMU's eight-port hub: DeviceRemovable two bytes (eight ports plus
   --  bit zero), the obsolete PortPwrCtrlMask only one.
   Decode (Short_Eight_Port, Value, Result);
   pragma Assert (Result = Decoded and Value.Ports = 8);
   --  The mask may be absent, but DeviceRemovable may not be short.
   Decode ([9, 16#29#, 8, 16#0A#, 0, 1, 0, 0, 0], Value, Result);
   pragma Assert (Result = Decoded and Value.Ports = 8);
   Decode ([8, 16#29#, 8, 16#0A#, 0, 1, 0, 0], Value, Result);
   pragma Assert (Result = Malformed);
   --  Longer than both bitmaps at full width.
   Decode ([12, 16#29#, 8, 16#0A#, 0, 1, 0, 0, 0, 255, 255, 255], Value, Result);
   pragma Assert (Result = Malformed);
   --  bLength must match the bytes received.
   Decode ([11, 16#29#, 8, 16#0A#, 0, 1, 0, 0, 0, 255], Value, Result);
   pragma Assert (Result = Malformed);
   Data (3) := 16; -- Sixteen ports plus bit zero need three DeviceRemovable bytes.
   Decode (Data, Value, Result);
   pragma Assert (Result = Malformed);
   Data (3) := 4; Data (4) := 3;
   Decode (Data, Value, Result);
   pragma Assert (Result = Decoded and Value.Power = Always_On);
   for Ports in 1 .. 255 loop
      declare
         Frame : Bytes (1 .. 7 + 2 * ((Ports + 8) / 8)) := [others => 0];
      begin
         Frame (1) := Unsigned_8 (Frame'Length);
         Frame (2) := 16#29#;
         Frame (3) := Unsigned_8 (Ports);
         Frame (6) := 255;
         Decode (Frame, Value, Result);
         pragma Assert (Result = Decoded and Value.Ports = Ports);
         pragma Assert (Value.Power_Delay_MS = 510);
      end;
   end loop;
   for Status in Unsigned_16 loop
      if Action (Status) = Ready then
         pragma Assert ((Status and 16#0103#) = 16#0103#);
         pragma Assert ((Status and 16#001C#) = 0);
         pragma Assert ((Status and 16#0600#) /= 16#0600#);
         pragma Assert (Rate (Status) /= XHCI_Topology.Super_Speed);
      end if;
   end loop;
   pragma Assert (Action (16#0101#) = Reset_Required);
   pragma Assert (Action (16#0111#) = Resetting);
   pragma Assert (Action (16#010B#) = Overcurrent);
   pragma Assert (Rate (16#0103#) = XHCI_Topology.Full_Speed);
   pragma Assert (Rate (16#0303#) = XHCI_Topology.Low_Speed);
   pragma Assert (Rate (16#0503#) = XHCI_Topology.High_Speed);
   Ada.Text_IO.Put_Line ("PASS: USB2 hub descriptor and port status");
end Hub_Tests;
