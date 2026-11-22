with Ada.Text_IO;
with Interfaces; use Interfaces;
with XHCI_Ports; use XHCI_Ports;

procedure Port_Tests is
begin
   pragma Assert (Before_Enumeration (0) = Disconnected);
   --  Exact laptop snapshot: powered SuperSpeed, connected, reset active.
   pragma Assert (Before_Enumeration (16#1211#) = Wait_For_Reset);
   pragma Assert (Before_Enumeration (16#1203#) = Already_Enabled);
   pragma Assert (Before_Enumeration (16#8000_1203#) = Wait_For_Reset);
   pragma Assert (Before_Enumeration (16#1201#) = Start_Reset);
   pragma Assert (Before_Enumeration (16#0E03#) = Start_Reset);
   pragma Assert (Reset_Write (16#0E03#) = 16#0210#);
   pragma Assert (Reset_Write (16#FFFF_FFFF#) = 16#0E00_C210#);
   for Bit in 0 .. 31 loop
      declare
         Written : constant Unsigned_32 := Reset_Write (Shift_Left (1, Bit));
      begin
         --  Neither disable the port, clear changes, change link state, nor
         --  accidentally trigger a warm reset by echoing a status register.
         pragma Assert ((Written and 16#80FF_01E2#) = 0);
         pragma Assert ((Written and 16#10#) /= 0);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("XHCI-PORTS: PASS in-progress reset and safe PORTSC writes");
end Port_Tests;
