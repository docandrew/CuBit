------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  rtw89.drv's side of the contract with wifi.svc (CuBit.Wifi_Driver):
--  attach, the frame grant both ways, the scan block, the parked
--  Command_Wait and Command_Done (NET-WIFI-001 W3/W4).
--
--  @description
--  As with netstack (RTW89_Link), the driver calls and rings wifi.svc and
--  parks one Command_Wait with it; wifi.svc answers that wait with a
--  command. Next_Command returns an answered command once; the caller
--  carries it out, reports with Done, and the next wait is parked.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Net_Link;
with CuBit.Wifi_Driver;
with CuBit.Wifi_Scan;

package RTW89_Wifi_Link is
   package D renames CuBit.Wifi_Driver;

   type Attach_Status is (Attached, No_Wifi_Service, Refused, Grant_Refused);
   function Name (S : Attach_Status) return String is
     (case S is
        when Attached        => "attached",
        when No_Wifi_Service => "no-wifi-service",
        when Refused         => "refused",
        when Grant_Refused   => "grant-refused");

   procedure Attach (Address : CuBit.Net_Link.Hardware_Address; Status : out Attach_Status);
   function Is_Attached return Boolean;

   --  The scan block, as for netstack.
   procedure Publish (Result : CuBit.Wifi_Scan.Scan_Result)
     with Pre => CuBit.Wifi_Scan.Normal (Result);

   type Frame_Bytes is array (Natural range <>) of Unsigned_8;

   --  A frame for wifi.svc (receive ring), then the doorbell. Dropped
   --  (counted) when the ring is full.
   procedure Deliver (Kind : D.Frame_Kind; Frame : Frame_Bytes)
     with Pre => Frame'First = 0;

   --  The next frame wifi.svc queued (transmit ring), copied out.
   Maximum_Frame : constant := D.Maximum_Frame;
   subtype Frame_Buffer is Frame_Bytes (0 .. Maximum_Frame - 1);
   procedure Next_Frame (Kind : out D.Frame_Kind; Frame : out Frame_Buffer; Length : out Natural;
                         Found : out Boolean);

   --  A completion this package is waiting for (its token); call for every
   --  completion polled. True when it was ours.
   function Take (Entry_Found : CuBit.Messages.CompletionEntry) return Boolean;

   --  Park a Command_Wait unless one is parked or a command is pending.
   procedure Park;

   --  The command wifi.svc answered with, once.
   type Pending_Command is record
      Present : Boolean := False;
      Label   : Unsigned_32 := 0;
      Words   : CuBit.Messages.MessageWords := [others => 0];
   end record;
   function Next_Command return Pending_Command;

   procedure Done (Label : Unsigned_32; Result : D.Outcome);

   type Statistics is record
      Delivered, Dropped, Taken, Refused_Frames : Natural := 0;
   end record;
   function Counts return Statistics;

end RTW89_Wifi_Link;
