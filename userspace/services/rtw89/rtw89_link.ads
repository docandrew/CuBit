------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  rtw89.drv's side of the driver contract with netstack (docs/net-link.md,
--  CuBit.Net_Link): a Wireless interface named by its PCI function, its
--  link report (Down, not associated: W1 never joins a network) and the
--  scan block (CuBit.Wifi_Scan) netstack hands to inspectors such as the
--  console's (wifi.scan).
--
--  @description
--  Both reports are level values in the frame grant's header page under
--  their seqlocks; a change costs the receive doorbell, once per netstack
--  wake epoch, and nothing else. While the 802.1X port is open (W3/W4)
--  the link is Up and Ethernet frames move through the grant's rings:
--  received data frames as Ethernet into the receive ring, netstack's
--  frames from the transmit ring to the air (RTW89_Station). The driver
--  parks one Transmit_Wait with netstack, answered when it queues frames.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with CuBit.Frame_Rings;
with CuBit.Net_Link;
with CuBit.Wifi_Scan;

package RTW89_Link is

   type Attach_Status is (Attached, No_Netstack, Refused, Grant_Refused);
   function Name (S : Attach_Status) return String is
     (case S is
        when Attached      => "attached",
        when No_Netstack   => "no-netstack",
        when Refused       => "refused",
        when Grant_Refused => "grant-refused");

   --  Attach as a Wireless interface; publish Down (Not_Associated).
   procedure Attach (Address : CuBit.Net_Link.Hardware_Address; Location : CuBit.Net_Link.PCI_Location;
                     Status : out Attach_Status);

   function Is_Attached return Boolean;

   --  The interface's name (pci-02-00-0), for log lines.
   function Interface_Name return String;

   --  Publish a scan result (seqlock), then ring netstack if it sleeps.
   procedure Publish (Result : CuBit.Wifi_Scan.Scan_Result)
     with Pre => CuBit.Wifi_Scan.Normal (Result);

   --  The link report: Up (associated, port open) with the BSS, or Down.
   procedure Report_Up (Detail : CuBit.Net_Link.Up_Detail);
   procedure Report_Down (Reason : CuBit.Net_Link.Down_Reason);

   type Frame_Bytes is array (Natural range <>) of Interfaces.Unsigned_8;

   --  An Ethernet frame for netstack (receive ring); dropped (counted)
   --  when the ring is full. Ring_Receive rings once per batch.
   procedure Deliver (Frame : Frame_Bytes) with Pre => Frame'First = 0;
   procedure Ring_Receive;

   --  netstack's next queued Ethernet frame, copied out.
   subtype Ethernet_Buffer is Frame_Bytes (0 .. CuBit.Frame_Rings.Maximum_Frame - 1);
   procedure Next_Frame (Frame : out Ethernet_Buffer; Length : out Natural; Found : out Boolean);

   --  The parked Transmit_Wait: Take is true for its completion; Park
   --  submits the next one after publishing a wake epoch.
   function Take (Entry_Found : CuBit.Messages.CompletionEntry) return Boolean;
   procedure Park;

   type Statistics is record
      Delivered, Dropped, Taken : Natural := 0;
   end record;
   function Counts return Statistics;

end RTW89_Link;
