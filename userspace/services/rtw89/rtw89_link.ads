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
--  wake epoch, and nothing else. No frame is ever put in the transmit
--  path: W1 receives only.
------------------------------------------------------------------------------
pragma Ada_2022;
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

end RTW89_Link;
