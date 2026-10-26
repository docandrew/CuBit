with Interfaces;
with CCL.Interfaces.Wifi;

--  The networks the Wi-Fi radios heard (wifi.scan), one body per platform:
--  native/ asks netstack for each wireless interface's last passive scan
--  (CuBit.Net_Management.Scan_Results); the Linux preview's host/ has no
--  radio and lists nothing (a stand-in, not CuBit).
package CCL_Wifi is
   package Wifi renames CCL.Interfaces.Wifi;
   MAXIMUM_INTERFACE : constant := 16;              --  CuBit.Net_Link.Name_Characters
   BSSID_TEXT : constant := 17;                     --  "aa:bb:cc:dd:ee:ff"
   --  An SSID is up to 32 octets of anything; each shows as itself when
   --  printable ASCII, else as \xNN.
   MAXIMUM_SSID_TEXT : constant := 32 * 4;
   type Listed is record
      Interface_Name : String (1 .. MAXIMUM_INTERFACE) := [others => ' '];
      Interface_Length : Natural range 0 .. MAXIMUM_INTERFACE := 0;
      BSSID : String (1 .. BSSID_TEXT) := [others => '0'];
      SSID : String (1 .. MAXIMUM_SSID_TEXT) := [others => ' '];
      SSID_Length : Natural range 0 .. MAXIMUM_SSID_TEXT := 0;
      On_Band : Wifi.Band := Wifi.Band_2_4_GHz;
      Channel : Natural := 0;
      Signal : Integer range -128 .. 0 := -128;
      Protection : Wifi.Security := Wifi.Open;
      Capability : Interfaces.Unsigned_16 := 0;
      Heard : Natural := 0;
   end record;
   subtype Listed_Count is Natural range 0 .. Wifi.MAXIMUM_LISTED;
   type Listing is array (1 .. Wifi.MAXIMUM_LISTED) of Listed;
   type Result_Kind is (Listed_All, Not_Granted, Unavailable);
   --  Up to Count networks, strongest first (per interface, interfaces in
   --  netstack's order); Total counts every network the scans heard.
   procedure Scan
     (Entries : out Listing; Count : out Listed_Count; Total : out Natural; Result : out Result_Kind);
end CCL_Wifi;
