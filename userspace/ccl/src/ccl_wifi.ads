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

   --  The connection (wifi.svc, CuBit.Wifi_Control).
   MAXIMUM_FAILURE_TEXT : constant := 32;
   type Connection is record
      State : Wifi.State := Wifi.No_Radio;
      Failure : String (1 .. MAXIMUM_FAILURE_TEXT) := [others => ' '];
      Failure_Length : Natural range 0 .. MAXIMUM_FAILURE_TEXT := 0;
      BSSID : String (1 .. BSSID_TEXT) := [others => '0'];
      On_Band : Wifi.Band := Wifi.Band_2_4_GHz;
      Channel : Natural := 0;
   end record;
   type Control_Result is
     (Done, Not_Granted, Unavailable, Malformed, No_Radio, Not_Found, Not_Joinable, No_Passphrase,
      Busy, Not_Stored);
   function Explain (R : Control_Result) return String is
     (case R is
        when Done => "done",
        when Not_Granted => "the program's manifest must request the wifi service (service ""wifi"" ""wifi"")",
        when Unavailable => "wifi.svc did not answer",
        when Malformed => "not an SSID wifi.svc can name (1 .. 32 octets, not ending in NUL)",
        when No_Radio => "no Wi-Fi radio is attached",
        when Not_Found => "no network with that SSID in the last scan",
        when Not_Joinable => "the network's security is not one CuBit joins (WPA2-PSK with CCMP, or open)",
        when No_Passphrase => "no passphrase stored for that network: use (wifi.passphrase ""ssid"") first",
        when Busy => "a join is under way: (wifi.leave) first",
        when Not_Stored => "the passphrase could not be stored");
   procedure Join (SSID : String; Status : out Connection; Result : out Control_Result);
   procedure Leave (Status : out Connection; Result : out Control_Result);
   procedure Get_Status (Status : out Connection; Result : out Control_Result);

   --  (wifi.passphrase "ssid") asks the console for the passphrase: the
   --  next line typed is not echoed, not kept in history, and goes to
   --  wifi.svc (Store_Passphrase), never to the program. A program without
   --  the wifi service is refused at once.
   procedure Request_Passphrase (SSID : String; Result : out Control_Result);
   function Passphrase_Requested return Boolean;
   function Requested_SSID return String;
   procedure Store_Passphrase (Passphrase : String; Result : out Control_Result);
   procedure Cancel_Passphrase;
end CCL_Wifi;
