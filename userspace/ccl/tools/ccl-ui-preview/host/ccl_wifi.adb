--  The Linux preview's stand-in for the wifi.* calls: the preview has no
--  CuBit radio, no netstack and no wifi.svc, so it lists no networks and
--  refuses the connection calls as not granted (a Linux-hosted stand-in,
--  not CuBit; it does not read or use the host's Wi-Fi).
package body CCL_Wifi is
   procedure Scan
     (Entries : out Listing; Count : out Listed_Count; Total : out Natural; Result : out Result_Kind) is
   begin
      Entries := [others => (others => <>)];
      Count := 0;
      Total := 0;
      Result := Listed_All;
   end Scan;

   procedure Join (SSID : String; Status : out Connection; Result : out Control_Result) is
      pragma Unreferenced (SSID);
   begin
      Status := (others => <>);
      Result := Not_Granted;
   end Join;

   procedure Leave (Status : out Connection; Result : out Control_Result) is
   begin
      Status := (others => <>);
      Result := Not_Granted;
   end Leave;

   procedure Get_Status (Status : out Connection; Result : out Control_Result) is
   begin
      Status := (others => <>);
      Result := Not_Granted;
   end Get_Status;

   procedure Request_Passphrase (SSID : String; Result : out Control_Result) is
      pragma Unreferenced (SSID);
   begin
      Result := Not_Granted;
   end Request_Passphrase;

   function Passphrase_Requested return Boolean is (False);
   function Requested_SSID return String is ("");

   procedure Store_Passphrase (Passphrase : String; Result : out Control_Result) is
      pragma Unreferenced (Passphrase);
   begin
      Result := Not_Granted;
   end Store_Passphrase;

   procedure Cancel_Passphrase is null;
end CCL_Wifi;
