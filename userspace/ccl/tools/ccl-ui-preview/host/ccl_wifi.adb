--  The Linux preview's stand-in for wifi.scan: the preview has no CuBit
--  radio and no netstack, so it lists no networks (a Linux-hosted
--  stand-in, not CuBit; it does not read the host's Wi-Fi).
package body CCL_Wifi is
   procedure Scan
     (Entries : out Listing; Count : out Listed_Count; Total : out Natural; Result : out Result_Kind) is
   begin
      Entries := [others => (others => <>)];
      Count := 0;
      Total := 0;
      Result := Listed_All;
   end Scan;
end CCL_Wifi;
