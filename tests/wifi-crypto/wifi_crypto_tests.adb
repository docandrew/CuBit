--  Known-answer tests for the Wi-Fi driver's crypto units (WiFi_Crypto,
--  userspace/services/rtw89/crypto; docs/wifi.md, W2). Linux-hosted.
with Ada.Command_Line;
with Checks;
with AES_Tests;
with HMAC_Tests;
with P256_Tests;

procedure Wifi_Crypto_Tests is
begin
   HMAC_Tests.Run;
   AES_Tests.Run;
   P256_Tests.Run;
   if not Checks.Report then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Wifi_Crypto_Tests;
