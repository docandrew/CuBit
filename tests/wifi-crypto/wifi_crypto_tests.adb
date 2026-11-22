--  Known-answer tests for the Wi-Fi driver's crypto units (WiFi_Crypto,
--  userspace/lib/wifi/crypto; docs/wifi.md, W2). Linux-hosted.
with Ada.Command_Line;
with Checks;
with AES_Tests;
with HMAC_Tests;
with P256_Tests;
with WiFi_Crypto.Self_Test;

procedure Wifi_Crypto_Tests is
   use WiFi_Crypto.Self_Test;
   Self_Test_Results : Outcomes;
begin
   HMAC_Tests.Run;
   AES_Tests.Run;
   P256_Tests.Run;
   --  rtw89's start-up self-test, as the driver and wifi-crypto-check run it.
   Run (Self_Test_Results);
   for Test in KAT loop
      Checks.Check ("self-test " & Name (Test), Self_Test_Results (Test));
   end loop;
   if not Checks.Report then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Wifi_Crypto_Tests;
