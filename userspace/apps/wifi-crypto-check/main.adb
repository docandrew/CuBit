pragma Ada_2022;
with Interfaces;
with CuBit.Messages; use CuBit.Messages;
with WiFi_Crypto.Self_Test;

--  rtw89's WiFi_Crypto known-answer self-test on CuBit, with the crates
--  compiled natively by ccl-build: the same unit and line rtw89 logs after
--  probe, which a QEMU run never reaches (no RTL8852BE).
procedure Main is
   Results : WiFi_Crypto.Self_Test.Outcomes;
   Passed  : Boolean;
   Ignore  : Interfaces.Unsigned_64;
begin
   WiFi_Crypto.Self_Test.Run (Results);
   Passed := WiFi_Crypto.Self_Test.All_Passed (Results);
   debugPrint ("wifi-crypto-check: " & WiFi_Crypto.Self_Test.Image (Results) & " "
               & (if Passed then "PASS" else "FAIL") & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT);
end Main;
