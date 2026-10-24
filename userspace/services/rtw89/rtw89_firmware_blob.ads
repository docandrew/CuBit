------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Realtek firmware embedded in rtw89.drv (docs/wifi.md, D6): the only
--  unit that names ccl-build's generated Embedded package.
--
--  @description
--  firmware/rtw89/rtw8852b_fw-1.bin (linux-firmware 20260110, SHA-256
--  5b68415e...cd5d3a, checked by ccl-build), linked unmodified; licence
--  firmware/LICENCE.rtlwifi_firmware.txt in the bundle.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with Embedded;

package RTW89_Firmware_Blob is

   Bytes : constant := Embedded.Rtw8852b_Firmware_Bytes;

   function Address return System.Address is (Embedded.Rtw8852b_Firmware'Address);

end RTW89_Firmware_Blob;
