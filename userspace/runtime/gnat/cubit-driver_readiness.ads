------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A driver's readiness report to devmgr (docs/device-matching.md).
--
--  @description
--  A driver devmgr started sends one of these labels, with no words, on its
--  "device-manager" endpoint once it has initialized: Ready when it drives
--  the device (devmgr then unmasks its MSI-X interrupts), Not_Present when
--  it declines it (devmgr releases its vectors). devmgr waits for one
--  before it starts the next driver.
------------------------------------------------------------------------------
with Interfaces;

package CuBit.Driver_Readiness with Pure, SPARK_Mode is
   Ready_Label       : constant Interfaces.Unsigned_32 := 16#FF00#;
   Not_Present_Label : constant Interfaces.Unsigned_32 := 16#FF01#;
end CuBit.Driver_Readiness;
