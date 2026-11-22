with CuBit.Firmware_Snapshot;
-- Models the generated binding accessor, not actual startup authentication.
package Startup is
   function Tables return CuBit.Firmware_Snapshot.Authority is
     (CuBit.Firmware_Snapshot.Test_Authority);
end Startup;
