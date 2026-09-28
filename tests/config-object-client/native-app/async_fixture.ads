with Interfaces;
with CCL.Types;
with CCL.VM;
with Config_Object_Client;
with CCL.Objects;

package Async_Fixture is
   type Scenario is (Read_Existing, Write_Initial, Denied_Write);
   procedure Run
     (Client : in out Config_Object_Client.Client; Contract : CCL.Objects.Binding;
      Types : CCL.Types.Registry;
      Expected : CCL.VM.Value; Revision : Interfaces.Unsigned_64;
      Token : in out Interfaces.Unsigned_64; Mode : Scenario; Good : out Boolean);
end Async_Fixture;
