with Interfaces;
with CCL.Objects;
with Config_Object_Client;

package Read_Fixture is
   procedure Run
     (Client : in out Config_Object_Client.Client; Contract : CCL.Objects.Binding;
      Expected : CCL.Objects.Image; Revision : Interfaces.Unsigned_64;
      Token : in out Interfaces.Unsigned_64; Good : out Boolean);
   procedure Store
     (Client : in out Config_Object_Client.Client; Contract : CCL.Objects.Binding;
      Value : CCL.Objects.Image; Expected_New_Revision : Interfaces.Unsigned_64;
      Token : in out Interfaces.Unsigned_64; Good : out Boolean;
      Value_Source : String := "(config-test.supplied)");
   -- Feed an owned aggregate through interpreted CCL to the native Set host,
   -- checking the real typed Committed receipt. No client serialization.
   -- Revision zero expects Missing; otherwise Found with the exact native
   -- value. Blocking activity wait belongs only to this dedicated test host.
end Read_Fixture;
