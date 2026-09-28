with Interfaces;
with CCL.Objects;
with CCL.Types;
with CCL.VM;
with Config_Object_Client;
with CCL.Catalog;

package Source_Fixture is
   type Operation is (Get_Value, Set_Value);
   for Operation use (Get_Value => 1, Set_Value => 2);
   procedure Configure
     (Catalog : out CCL.Catalog.Interface_Catalog; Grants : out CCL.Catalog.Granted_Bindings;
      Contract : CCL.Objects.Binding; Types : CCL.Types.Registry;
      Write : Boolean; Good : out Boolean);
   procedure Run
     (Client : in out Config_Object_Client.Client;
      Contract : CCL.Objects.Binding; Types : CCL.Types.Registry;
      Source : String; Expected : CCL.VM.Value;
      Revision : Interfaces.Unsigned_64; Token : in out Interfaces.Unsigned_64;
      Write : Boolean; Good : out Boolean);
end Source_Fixture;
