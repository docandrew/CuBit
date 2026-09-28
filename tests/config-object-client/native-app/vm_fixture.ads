with CCL.Types;
with CCL.VM;
with CCL.Catalog;

package VM_Fixture is
   function Empty_Catalog return CCL.Catalog.Interface_Catalog;
   procedure Run
     (Source : String; Types : out CCL.Types.Registry;
      Value : out CCL.VM.Value; Good : out Boolean;
      Catalog : CCL.Catalog.Interface_Catalog := Empty_Catalog);
end VM_Fixture;
