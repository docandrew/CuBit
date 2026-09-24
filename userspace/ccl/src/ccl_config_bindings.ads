with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
package CCL_Config_Bindings is
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Value : out CCL.Host_Values.Value; Success : out Boolean);
end CCL_Config_Bindings;
