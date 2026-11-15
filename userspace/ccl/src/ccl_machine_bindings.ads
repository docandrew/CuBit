with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;

--  machine.* (CCL.Interfaces.Machine) over the platform's CCL_Machine.
package CCL_Machine_Bindings is
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result);
end CCL_Machine_Bindings;
