with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;

--  secrets.* (CCL.Interfaces.Secrets) over the platform's CCL_Secrets. No
--  operation answers a secret's value: (secrets.set s) only arms the
--  console's non-echoing entry, which hands the value to secrets.svc.
package CCL_Secrets_Bindings is
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result);
end CCL_Secrets_Bindings;
