with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
--  The Workbench's logs interface: (logs.recent "service"), answered by
--  CCL_Log_IO.
package CCL_Log_Bindings is
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result);
end CCL_Log_Bindings;
