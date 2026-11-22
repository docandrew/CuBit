with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;

--  calc.* (CCL.Interfaces.Calc): the calculator core (userspace/lib/calc)
--  answering in the calling process. No IPC: the core is statically
--  linked, so a call costs what the arithmetic costs.
package CCL_Calc_Bindings is
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result);
end CCL_Calc_Bindings;
