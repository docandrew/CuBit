with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;

--  Every system interface a CCL front end offers (the Workbench, the console,
--  the remote session host): discovery, grants and dispatch in one place, so
--  no interface exists in one front end only. A front end adds only its own
--  UI interfaces (the Workbench's ui.*), and supplies its platform's clock.
generic
   --  Monotonic milliseconds, or Available = False.
   with function Monotonic_Ms (Available : out Boolean) return Interfaces.Unsigned_64;
package CCL_Host_Environment is
   --  config (inspector) and typed Config collections, clock, logs: each
   --  published and granted only where this process may use it.
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result);
end CCL_Host_Environment;
