with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;

--  debug.* (CCL.Interfaces.Debug, DBG-001): the debugger engine (CuBit.Debug)
--  through the kernel, for agents and the console. One body per platform:
--  native/ attaches through procmgr's "debug-observe for the session"
--  authority (CuBit.Launch_Handles.Session_Debug_Operation); the Linux
--  preview's host/ publishes the same interface and refuses every call
--  (it has no CuBit kernel to attach through).
package CCL_Debug_Bindings is
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result);
end CCL_Debug_Bindings;
