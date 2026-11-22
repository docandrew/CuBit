with CCL.Interfaces.Debug;
with CuBit.Failures;

--  The Linux preview's debug.*: the same interface, published so programs
--  type-check alike, and refused: a Linux-hosted preview has no CuBit
--  kernel session to attach through (a Linux stand-in, never CuBit).
package body CCL_Debug_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   package DI renames CCL.Interfaces.Debug;

   function Handles (Binding : Unsigned_32) return Boolean is
     (Binding in DI.FIRST_BINDING .. DI.Binding_Of (DI.Operation'Last));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      DI.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      for Op in DI.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "debug." & DI.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, DI.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Binding, Argument);
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False,
                Why => CuBit.Failures.Failed
                  (CuBit.Failures.Unavailable,
                   "debug.* attaches only on CuBit: the Linux preview has no kernel debug session",
                   "run it on CuBit, in the console or the Workbench"));
   end Invoke;
end CCL_Debug_Bindings;
