with CCL_Native_Execution;
package body CCL_Execution is
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
     renames CCL_Native_Execution.Install;
   function Can_Replace return Boolean renames CCL_Native_Execution.Can_Replace;
   function Waiting_For_IO return Boolean renames CCL_Native_Execution.Waiting_For_IO;
   procedure Load (Item : CCL.VM.Validated_Program; Fuel : Natural; Success : out Boolean)
     renames CCL_Native_Execution.Load;
   procedure Advance (Instructions : Natural; Result : out CCL.VM.Execution_Result)
     renames CCL_Native_Execution.Advance;
   procedure Complete_Scalar (Value : CCL.VM.Value; Accepted : Boolean)
     renames CCL_Native_Execution.Complete_Scalar;
   procedure Stop renames CCL_Native_Execution.Stop;
   function Snapshot return CCL.VM.Machine_Snapshot renames CCL_Native_Execution.Snapshot;
   procedure Inspect (Result : out CCL.VM.Inspection_Snapshot) renames CCL_Native_Execution.Inspect;
   procedure Take_Changed (Changed : out Boolean) renames CCL_Native_Execution.Take_Changed;
end CCL_Execution;
