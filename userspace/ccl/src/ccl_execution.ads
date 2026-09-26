with CCL.Catalog;
with CCL.VM;

-- Platform execution facade for the Workbench's single debugger. Native
-- resource ownership lives in reusable Config_Object_Client.Resources.Runs,
-- not in widgets. A remote host can own one such runner per admitted session.
package CCL_Execution is
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Can_Replace return Boolean;
   function Waiting_For_IO return Boolean;
   procedure Load (Item : CCL.VM.Validated_Program; Fuel : Natural; Success : out Boolean);
   procedure Advance (Instructions : Natural; Result : out CCL.VM.Execution_Result);
   procedure Complete_Scalar (Value : CCL.VM.Value; Accepted : Boolean);
   procedure Stop;
   function Snapshot return CCL.VM.Machine_Snapshot;
   procedure Inspect (Result : out CCL.VM.Inspection_Snapshot);
   procedure Take_Changed (Changed : out Boolean);
end CCL_Execution;
