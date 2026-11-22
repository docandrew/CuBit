with Interfaces;
with CCL.Catalog;
with CCL.VM;
with CuBit.Messages;

package CCL_Native_Execution is
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Can_Replace return Boolean;
   function Waiting_For_IO return Boolean;
   function Cleanup_Needs_Retry return Boolean;
   procedure Maintain;
   procedure Load (Item : CCL.VM.Validated_Program; Fuel : Natural; Success : out Boolean);
   procedure Advance (Instructions : Natural; Result : out CCL.VM.Execution_Result);
   procedure Complete_Scalar (Value : CCL.VM.Value; Accepted : Boolean);
   procedure Stop;
   function Snapshot return CCL.VM.Machine_Snapshot;
   procedure Inspect (Result : out CCL.VM.Inspection_Snapshot);
   procedure Take_Changed (Changed : out Boolean);
   procedure Deliver (Receipt : CuBit.Messages.CompletionEntry; Consumed : out Boolean);
   function Next_Token return Interfaces.Unsigned_64;
   -- Shared process-wide counter for Config and desktop input waits.
end CCL_Native_Execution;
