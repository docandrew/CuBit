with CCL.VM.Native_Objects;
with Config_Object_Client.Resources.Calls;
with Config_Object_Interfaces;
with Config_Read_Outcomes;

-- Single-owner event-loop coordinator, independent of widgets or a transport
-- wait primitive. The program, machine, registry and collection have ONE stable
-- lifetime. Callers cannot replace the program while its effects are draining.
package Config_Object_Client.Resources.Runs is
   subtype Binding_ID is Interfaces.Unsigned_32;
   type Binding_Table is array (Config_Object_Interfaces.Operation) of Binding_ID;
   type Runner (Context : CCL.Resources.Context_ID) is limited private;

   procedure Configure
     (Host : in out Runner; Types : CCL.Types.Registry;
      Kind : CCL.Types.Type_Reference; Contract : CCL.Objects.Binding;
      Reads : Config_Read_Outcomes.Description; Bindings : Binding_Table;
      Endpoint : CuBit.Messages.CapabilitySlot; Name : String;
      Accepted : out Boolean);
   -- Trusted host-only: metadata and endpoint must come from its approved
   -- catalog/grants. One collection slot per run; concurrent opens fail locally.
   -- Configuration cannot change during a loaded execution, even if no I/O
   -- has started. A runner may execute ordinary scalar programs unconfigured.

   function Can_Replace (Host : Runner) return Boolean;
   function Background_Work (Host : Runner) return Boolean;
   function Waiting_For_IO (Host : Runner) return Boolean;
   function Cleanup_Retry_Needed (Host : Runner) return Boolean;
   procedure Load
     (Host : in out Runner; Item : CCL.VM.Validated_Program; Fuel : Natural;
      Accepted : out Boolean) with Pre => CCL.VM.Is_Valid (Item);
   -- Rejected Load is non-mutating. Tokens belong to the process-wide
   -- dispatcher: share the same monotonically increasing counter across runners.
   procedure Advance
     (Host : in out Runner; Instructions : Natural; Tokens : in out Number;
      Result : out CCL.VM.Execution_Result);
   procedure Complete
     (Host : in out Runner; Receipt : CuBit.Messages.CompletionEntry;
      Tokens : in out Number; Accepted : out Boolean);
   -- Receipt must come from the authenticated kernel completion queue.
   -- Does not poll, wait or resume another runner. Foreign/duplicate receipts
   -- return Accepted=False for dispatch to another registered client.
   procedure Maintain (Host : in out Runner; Tokens : in out Number);
   -- Retry only cleanup/grant retirement; never replay a data operation.
   procedure Stop (Host : in out Runner; Tokens : in out Number);
   procedure Complete_Scalar
     (Host : in out Runner; Value : CCL.VM.Value; Accepted : Boolean);
   -- For unrelated, synchronous host bindings only; cannot complete Config I/O.
   function Snapshot (Host : Runner) return CCL.VM.Machine_Snapshot;
   procedure Inspect (Host : Runner; Result : out CCL.VM.Inspection_Snapshot);
private
   type Pending_Operation is (No_IO, Opening, Data_Call, Closing_Call, Cleanup_Call);
   type Runner (Context : CCL.Resources.Context_ID) is limited record
      Owner : CCL.Resources.Registry (Context);
      Session : CCL.Resources.Run := CCL.Resources.No_Run;
      Object : Collection;
      Call : Calls.Invocation;
      Program : CCL.VM.Validated_Program;
      Machine : CCL.VM.Native_Objects.Machine;
      Last : CCL.VM.Execution_Result;
      Loaded, Configured, Running : Boolean := False;
      Pending : Pending_Operation := No_IO;
      Types : CCL.Types.Registry;
      Kind : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      Contract : CCL.Objects.Binding;
      Reads : Config_Read_Outcomes.Description;
      Bindings : Binding_Table := [others => 0];
      Endpoint : CuBit.Messages.CapabilitySlot := 0;
      Name : String (1 .. Config_Object_Messages.Maximum_Name) := [others => ' '];
      Name_Length : Natural range 0 .. Config_Object_Messages.Maximum_Name := 0;
      Revision : Number := 0;
   end record;
end Config_Object_Client.Resources.Runs;
