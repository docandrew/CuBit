with CCL.VM.Native_Objects;
with Config_Read_Outcomes;

-- One-shot typed Config calls for an owned CCL collection. The host selects
-- Action/Binding from its granted operation table and retains the same
-- program, machine, registry and collection until Resume or Drain completes.
-- Single owner/event loop; this adapter never waits or polls.
package Config_Object_Client.Resources.Calls is
   type Operation is (Read_Value, Write_Value);
   type Invocation is limited private;
   function Pending (Call : Invocation) return Boolean;

   procedure Submit
     (Call : in out Invocation; Object : in out Collection;
      Owner : in out CCL.Resources.Registry;
      Action : Operation; Binding : Interfaces.Unsigned_32;
      Item : CCL.VM.Validated_Program;
      Machine : in out CCL.VM.Native_Objects.Machine;
      Read_Description : Config_Read_Outcomes.Description;
      Expected_Revision, Token : Number; Result : out Submission)
     with Pre => CCL.VM.Is_Valid (Item);
   -- Checks the actual receiver, input and output schemas before any IPC.
   -- Successful submission retains the exact registry call ticket and
   -- acknowledges the VM borrow. A rejected submission leaves it offered.

   type Resume_State is (No_Completion, Other_Call, Type_Mismatch, Resumed);
   procedure Resume
     (Call : in out Invocation; Object : in out Collection;
      Owner : in out CCL.Resources.Registry;
      Item : CCL.VM.Validated_Program;
      Machine : in out CCL.VM.Native_Objects.Machine;
      Result : out Resume_State)
     with Pre => CCL.VM.Is_Valid (Item);
   -- Complete the collection with an authenticated kernel receipt first.
   -- Missing/denied/stale reads and rejected/uncertain writes are ordinary
   -- typed outcomes. A stopped machine or different receiver/call cannot
   -- consume this completion; it remains available for Drain.

   procedure Drain
     (Call : in out Invocation; Object : in out Collection;
      Owner : in out CCL.Resources.Registry;
      Item : out Response; Taken : out Boolean);
   -- After stopping the owning machine, drain the original operation before
   -- resource cleanup. Draining never resumes code or retries the operation.
private
   type Invocation is limited record
      Active : Boolean := False;
      Ticket : CCL.Resources.Ticket := CCL.Resources.No_Ticket;
      Receiver : CCL.Resources.Reference := CCL.Resources.No_Reference;
      Binding : Interfaces.Unsigned_32 := 0;
      Import_Index : CCL.VM.Import_Index := 0;
      Action : Operation := Read_Value;
      Read_Description : Config_Read_Outcomes.Description;
   end record;
   function Pending (Call : Invocation) return Boolean is (Call.Active);
end Config_Object_Client.Resources.Calls;
