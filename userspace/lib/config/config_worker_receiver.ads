with Interfaces;
with CuBit.Messages;
with CCL.Objects.Persistence;
with Config_Worker;
with Config_Worker_Protocol;
with Config_Worker_Storage;
with Config_Schema_Protocol;
with Config_Schema_Worker;

--  Single-owner worker-side adapter. The trusted launch/receive shell supplies
--  a held endpoint back to Config and authenticates kernel-stamped source/tag.
--  The expected grant owner is derived by the kernel from that endpoint, not
--  from the request's PID/bytes. Only authenticated Config may provision types;
--  a normal object's schema key can only select an already approved binding.
generic
   Owner_Endpoint : CuBit.Messages.CapabilitySlot;
   with function Authorized_Source
     (Sender : CuBit.Messages.ProcessID; Authority_Tag : Interfaces.Unsigned_64) return Boolean;
   with procedure Invoke
     (Action : Config_Worker_Protocol.Operation;
      Name, Context : String; Expected_Revision : Config_Worker_Protocol.Number;
      Schema : CCL.Objects.Schema_Key; Input : CCL.Objects.Persistence.Packet;
      Output : out Config_Worker_Storage.Reply);
   with procedure Invoke_Type
     (Action : Config_Schema_Protocol.Operation; Name, Context : String;
      Contract : CCL.Objects.Binding; Recovered : out CCL.Objects.Binding;
      Result : out Config_Schema_Protocol.Reply_Kind);
package Config_Worker_Receiver is
   Maximum_Schemas : constant := 16;
   type State is limited private;
   function Needs_Recovery (Object : State) return Boolean;
   --  This may block in database IO: run only in the dedicated worker, never
   --  in Config's dispatcher. No borrowed mapping is retained across Invoke.
   --  A lost response after a commit is uncertain, NOT permission to retry.
   procedure Handle
     (Object : in out State; Sender : CuBit.Messages.ProcessID;
      Request : CuBit.Messages.Message;
      Reply : out CuBit.Messages.Message);
private
   package Executor is new Config_Worker (Invoke);
   package Type_Executor is new Config_Schema_Worker (Invoke_Type);
   type Schema_Array is array (1 .. Maximum_Schemas) of CCL.Objects.Binding;
   type State is limited record
      Worker : Executor.State;
      Types : Type_Executor.State;
      Failed_Transfer : Boolean := False;
      Schemas : Schema_Array;
      Schema_Count : Natural range 0 .. Maximum_Schemas := 0;
   end record;
end Config_Worker_Receiver;
