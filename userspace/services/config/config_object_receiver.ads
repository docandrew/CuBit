with System;
with CCL.Objects;
with Interfaces;
with CuBit.Messages;
with CuBit.Memory_Grants;
with Config_Authority;
with Config_Typed_Store;
with Config_Object_Dispatch;
with Config_Worker_Protocol;
with Config_Object_Messages;

--  Single-owner syscall shell. The generic operations are the native kernel
--  primitives, substituted by deterministic fault injection in hosted tests.
--  Not a SPARK proof of shared memory, kernel identity or reply-cap lifetime.
generic
   --  A reply set aside to answer later (natively CuBit.Handles.Reply).
   type Reply_Handle is private;
   with procedure Acquire
     (Reference : CuBit.Memory_Grants.Grant_Reference;
      Expected_Owner : CuBit.Messages.Process_ID;
      Byte_Offset, Byte_Length : Interfaces.Unsigned_64;
      Required_Access : CuBit.Memory_Grants.Required_Access;
      Mapped_Address : out System.Address; Success : out Boolean);
   with procedure Return_Acquisition
     (Reference : CuBit.Memory_Grants.Grant_Reference; Success : out Boolean);
   --  Set the current request's reply authority aside (Saved False: it
   --  stays with the request and can still be answered now).
   with procedure Save_Reply (Handle : out Reply_Handle; Saved : out Boolean);
   --  Answer a saved request, or the request being handled (its Sender).
   with function Send_Saved_Reply
     (Handle : Reply_Handle; Message : CuBit.Messages.Message)
      return Interfaces.Unsigned_64;
   with function Send_Current_Reply
     (Sender : CuBit.Messages.Process_ID; Message : CuBit.Messages.Message)
      return Interfaces.Unsigned_64;
   -- Native replies return 1 only after delivery, 0 on failure. They consume
   -- the one-use reply even on failure. Test substitutes must preserve this ABI.
package Config_Object_Receiver is
   use all type Config_Object_Messages.Operation;
   type State is limited private;
   function Needs_Recovery (Object : State) return Boolean;
   function Waiting (Object : State) return Boolean;
   subtype Definition_Operation is Config_Object_Messages.Operation
     with Static_Predicate => Definition_Operation in
       Config_Object_Messages.Open_Collection | Config_Object_Messages.Create_Collection;
   function Definition_Pending (Object : State) return Boolean;
   procedure Begin_Definition
     (Object : in out State; Store : in out Config_Typed_Store.State;
      Action : Definition_Operation;
      Authority : Config_Authority.Authority_State; Sender : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message; Storage_Available : Boolean; Staged : out Boolean);
   procedure Pending_Definition
     (Object : State; Control : out Config_Object_Messages.Open_Descriptor;
      Contract : out CCL.Objects.Binding);
   procedure Finish_Definition
     (Object : in out State; Store : in out Config_Typed_Store.State;
      Authority : Config_Authority.Authority_State; Code : Config_Object_Messages.Status);
   procedure Handle
     (Object : in out State; Store : in out Config_Typed_Store.State;
      Authority : Config_Authority.Authority_State;
      Sender : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Storage_Token : Interfaces.Unsigned_64; Staged : out Boolean);
   --  Called only with a request and sender returned by kernel RECEIVE, with
   --  that request's CURRENT reply authority still with this same thread.
   --  This instance holds at most one saved reply, its own, for its entire
   --  lifetime. Do not reset/copy State.
   --  Staged asks the caller to submit Store.Pending exactly once. If enqueue
   --  fails, call Lost, not Handle again. No client mapping survives staging.
   procedure Finish
     (Object : in out State; Store : in out Config_Typed_Store.State;
      Response : Config_Worker_Protocol.Frame);
   procedure Lost
     (Object : in out State; Store : in out Config_Typed_Store.State;
      Session : Interfaces.Unsigned_64);
   --  Only the owning authenticated worker channel may call Finish/Lost.
   --  Neither takes a caller PID; replies consume the saved kernel authority.
private
   type State is limited record
      Dispatch : Config_Object_Dispatch.State;
      Failed_Transfer : Boolean := False;
      Acquiring_Definition : Boolean := False;
      Definition_Action : Definition_Operation := Config_Object_Messages.Open_Collection;
      Definition_Control : Config_Object_Messages.Open_Descriptor;
      Definition_Contract : CCL.Objects.Binding;
      Caller : Config_Authority.Subject_ID := Config_Authority.No_Subject;
      Grant_Revision : Interfaces.Unsigned_64 := 0;
      --  The reply saved for a deferred Set or definition.
      Saved_Reply : Reply_Handle;
      --  The sender of the request being handled.
      Current_Sender : CuBit.Messages.Process_ID := CuBit.Messages.No_Process;
   end record;
end Config_Object_Receiver;
