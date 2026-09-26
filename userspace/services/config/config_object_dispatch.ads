with Interfaces;
with CCL.Objects;
with CuBit.Messages;
with Config_Authority;
with Config_Typed_Store;
with Config_Object_Messages;
with Config_Worker_Protocol;

--  Pure message dispatch over OWNED input/output snapshots. The thin IPC shell
--  acquires/releases grants, authenticates subjects and owns saved reply caps.
--  No caller mapping is retained while a Set waits for its storage receipt.
package Config_Object_Dispatch with SPARK_Mode is
   use type CuBit.Messages.Message;
   use type Interfaces.Unsigned_32;
   use type Config_Object_Messages.Operation;
   type State is limited private;
   type Disposition is (Reply_Now, Await_Storage);
   function Waiting (Object : State) return Boolean;
   procedure Handle
     (Object : in out State; Store : in out Config_Typed_Store.State;
      Authority : Config_Authority.Authority_State; Sender : CuBit.Messages.ProcessID;
      Action : Config_Object_Messages.Operation; Request : CuBit.Messages.Message;
      Input : Config_Object_Messages.Frame; Storage_Token : Interfaces.Unsigned_64;
      Reply_Reserved : Boolean; Reply : out CuBit.Messages.Message;
      Value : out CCL.Objects.Image; Next : out Disposition)
     with Post =>
       (if Action = Config_Object_Messages.Set_Object then
          Reply.tag.label /= Config_Object_Messages.Status'Enum_Rep (Config_Object_Messages.Success)) and then
       (if Next = Await_Storage then Reply = CuBit.Messages.NULL_MESSAGE and Waiting (Object));
   --  Reply_Reserved is trusted shell state, never a request field: the kernel
   --  has moved this client's reply authority into an EMPTY dedicated slot.
   --  Never replace an existing saved reply while Waiting=True. A rejected Set
   --  replies immediately through that saved slot; Await_Storage keeps it.
   --  Other calls use their current reply authority and leave the saved one.
   --  Await_Storage exposes NULL_MESSAGE, not an acknowledgment of success.

   --  ONLY the authenticated worker completion path calls Finish/Lost.
   --  Ready identifies a response for the one saved client reply capability;
   --  background restores and stale receipts must not consume that capability.
   procedure Finish
     (Object : in out State; Store : in out Config_Typed_Store.State;
      Response : Config_Worker_Protocol.Frame; Reply : out CuBit.Messages.Message; Ready : out Boolean)
     with Post =>
       (if Ready then not Waiting (Object) and then
          (Reply.tag.label = Config_Object_Messages.Status'Enum_Rep (Config_Object_Messages.Success) or else
           Reply = Config_Object_Messages.Reply (Config_Object_Messages.Rejected) or else
           Reply = Config_Object_Messages.Reply (Config_Object_Messages.Uncertain))
        else Reply = CuBit.Messages.NULL_MESSAGE);
   procedure Lost
     (Object : in out State; Store : in out Config_Typed_Store.State;
      Session : Interfaces.Unsigned_64; Reply : out CuBit.Messages.Message; Ready : out Boolean)
     with Post =>
       (if Ready then not Waiting (Object) and then
          Reply = Config_Object_Messages.Reply (Config_Object_Messages.Uncertain)
        else Reply = CuBit.Messages.NULL_MESSAGE);
private
   type State is limited record
      Awaiting : Boolean := False;
      Session, Token : Interfaces.Unsigned_64 := 0;
   end record;
end Config_Object_Dispatch;
