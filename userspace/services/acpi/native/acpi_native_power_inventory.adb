pragma Ada_2022;
with CuBit.Handles.IPC;
package body ACPI_Native_Power_Inventory is
   package P renames ACPI_Power_Watch_Protocol;
   package M renames CuBit.Messages;package H renames CuBit.Handles;package IPC renames H.IPC;
   use type Interfaces.Unsigned_32, Policy.Delivery;
   Item : Policy.State;
   procedure Encode (ID : Interfaces.Unsigned_64; Status : P.Status; Generation : Interfaces.Unsigned_64; Reply : out M.Message) is
   begin Reply:=M.NULL_MESSAGE;Reply.tag.label:=P.Watch;Reply.tag.length:=4;
      Reply.words:=[ID,Interfaces.Unsigned_64(P.Status'Enum_Rep(Status)),Generation,P.Schema];end Encode;
   procedure Save_Current (Token : out H.Reply; Saved : out Boolean) is
      R : constant IPC.Saved_Reply:=IPC.Save_Reply;
   begin Saved:=R.Saved;Token:=(if R.Saved then R.Handle else H.No_Reply);end Save_Current;
   procedure Answer_Saved (Token : in out H.Reply; ID : Interfaces.Unsigned_64;
      Status : P.Status; Generation : Interfaces.Unsigned_64; Delivered : out Boolean) is Reply : M.Message;
   begin Encode(ID,Status,Generation,Reply);Delivered:=IPC.Answer(Token,Reply);Token:=H.No_Reply;end Answer_Saved;
   procedure Changed is begin Policy.Changed(Item);end Changed;
   procedure Begin_Stop is begin Policy.Begin_Stop(Item);end Begin_Stop;
   function Quiet return Boolean is (Policy.Quiet(Item));
   function Pending return Boolean is (Policy.Runnable(Item));
   procedure Advance (Owner_Quiet : Boolean; Did_Work : out Boolean) is R:Policy.Delivery;
   begin Policy.Advance(Item,Owner_Quiet,R);Did_Work:=R/=Policy.No_Attempt;end Advance;
   procedure Dispatch (From : M.Process_ID; Request : M.Message; Owner_Quiet : Boolean;
      Reply : out M.Message; Handled,Reply_Now : out Boolean) is
      R:Policy.Response;Deferred:Boolean;Previous:Policy.Delivery;
   begin
      Reply:=M.NULL_MESSAGE;Reply_Now:=True;Handled:=Request.tag.label=P.Watch;if not Handled then return;end if;
      Policy.Watch(Item,(Request.authorityTag,M.To_Word(From),Interfaces.Unsigned_64(Request.tag.length),
        Request.words(0),Request.words(1),Request.words(2),Request.words(3),Interfaces.Unsigned_64(M.Flag_Byte(Request.tag)),
        Interfaces.Unsigned_64(Request.tag.reserved)),Owner_Quiet,R,Deferred,Previous);
      Reply_Now:=not Deferred;if Reply_Now then Encode(R.ID,R.Status,R.Generation,Reply);end if;
   end Dispatch;
end ACPI_Native_Power_Inventory;
