pragma Ada_2022;
with Bridge_Trace; use Bridge_Trace;
package body CuBit.Handles.IPC is
 function Call(Target : Endpoint; Msg : in out CuBit.Messages.Message;
   Deadline : Unsigned_64) return CuBit.Messages.MessageTag is
  Tag : CuBit.Messages.MessageTag := (label=>16#F000#,others=><>);
 begin
  if not Valid(Target) or else Deadline=Unsigned_64'Last or else Deadline<=Now then raise Program_Error; end if;
  Note(IPC_Call); Calls:=Calls+1; Last_Message:=Msg; Last_Deadline:=Deadline;
  if Timeout then Tag.label:=CuBit.Messages.REPLY_TIMEOUT;
  elsif Kernel_Error then Tag.label:=CuBit.Messages.Kernel_Reply_First;
  end if;
  -- Deliberately do not update Msg.tag; adapter must use the returned tag.
  Msg.words:=[Msg.words(0)+1,0,0,0]; return Tag;
 end Call;
end CuBit.Handles.IPC;
