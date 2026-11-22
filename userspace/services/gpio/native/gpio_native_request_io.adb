pragma Ada_2022;
with Interfaces;
with CuBit.Handles.IPC;
with CuBit.Messages;
package body GPIO_Native_Request_IO is
   package W renames GPIO_Control_Wire;
   package M renames CuBit.Messages;
   package IPC renames CuBit.Handles.IPC;
   use type W.Operation, Interfaces.Unsigned_64;
   function Message_For (Label : W.Operation; Value : W.Completion) return M.Message is
      Words : constant W.Message_Words := W.Encode_Completion (Value);
      Message : M.Message := M.NULL_MESSAGE;
   begin
      -- Never originate a reserved kernel reply label, even for an invalid
      -- request. Otherwise echo the request's label, including unknown ops.
      Message.tag.label := (if Label < M.Kernel_Reply_First then Label else W.OP_GPIO_ARM);
      Message.tag.protocol := M.Protocol_Flags (W.Protocol_Version);
      Message.tag.length := Interfaces.Unsigned_8 (Words'Length);
      for Index in Words'Range loop Message.words (Index) := Words (Index); end loop;
      return Message;
   end Message_For;
   procedure Save (Reply : out CuBit.Handles.Reply; Saved : out Boolean) is
      Result : constant IPC.Saved_Reply := IPC.Save_Reply;
   begin
      Saved := Result.Saved; Reply := CuBit.Handles.No_Reply;
      if Result.Saved then Reply := Result.Handle; end if;
   end Save;
   procedure Answer_Saved (Reply : CuBit.Handles.Reply;
      Label : W.Operation; Value : W.Completion; Delivered : out Boolean) is
   begin Delivered := IPC.Answer (Reply, Message_For (Label, Value)); end Answer_Saved;
   procedure Answer_Current (Caller : CuBit.Process_IDs.Process_ID;
      Label : W.Operation; Value : W.Completion; Delivered : out Boolean) is
   begin Delivered := M.reply (Caller, Message_For (Label, Value)) /= 0; end Answer_Current;
end GPIO_Native_Request_IO;
