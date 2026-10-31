pragma Ada_2022;
with Interfaces;
package body GPIO_Native_Dispatch is
   package M renames CuBit.Messages;
   package W renames Dispatch.W;
   use type Interfaces.Unsigned_16, M.IPC_Flags;
   procedure Handle (Caller : M.Process_ID; Message : M.Message;
      Result : out Dispatch.Report) is
      Length : Natural := Natural (Message.tag.length);
      Frame_Valid : constant Boolean := Length <= Message.words'Length
         and then Message.tag.reserved = 0 and then Message.tag.ipc = M.No_IPC_Flags;
   begin
      if not Frame_Valid then Length := 0; end if;
      declare Words : W.Message_Words (1 .. Length); begin
         for Index in Words'Range loop Words (Index) := Message.words (Index - 1); end loop;
         Dispatch.Handle (Caller, Message.authorityTag, Message.tag.label,
            W.Word (Message.tag.protocol), Words, Frame_Valid, Result);
      end;
   end Handle;
end GPIO_Native_Dispatch;
