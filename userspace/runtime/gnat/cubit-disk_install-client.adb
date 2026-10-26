------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles.IPC;

package body CuBit.Disk_Install.Client is
   function Open
     (Service : CuBit.Handles.Endpoint; Item : Open_Request) return Opened_Disk
   is
      package IPC renames CuBit.Handles.IPC;
      Encoded : constant Request_Words := Encode (Item);
      Msg : Message := NULL_MESSAGE;
      Received : IPC.Carried;
      Reply : MessageTag;
   begin
      Msg.tag := (label => Open_Disk, length => Open_Words, protocol => 0,
                  ipc => <>, reserved => 0);
      Msg.words (Ordinal_Word) := Encoded (Ordinal_Word);
      Msg.words (Access_Word) := Encoded (Access_Word);
      Reply := IPC.Call_Accepting (Service, Msg, Wait_Forever, Received);
      if Reply.label /= Reply_OK then
         return (Opened => False);
      end if;
      declare
         Taken : constant IPC.Accepted_Endpoint := IPC.Accept_Endpoint (Received);
      begin
         if Taken.OK then
            return (Opened => True, Device => Taken.Value);
         end if;
      end;
      return (Opened => False);
   end Open;
end CuBit.Disk_Install.Client;
