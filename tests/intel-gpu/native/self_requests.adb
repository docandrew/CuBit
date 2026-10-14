with CuBit.Messages; use CuBit.Messages;
with CuBit.Process_IDs;
with CuBit.Handles.IPC;
with Startup;

package body Self_Requests is
   package IPC renames CuBit.Handles.IPC;

   function Open (Label : Unsigned_32; Deadline_Ms : Unsigned_64)
     return CuBit.Handles.Reply
   is
      Msg : Message := NULL_MESSAGE;
      From : Process_ID;
      Found : Boolean;
      Until_Time : Unsigned_64;
   begin
      Msg.tag := (label => Label, length => 0, protocol => 0, ipc => <>, reserved => 0);
      if not capSubmit (Startup.Device_Manager, Msg, Unsigned_64 (Label)) then
         return CuBit.Handles.No_Reply;
      end if;
      Until_Time := syscall (SYSCALL_GETTIME) + Deadline_Ms;
      loop
         Poll_Service_Request (From, Msg, Found);
         exit when Found;
         if syscall (SYSCALL_GETTIME) >= Until_Time then
            return CuBit.Handles.No_Reply;
         end if;
      end loop;
      if Msg.tag.label /= Label or else
        CuBit.Process_IDs.To_Word (From) /= syscall (SYSCALL_GETPID)
      then
         return CuBit.Handles.No_Reply;
      end if;
      declare
         Saved : constant IPC.Saved_Reply := IPC.Save_Reply;
      begin
         return (if Saved.Saved then Saved.Handle else CuBit.Handles.No_Reply);
      end;
   end Open;

   function Close (Item : CuBit.Handles.Reply; Label : Unsigned_32;
                   Deadline_Ms : Unsigned_64) return Boolean
   is
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Until_Time : Unsigned_64;
   begin
      Msg.tag := (label => Label, length => 0, protocol => 0, ipc => <>, reserved => 0);
      if not IPC.Answer (Item, Msg) then
         return False;
      end if;
      Until_Time := syscall (SYSCALL_GETTIME) + Deadline_Ms;
      loop
         if Poll_Completion (Receipt'Address) /= 0 then
            return Receipt.token = Unsigned_64 (Label)
              and then Receipt.status = COMPLETION_OK;
         end if;
         if syscall (SYSCALL_GETTIME) >= Until_Time then
            return False;
         end if;
      end loop;
   end Close;
end Self_Requests;
