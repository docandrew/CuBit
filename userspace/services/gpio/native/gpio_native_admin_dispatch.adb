pragma Ada_2022;
with Interfaces;
package body GPIO_Native_Admin_Dispatch is
   package M renames CuBit.Messages;
   package W renames Admin.W;
   use type Interfaces.Unsigned_16, M.IPC_Flags;
   procedure Handle (S : in out Admin.State; Caller : M.Process_ID;
      Message : M.Message; Setup_Deadline, Cleanup_Deadline : Admin.I.Tick) is
      Length : Natural := Natural (Message.tag.length);
      Has_Offer : constant Boolean := CuBit.Handles.IPC.Offers (Message);
      Allowed : M.IPC_Flags := M.No_IPC_Flags;
      Frame_Valid : Boolean;
   begin
      Allowed.Capability_Offered := Has_Offer;
      Frame_Valid := Length <= Message.words'Length and then Message.tag.reserved = 0 and then Message.tag.ipc = Allowed;
      if not Frame_Valid then Length := 0; end if;
      declare Words : W.Message_Words (1 .. Length); begin
         for Index in Words'Range loop Words (Index) := Message.words (Index - 1); end loop;
         Admin.Handle (S, Caller, Message.tag.label, W.Word (Message.tag.protocol), Words,
            Frame_Valid, Has_Offer, Setup_Deadline, Cleanup_Deadline);
      end;
   end Handle;
end GPIO_Native_Admin_Dispatch;
