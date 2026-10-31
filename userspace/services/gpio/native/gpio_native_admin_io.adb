pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with GPIO_Admin_Wire;
package body GPIO_Native_Admin_IO is
   package IPC renames CuBit.Handles.IPC;
   package P renames CuBit.Process_IDs;
   package W renames GPIO_Control_Wire;
   package M renames CuBit.Messages;
   use type IPC.Carried_Kind, CuBit.Handles.ABI.Kind, P.Process_ID, W.Operation, W.Word;
   use type GPIO_Native_Transport.Resolution_Status;
   function Authorized (Caller : P.Process_ID) return Boolean is
      D : constant IPC.Description := IPC.Describe (Launcher);
   begin
      return P.Is_Process (Caller) and then D.Current and then D.Kind = CuBit.Handles.ABI.Process
         and then Caller = P.From_Word (D.Object);
   end Authorized;
   procedure Take_Offer (Caller : P.Process_ID; Receipt : out Offer_Receipt; Owned : out Boolean) is
   begin Receipt := IPC.Accept_Offered (Caller); Owned := Receipt.Kind = IPC.Present; end Take_Offer;
   procedure Inspect (Receipt : Offer_Receipt; Sink : out GPIO_Native_Transport.Event_Sink;
      Owner : out P.Process_ID; Badge : out W.Word; Valid : out Boolean) is
      Accepted : constant IPC.Accepted_Endpoint := IPC.Accept_Endpoint (Receipt);
   begin
      Sink := GPIO_Native_Transport.No_Event_Sink; Owner := P.No_Process; Badge := 0; Valid := False;
      if not Accepted.OK then return; end if;
      declare D : constant GPIO_Native_Transport.Resolution := GPIO_Native_Transport.Resolve (Accepted.Value); begin
         if D.Status = GPIO_Native_Transport.Resolved then
            Sink := D.Sink; Owner := D.Owner; Badge := D.Badge; Valid := True;
         end if;
      end;
   end Inspect;
   procedure Discard (Receipt : Offer_Receipt; Closed : out Boolean) is
   begin
      Closed := False; if Receipt.Kind /= IPC.Present then return; end if;
      declare Result : constant IPC.Close_Result := IPC.Close (Receipt); begin Closed := Result.Closed; end;
   end Discard;
   function Message_For (Label : W.Operation; Status : W.Completion_Status) return M.Message is
      Words : constant W.Message_Words := GPIO_Admin_Wire.Encode_Reply (Status);
      Message : M.Message := M.NULL_MESSAGE;
   begin
      Message.tag.label := (if Label < M.Kernel_Reply_First then Label else GPIO_Admin_Wire.OP_GPIO_ADMIT_SINK);
      Message.tag.protocol := M.Protocol_Flags (GPIO_Admin_Wire.Protocol_Version);
      Message.tag.length := Interfaces.Unsigned_8 (Words'Length);
      for Index in Words'Range loop Message.words (Index) := Words (Index); end loop;
      return Message;
   end Message_For;
   procedure Answer_Saved (Reply : CuBit.Handles.Reply; Label : W.Operation;
      Status : W.Completion_Status; Delivered : out Boolean) is
   begin Delivered := IPC.Answer (Reply, Message_For (Label, Status)); end Answer_Saved;
   procedure Answer_Current (Caller : P.Process_ID; Label : W.Operation;
      Status : W.Completion_Status; Delivered : out Boolean) is
   begin Delivered := M.reply (Caller, Message_For (Label, Status)) /= 0; end Answer_Current;
end GPIO_Native_Admin_IO;
