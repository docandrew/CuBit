pragma Ada_2022;
with Interfaces;
with CuBit.Handles.IPC;
with CuBit.Messages;
package body GPIO_Native_Transport is
   package W renames GPIO_Control_Wire;
   package M renames CuBit.Messages;
   package P renames CuBit.Process_IDs;
   use type CuBit.Handles.ABI.Kind, CuBit.Handles.Rights;
   function Resolve (Target : CuBit.Handles.Endpoint) return Resolution is
      D : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Target);
      Destination : P.Process_ID;
   begin
      if not D.Current then return (Endpoint_Refused, D.Why); end if;
      if D.Kind /= CuBit.Handles.ABI.Endpoint then return (Status => Wrong_Kind); end if;
      if D.Held /= CuBit.Handles.Write_Only then return (Status => Wrong_Sink_Rights); end if;
      Destination := P.From_Word (D.Object);
      if not P.Is_Process (Destination) then return (Status => Invalid_Identity); end if;
      return (Resolved, (Target, Destination), Destination, D.Authority_Tag);
   end Resolve;
   procedure Answer (Authority : CuBit.Handles.Reply;
      Value : W.Completion; Delivered : out Boolean) is
      Data : constant W.Message_Words := W.Encode_Completion (Value);
      Message : M.Message := M.NULL_MESSAGE;
   begin
      Message.tag.label := W.Label_Of (Value.Kind);
      Message.tag.protocol := M.Protocol_Flags (W.Protocol_Version);
      Message.tag.length := Interfaces.Unsigned_8 (Data'Length);
      for Index in Data'Range loop Message.words (Index) := Data (Index); end loop;
      Delivered := CuBit.Handles.IPC.Answer (Authority, Message);
   end Answer;
   procedure Try_Event (Sink : Event_Sink; Value : W.Event; Accepted : out Boolean) is
      Data : constant W.Message_Words := W.Encode_Event (Value);
      Message : M.Message := M.NULL_MESSAGE;
   begin
      Accepted := False;
      if not CuBit.Handles.Valid (Sink.Authority) or else not P.Is_Process (Sink.Destination) then return; end if;
      Message.tag.label := W.OP_GPIO_EVENT;
      Message.tag.protocol := M.Protocol_Flags (W.Protocol_Version);
      Message.tag.length := Interfaces.Unsigned_8 (Data'Length);
      for Index in Data'Range loop Message.words (Index) := Data (Index); end loop;
      Accepted := M.trySendEvent (Sink.Destination, Message, Sink.Authority);
   end Try_Event;
   procedure Close (Sink : Event_Sink; Closed : out Boolean) is
      Result : constant CuBit.Handles.IPC.Close_Result := CuBit.Handles.IPC.Close (Sink.Authority);
   begin Closed := Result.Closed; end Close;
end GPIO_Native_Transport;
