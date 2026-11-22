pragma Ada_2022;
with CuBit.Messages;
with CuBit.Driver_Readiness;
package body HID_Native_Readiness is
   package M renames CuBit.Messages;
   use type M.MessageTag, HID_Startup_Readiness.Choice;
   procedure Send (Item : HID_Startup_Readiness.Outgoing;
      Deadline : HID_Startup_Readiness.W.Millisecond; Acknowledged : out Boolean) is
      Message : M.Message := M.NULL_MESSAGE;
      Reply : M.MessageTag;
   begin
      Message.tag.label := (if Item = HID_Startup_Readiness.Report_Ready then
         CuBit.Driver_Readiness.Ready_Label else CuBit.Driver_Readiness.Not_Present_Label);
      Reply := M.capSend (Target, Message, Deadline);
      Acknowledged := Reply = M.NULL_TAG;
   end Send;
   procedure Decline_Without_Clock (Queued : out Boolean) is
      Message : M.Message := M.NULL_MESSAGE;
   begin
      Message.tag.label := CuBit.Driver_Readiness.Not_Present_Label;
      Queued := M.capSubmit (Target, Message, M.NO_COMPLETION_TOKEN);
   end Decline_Without_Clock;
end HID_Native_Readiness;
