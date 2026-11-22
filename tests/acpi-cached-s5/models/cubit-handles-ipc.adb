pragma Ada_2022;
with S5_Boundary; with CuBit.Power_Event_Exchange;
package body CuBit.Handles.IPC is
   function Save_Reply return Saved_Reply is
   begin
      S5_Boundary.Saves := S5_Boundary.Saves + 1;
      return (S5_Boundary.Saved_Allowed, (if S5_Boundary.Saved_Allowed then 1 else No_Reply));
   end Save_Reply;
   function Answer (Token : Reply; Message : CuBit.Messages.Message) return Boolean is
   begin
      if Token = No_Reply then raise Program_Error; end if;
      S5_Boundary.Answers := S5_Boundary.Answers + 1;
      S5_Boundary.Reply_Status := Message.words (CuBit.Power_Event_Exchange.Enter_Status_Word);
      return S5_Boundary.Delivered;
   end Answer;
end CuBit.Handles.IPC;
