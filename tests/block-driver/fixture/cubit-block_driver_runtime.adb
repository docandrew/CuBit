--  Test fixture body: requests and the clock come from Block_Script.
with CuBit.Handles.Block_Fixture;
with Block_Script; use Block_Script;
package body CuBit.Block_Driver_Runtime is
   procedure Next_Request
     (Sender : out Process_ID; Request : out Message; Found : out Boolean) is
   begin
      Found := Waiting > 0;
      if Found then
         Taken := Taken + 1;
         Sender := Queue (Taken).Sender;
         Request := Queue (Taken).Request;
      else
         Sender := 0;
         Request := NULL_MESSAGE;
      end if;
   end Next_Request;

   procedure Wait_Request (Sender : out Process_ID; Request : out Message) is
      Found : Boolean;
   begin
      Next_Request (Sender, Request, Found);
      pragma Assert (Found, "Wait_Request would block forever");
   end Wait_Request;

   procedure Keep_Reply (Saved : out CuBit.Handles.Reply; Kept : out Boolean) is
   begin
      Kept := not Refuse_Keep;
      Saved := (if Kept then CuBit.Handles.Block_Fixture.Reply_Of
                               (Unsigned_64 (Taken))
                else CuBit.Handles.No_Reply);
      if Kept then
         Kept_Replies := Kept_Replies + 1;
      end if;
   end Keep_Reply;

   procedure Answer (Saved : CuBit.Handles.Reply; Response : Message) is
   begin
      pragma Assert (CuBit.Handles."/=" (Saved, CuBit.Handles.No_Reply));
      Answered := Answered + 1;
      Answers (Answered) :=
        (Saved => True,
         Reply_Word => CuBit.Handles.Block_Fixture.Word_Of (Saved),
         To => 0, Response => Response);
   end Answer;

   procedure Respond (To : Process_ID; Response : Message) is
   begin
      Answered := Answered + 1;
      Answers (Answered) :=
        (Saved => False, Reply_Word => 0, To => To, Response => Response);
   end Respond;

   function Now return Unsigned_64 is (Clock);

   function Within (Since, Micros : Unsigned_64) return Boolean is
     (Clock - Since < Micros);
end CuBit.Block_Driver_Runtime;
