with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with Desktop_Endpoints;

package body Desktop_Shutdown is
   package PR renames CuBit.Power_Requests;
   use type PR.Answer;

   State : Phase := Closed;
   Next_Id : Unsigned_64 := 1;
   Last_Note : String (1 .. 80) := [others => ' '];
   Last_Length : Natural := 0;

   function Current return Phase is (State);
   function Note return String is (Last_Note (1 .. Last_Length));

   procedure Say (Text : String) is
   begin
      Last_Length := Natural'Min (Text'Length, Last_Note'Length);
      Last_Note (1 .. Last_Length) := Text (Text'First .. Text'First + Last_Length - 1);
   end Say;

   procedure Ask is
   begin
      if State in Closed | Refused then
         State := Confirming;
         debugPrint ("desktop: shut down: confirmation shown" & ASCII.LF);
      end if;
   end Ask;

   procedure Cancel is
   begin
      if State = Confirming then
         State := Closed;
         debugPrint ("desktop: shut down: cancelled; nothing sent" & ASCII.LF);
      end if;
   end Cancel;

   procedure Dismiss is
   begin
      if State = Refused then
         State := Closed;
      end if;
   end Dismiss;

   procedure Confirm is
      Msg : Message := NULL_MESSAGE;
      Tag : MessageTag;
      Id : constant Unsigned_64 := Next_Id;
   begin
      if State /= Confirming then
         return;
      end if;
      if not CuBit.Handles.Valid (Desktop_Endpoints.Power_Service) then
         State := Refused;
         Say ("power.svc is not running here");
         debugPrint ("desktop: shut down: no power.svc endpoint" & ASCII.LF);
         return;
      end if;
      Next_Id := Next_Id + 1;
      Msg.tag := (label => PR.Request_Label, length => PR.Request_Words, protocol => 0, ipc => <>, reserved => 0);
      Msg.words (PR.Request_Word) := Id;
      Msg.words (PR.Action_Word) := PR.Action'Enum_Rep (PR.Power_Off);
      Tag := capCall (Desktop_Endpoints.Power_Service, Msg, Deadline_After (PR.Answer_Budget_Milliseconds));
      if Ending_Of (Tag) = Call_Answered and then Tag.label = PR.Request_Label
        and then Msg.words (PR.Reply_Request_Word) = Id and then PR.Valid_Answer (Msg.words (PR.Answer_Word))
      then
         declare
            A : constant PR.Answer := PR.To_Answer (Msg.words (PR.Answer_Word));
         begin
            debugPrint ("desktop: shut down: power.svc answered " & PR.Name (A) & ASCII.LF);
            if A = PR.Accepted then
               State := Shutting_Down;
            else
               State := Refused;
               Say ("Shut down refused: " & PR.Name (A));
            end if;
         end;
      else
         State := Refused;
         Say ("power.svc did not answer");
         debugPrint ("desktop: shut down: power.svc did not answer" & ASCII.LF);
      end if;
   end Confirm;
end Desktop_Shutdown;
