--  A scripted CCL_Llm for the Linux-hosted llm.* and agent.* tests (not
--  CuBit, not llm.svc): Ask records what it was asked; an llm.* exchange
--  then delivers the script's text in pieces, one piece per Take_Text, and
--  ends as the script says; an agent.* exchange is answered by the next
--  scripted turn.
with Llm_Script; use Llm_Script;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body CCL_Llm is
   use Interfaces;

   Exchange_Id : Unsigned_64 := 6;   --  the first exchange is 7
   Active : Boolean := False;
   Piece : Natural := 0;
   Text_Taken : Boolean := False;
   Current : Natural := 0;

   procedure Ask
     (Model, Mission, System, Tools, Conversation : String;
      Cache, Capture : Boolean;
      Max_Tokens, Deadline_Ms : Positive;
      Ceiling : Ceiling_Kind; Ceiling_Nano_Usd : Interfaces.Unsigned_64;
      Exchange : out Interfaces.Unsigned_64; Result : out Ask_Result;
      Why : out String; Why_Length : out Natural)
   is
   begin
      Asked := True;
      Asks := Asks + 1;
      Last_Model_Length := Model'Length;
      Last_Model := To_Unbounded_String (Model);
      Last_Max_Tokens := Max_Tokens;
      Last_Deadline := Deadline_Ms;
      Last_Bounded := Ceiling = Nano_Usd;
      Last_Ceiling := Ceiling_Nano_Usd;
      Last_Mission_Length := Mission'Length;
      Last_System_Length := System'Length;
      Last_System := To_Unbounded_String (System);
      Last_Tools := To_Unbounded_String (Tools);
      Last_Conversation := To_Unbounded_String (Conversation);
      Last_Cache := Cache;
      Last_Capture := Capture;
      Why := [others => ' '];
      Why_Length := 0;
      --  The same ceiling the native client and llm.svc enforce.
      if Model'Length + Mission'Length + System'Length + Tools'Length + Conversation'Length > MAXIMUM_ASK_TEXT then
         Result := Capacity;
         Exchange := 0;
         Capacity_Refusals := Capacity_Refusals + 1;
         return;
      end if;
      if Agent_Mode and then Next_Turn > Turn_Count then
         Result := Refused;
         Exchange := 0;
         return;
      end if;
      Exchange_Id := Exchange_Id + 1;
      Exchange := Exchange_Id;
      Active := True;
      Piece := 0;
      Text_Taken := False;
      if Agent_Mode then
         Current := Next_Turn;
         Next_Turn := Next_Turn + 1;
      end if;
      Result := Started;
   end Ask;

   procedure Take_Text (Exchange : Interfaces.Unsigned_64; Into : out String; Last : out Natural) is
   begin
      Into := [others => ' '];
      Last := Into'First - 1;
      if not Active or else Exchange /= Exchange_Id then
         return;
      end if;
      if Agent_Mode then
         if not Text_Taken then
            Text_Taken := True;
            declare
               Text : constant String := To_String (Turns (Current).Text);
            begin
               Into (Into'First .. Into'First + Text'Length - 1) := Text;
               Last := Into'First + Text'Length - 1;
            end;
         end if;
         return;
      end if;
      if Piece >= Released then
         return;
      end if;
      Piece := Piece + 1;
      declare
         Text : constant String := Piece_Text (Piece);
      begin
         Into (Into'First .. Into'First + Text'Length - 1) := Text;
         Last := Into'First + Text'Length - 1;
      end;
   end Take_Text;

   procedure Take_Summary
     (Exchange : Interfaces.Unsigned_64; Result : out Summary;
      Content : out Text_Access; Ended : out Boolean)
   is
   begin
      Result := (others => <>);
      Ended := False;
      Content := null;
      if not Active or else Exchange /= Exchange_Id then
         return;
      end if;
      if Agent_Mode then
         if Hold or else not Text_Taken then
            return;
         end if;
         declare
            T : Turn renames Turns (Current);
            Blocks : constant String := To_String (T.Content);
         begin
            Result.Ended_As := T.Stop;
            Result.Status := 200;
            Result.Input := T.Input;
            Result.Output := T.Output;
            Result.Cache_Read := T.Cache_Read;
            Result.Priced := True;
            Result.Cost_Nano_Usd := T.Cost;
            if Blocks'Length > 0 then
               Content := new String'(Blocks);
            end if;
         end;
         Active := False;
         Ended := True;
         return;
      end if;
      if Llm_Script.Ended and then Piece >= Released then
         Result.Ended_As := End_Turn;
         Result.Status := 200;
         Result.Input := 25;
         Result.Output := 12;
         Result.Priced := True;
         Result.Cost_Nano_Usd := 340_000;
         Result.Model.Data (1 .. 15) := "claude-opus-5-5";
         Result.Model.Length := 15;
         Result.Error.Data (1 .. 4) := "none";
         Result.Error.Length := 4;
         Result.Problem.Data (1 .. 4) := "none";
         Result.Problem.Length := 4;
         Active := False;
         Ended := True;
      end if;
   end Take_Summary;

   procedure Cancel (Exchange : Interfaces.Unsigned_64; Done : out Boolean) is
   begin
      Done := Exchange = Exchange_Id;
      if Done and then Agent_Mode then
         Turns (Current).Stop := Cancelled;
         Turns (Current).Content := Null_Unbounded_String;
         Hold := False;
      end if;
   end Cancel;

   function Following (Exchange : Interfaces.Unsigned_64) return Boolean is
     (Active and then Exchange = Exchange_Id);
end CCL_Llm;
