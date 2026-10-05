with Ada.Text_IO;
with Client_Input_Channel_Policy;
procedure Client_Input_Channel_Policy_Tests is
   package P renames Client_Input_Channel_Policy;
   use type P.Phase; use type P.IQ.Word;
   S : P.State;
   Needed : Boolean;
   Identity : P.IQ.Word;
   Seeds : constant array (1..4) of P.IQ.Word := [0,1,P.IQ.Word'Last-1,P.IQ.Word'Last];
begin
   for OK in Boolean loop
      for Seed of Seeds loop
         S:=P.Create(Seed);
         P.Reserve(S,Identity); pragma Assert(Identity=0 and P.Mode(S)=P.Fresh);
         P.Begin_Setup(S,Needed); pragma Assert(Needed);
         P.Begin_Setup(S,Needed); pragma Assert(not Needed);
         P.Finish_Setup(S,OK);
         for I in 1..32 loop
            declare Previous : constant P.IQ.Word := P.Next_Identity(S); begin
               P.Begin_Setup(S,Needed); pragma Assert(not Needed);
               P.Reserve(S,Identity);
               if P.Mode(S)=P.Ready then
                  pragma Assert(Identity=Previous and P.Next_Identity(S)=Previous+1);
               else pragma Assert(Identity=0 and P.Mode(S)=P.Disabled);
               end if;
            end;
         end loop;
         P.Disable(S);
         for I in 1..32 loop
            P.Begin_Setup(S,Needed); pragma Assert(not Needed);
            P.Reserve(S,Identity); pragma Assert(Identity=0 and P.Mode(S)=P.Disabled);
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line("CLIENT INPUT CHANNEL: PASS one setup attempt, fallback, quarantine and nonwrapping identities");
end Client_Input_Channel_Policy_Tests;
