pragma Ada_2022;

package body Wake_Handshake_Model with SPARK_Mode is

   procedure Step (S : in out State; A : Action) is
   begin
      case A is
         when Produce =>
            S.Produced := S.Produced + 1;
            S.Producer := Published;
         when Check_And_Wake =>
            if S.Needs_Wake then
               S.Wake_Pending := True;
            end if;
            S.Producer := Idle;
         when Arm =>
            S.Needs_Wake := True;
            S.Consumer := Armed;
         when Recheck =>
            if S.Produced > S.Consumed then
               S.Needs_Wake := False;
               S.Consumer := Running;
            else
               S.Consumer := Asleep;
            end if;
         when Wake_Up =>
            S.Wake_Pending := False;
            S.Needs_Wake := False;
            S.Consumer := Running;
         when Consume =>
            S.Consumed := S.Consumed + 1;
      end case;
   end Step;

end Wake_Handshake_Model;
