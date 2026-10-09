package body Observatory_Query_Lifetime with SPARK_Mode is
   package CR renames Compositor_Requests;
   procedure Start (S : in out State; New_Token, Now : Unsigned_64; Accepted : out Boolean; Budget_Us : Timeout_Budget := Timeout_Us) is
   begin
      Accepted := False;
      if S.Mode /= Idle then return; end if;
      CR.Begin_Request (S.Flight, New_Token, Accepted);
      if Accepted then
         S.Mode := Waiting;
         S.Due := (if Now > Unsigned_64'Last - Budget_Us then Unsigned_64'Last else Now + Budget_Us);
      end if;
   end Start;
   procedure Fail (S : in out State) is
   begin
      CR.Quarantine (S.Flight); S.Mode := Failed;
   end Fail;
   procedure Receive (S : in out State; Reply_Token : Unsigned_64; Valid : Boolean) is
   begin
      if S.Mode /= Waiting or else Reply_Token /= Token (S) then return; end if;
      if Valid then S.Mode := Retiring; else Fail (S); end if;
   end Receive;
   procedure Retired (S : in out State; Confirmed : Boolean) is
   begin
      if S.Mode = Retiring and then Confirmed then
         CR.Complete (S.Flight, Token (S), True);
         S.Mode := Ready;
      end if;
   end Retired;
   procedure Expire (S : in out State; Now : Unsigned_64) is
   begin
      if S.Mode in Waiting | Retiring and then Now >= S.Due then Fail (S); end if;
   end Expire;
   procedure Consume (S : in out State) is
   begin
      if S.Mode = Ready then S.Mode := Idle; end if;
   end Consume;
end Observatory_Query_Lifetime;
