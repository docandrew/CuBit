package body Client_Input_Channel_Policy with SPARK_Mode is
   procedure Begin_Setup (S : in out State; Needed : out Boolean) is
   begin
      Needed := S.Current = Fresh;
      if Needed then S.Current := Preparing; end if;
   end Begin_Setup;
   procedure Finish_Setup (S : in out State; OK : Boolean) is
   begin S.Current := (if OK then Ready else Disabled); end Finish_Setup;
   procedure Disable (S : in out State) is
   begin S.Current := Disabled; end Disable;
   procedure Reserve (S : in out State; Identity : out IQ.Word) is
   begin
      Identity := 0;
      if S.Current /= Ready then return; end if;
      IQ.Reserve (S.Next, Identity);
      if Identity = 0 then S.Current := Disabled; end if;
   end Reserve;
end Client_Input_Channel_Policy;
