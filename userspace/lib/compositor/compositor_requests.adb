package body Compositor_Requests with SPARK_Mode is
   procedure Allocate (Sequence : in out ID; Token : out ID) is
   begin
      Token := 0;
      if Sequence < ID'Last - 1 then
         Sequence := Sequence + 1;
         Token := Sequence;
      end if;
   end Allocate;
   procedure Begin_Request (S : in out State; New_Token : ID; Accepted : out Boolean) is
   begin
      Accepted := Available (S) and New_Token > S.Last and New_Token < ID'Last;
      if Accepted then S := (Pending, New_Token); end if;
   end Begin_Request;
   procedure Quarantine (S : in out State) is
   begin
      S.Status := Uncertain;
   end Quarantine;
   procedure Complete (S : in out State; Reply_Token : ID; Confirmed : Boolean) is
   begin
      if Busy (S) and Reply_Token = S.Last and Confirmed then S.Status := Idle;
      else Quarantine (S);
      end if;
   end Complete;
end Compositor_Requests;
