package body Compositor_Lease_Request with SPARK_Mode is
   procedure Prepare (S : in out State; New_Token : ID; Prepared : out Boolean) is
   begin
      Prepared := S.Stage = Ready and New_Token > S.Last and New_Token < ID'Last;
      if Prepared then S := (Submitting, New_Token); end if;
   end Prepare;
   procedure Submitted (S : in out State; Accepted : Boolean) is
   begin
      S.Stage := (if Accepted then Pending else Ready);
   end Submitted;
   procedure Complete (S : in out State; Reply_Token : ID; Confirmed : Boolean) is
   begin
      if S.Stage = Pending and Reply_Token = S.Last and Confirmed then S.Stage := Released;
      else S.Stage := Quarantined; end if;
   end Complete;
   procedure Quarantine (S : in out State) is
   begin
      S.Stage := Quarantined;
   end Quarantine;
end Compositor_Lease_Request;
