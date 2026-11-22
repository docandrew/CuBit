package body CuBit.Display_Pool_Registry with SPARK_Mode is
   procedure Register (S : in out State; A : P.Attachment; Accepted : out Boolean) is
   begin
      Accepted := Admits (S, A);
      if Accepted then S.Sources (A.Buffer) := (True, A.Source); end if;
   end Register;
   procedure Open (S : in out State; Accepted : out Boolean) is
   begin
      Accepted := Count (S) = 3 and not S.Live and not S.Failed;
      if Accepted then S.Live := True; end if;
   end Open;
   procedure Quarantine (S : in out State) is
   begin S.Failed := True; end Quarantine;
end CuBit.Display_Pool_Registry;
