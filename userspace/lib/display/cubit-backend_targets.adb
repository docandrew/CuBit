package body CuBit.Backend_Targets with SPARK_Mode is
   procedure Set_Count (S : in out State; N : Buffer_Count) is
   begin
      if S.Stage = Unconfigured then S.Buffers := N; end if;
   end Set_Count;
   procedure Cleared (S : in out State) is
   begin
      if S.Stage in Unconfigured | Idle then S := (Idle, 0, 0, S.Buffers);
      else S.Stage := Failed; end if;
   end Cleared;
   procedure Prepare (S : in out State; Accepted : out Boolean) is
   begin
      Accepted := S.Stage = Idle;
      if Accepted then S.Stage := Preparing; end if;
   end Prepare;
   procedure Seal (S : in out State; ID : Identifier) is
   begin
      if S.Stage = Preparing and ID /= 0 then S.Stage := In_Flight; S.Pending := ID;
      else S.Stage := Failed; end if;
   end Seal;
   procedure Complete (S : in out State; ID : Identifier; Published : Boolean) is
   begin
      if S.Stage = In_Flight and ID = S.Pending and Published then
         S := (Idle, (S.Front + 1) mod S.Buffers, 0, S.Buffers);
      else S.Stage := Failed; end if;
   end Complete;
   procedure Quarantine (S : in out State) is
   begin S.Stage := Failed; end Quarantine;
end CuBit.Backend_Targets;
