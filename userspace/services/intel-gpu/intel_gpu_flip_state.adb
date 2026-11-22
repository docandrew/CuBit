package body Intel_GPU_Flip_State with SPARK_Mode is
   function First_Writable (S : State; Except : Source) return Buffer is
   begin
      for B in Buffer loop
         if B /= Except and then Writable (S, B) then
            return B;
         end if;
         pragma Loop_Invariant
           (for all C in Buffer'First .. B => C = Except or else not Writable (S, C));
      end loop;
      pragma Assert (False);
      return Buffer'First;
   end First_Writable;

   procedure Arm (S : in out State; B : Buffer; Superseded : out Boolean) is
   begin
      Superseded := S.Owed;
      if S.Armed in Buffer and then S.Armed /= S.Seen then
         S.Maybe (S.Armed) := True;
      end if;
      S.Armed := B;
      S.Owed := True;
   end Arm;

   procedure Arm_Firmware (S : in out State; Superseded : out Boolean) is
   begin
      Superseded := S.Owed;
      if S.Armed in Buffer and then S.Armed /= S.Seen then
         S.Maybe (S.Armed) := True;
      end if;
      S.Armed := Firmware;
      S.Owed := False;
   end Arm_Firmware;

   procedure Observe (S : in out State; Live : Source; Latched : out Boolean) is
   begin
      Latched := False;
      if S.Faulted or else
        (Live /= S.Seen and then Live /= S.Armed and then
         not (Live in Buffer and then S.Maybe (Live)))
      then
         S.Faulted := True;
         return;
      end if;
      Latched := S.Owed and then Live = S.Armed;
      S.Seen := Live;
      S.Maybe := [others => False];
      if Latched then
         S.Owed := False;
      end if;
   end Observe;
end Intel_GPU_Flip_State;
