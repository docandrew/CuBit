package body Compositor_Readers with SPARK_Mode is
   procedure Retire (S : in out State) is
      Confirmed : Boolean;
   begin
      if S.Lease then
         Release_Lease (Confirmed);
         if not Confirmed then S.Failed := True; return; end if;
         S.Lease := False;
      end if;
      for I in Target_Index loop
         if S.Grants (I) then
            Retire_Grant (I, Confirmed);
            if not Confirmed then S.Failed := True; return; end if;
            S.Grants (I) := False;
         end if;
         pragma Loop_Invariant (not S.Lease and not S.Failed);
         pragma Loop_Invariant
           (for all J in Target_Index => (if J <= I then not S.Grants (J)));
         pragma Loop_Invariant
           (for all J in Target_Index =>
              (if not S.Grants'Loop_Entry (J) then not S.Grants (J)));
      end loop;
   end Retire;
end Compositor_Readers;
