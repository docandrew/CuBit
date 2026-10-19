package body Intel_GPU_Metadata_Bundle.Releasing is
   procedure Release (Object : in out Bundle; Accepted : out Boolean) is
      Released : Boolean;
   begin
      Accepted := Object.Status in Idle | Failed;
      if not Accepted then return; end if;
      for T in Table_ID loop
         Release_Arena (Object.Items (T), Released);
         Accepted := Accepted and Released;
      end loop;
      if Accepted then
         Object.Status := Idle;
         Object.Target := 1;
         Object.Current := Table_ID'First;
         Object.Plan := [others => (others => <>)];
      end if;
   end Release;
end Intel_GPU_Metadata_Bundle.Releasing;
