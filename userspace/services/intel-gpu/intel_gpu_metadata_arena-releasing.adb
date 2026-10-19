package body Intel_GPU_Metadata_Arena.Releasing is
   procedure Release (Object : in out Arena; Accepted : out Boolean) is
   begin
      Accepted := Object.Data.Base = 0;
      if Accepted then
         Object.Data := (others => <>);
         return;
      end if;
      if not Unreserve (Object.Data.Base, Object.Data.Limit) then return; end if;
      Object.Data := (others => <>);
      Accepted := True;
   end Release;
end Intel_GPU_Metadata_Arena.Releasing;
