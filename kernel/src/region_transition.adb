package body Region_Transition is
   function State (Object : Region) return Phase is (Object.Status);
   function Current (Object : Region) return Permission is (Object.Mode);
   procedure Change (Object : in out Region; Target : Permission;
                     Authorized : Boolean; Success : out Boolean) is
      OK : Boolean;
   begin
      Success := False;
      if not Authorized or Object.Status /= Stable then return; end if;
      if Target = Object.Mode then Success := True; return; end if;
      -- Latch before callbacks: exceptions cannot leave a reusable state.
      Object.Status := Quarantined;
      Revoke (OK);
      if not OK then return; end if;
      Invalidate (OK);
      if not OK then return; end if;
      -- Only after old translations are gone may opposite access be installed.
      Install (Target = Executable, OK);
      if not OK then return; end if;
      Invalidate (OK);
      if not OK then return; end if;
      Object.Mode := Target;
      Object.Status := Stable;
      Success := True;
   end Change;
end Region_Transition;
