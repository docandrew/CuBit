package body Region_Install is
   function State (Object : Attempt) return Phase is (Object.Current);
   procedure Apply (Object : in out Attempt; Pages : Positive) is
      Added : Natural := 0;
      OK, Clean : Boolean;
   begin
      if Object.Current /= Fresh then return; end if;
      Object.Current := Quarantined;
      while Added < Pages loop
         Map_Page (Added, OK);
         if not OK then
            Clean := True;
            for P in reverse 1 .. Added loop
               Unmap_Page (P - 1, OK);
               Clean := Clean and OK;
            end loop;
            if not Clean then return; end if;
            Synchronize (OK);
            if not OK then return; end if;
            Release_Extent (OK);
            if OK then Object.Current := Released; end if;
            return;
         end if;
         Added := Added + 1;
      end loop;
      Object.Current := Mapped;
   end Apply;
end Region_Install;
