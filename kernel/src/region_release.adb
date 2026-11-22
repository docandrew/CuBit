package body Region_Release is
   function State (Object : Attempt) return Phase is (Object.Current);
   procedure Apply (Object : in out Attempt; Pages : Positive;
                    Authorized : Boolean) is
      OK, Clean : Boolean;
   begin
      if not Authorized or Object.Current /= Fresh then return; end if;
      Object.Current := Quarantined;
      Clean := True;
      for Index in 0 .. Pages - 1 loop
         Unmap_Page (Index, OK);
         Clean := Clean and OK;
      end loop;
      if not Clean then return; end if;
      Synchronize (OK);
      if not OK then return; end if;
      Release_Extent (OK);
      if OK then Object.Current := Released; end if;
   end Apply;
end Region_Release;
