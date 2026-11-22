package body Intel_GPU_Handoff is
   use Intel_GPU_ADLN_Inventory;
   function State (Object : Attempt) return Phase is (Object.Current);
   procedure Execute (Object : in out Attempt;
                      Vendor, Device : Interfaces.Unsigned_16;
                      Fuse : Interfaces.Unsigned_32; Status : out Result) is
      Description : constant Inventory := Decode (Vendor, Device, Fuse);
      OK, Prepared, Clean : Boolean;
   begin
      Status := Rejected;
      if Object.Current /= Fresh or else not Description.Valid then return; end if;
      Object.Current := Quarantined;
      Hold_Forcewake (OK);
      if not OK then Status := Forcewake_Failed; return; end if;
      for E in Engine loop
         if Description.Engines (E) then
            Stop_Engine (E, OK);
            if not OK then Status := Stop_Failed; return; end if;
         end if;
      end loop;
      Prepared := True;
      Status := Prepare_Failed;
      for E in Engine loop
         if Description.Engines (E) then
            Prepare_Engine (E, OK);
            if not OK then Prepared := False; exit; end if;
         end if;
      end loop;
      if Prepared then
         Reset_And_Settle (OK);
         Status := (if OK then Complete else Reset_Failed);
      end if;
      -- Every selected engine, including unvisited or failed preparation,
      -- receives cancellation. Continue after a reported cleanup failure.
      Clean := True;
      for E in Engine loop
         if Description.Engines (E) then
            Cancel_Preparation (E, OK);
            Clean := Clean and OK;
         end if;
      end loop;
      if not Clean then Status := Cleanup_Failed; end if;
      if Status = Complete then Object.Current := Reset_Held; end if;
   end Execute;
end Intel_GPU_Handoff;
