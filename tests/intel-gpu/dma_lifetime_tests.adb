with Intel_GPU_DMA_Lifetime; use Intel_GPU_DMA_Lifetime;
procedure DMA_Lifetime_Tests is
   --  Enumerate every event sequence up to length eight, checking a separate
   --  history oracle rather than only repeating the implementation's table.
   procedure Explore
     (Current : State; Depth : Natural; Exposed, Stopped, Unmapped : Boolean)
   is
   begin
      if Exposed and Current = Reclaimable then
         pragma Assert (Stopped and Unmapped);
      end if;
      if Depth = 0 then return; end if;
      for Action in Event loop
         declare
            After : constant State := Next (Current, Action);
            E : constant Boolean := Exposed or
              (Current = Private_Buffer and Action = Publish);
            S : constant Boolean := Stopped or
              (Current = Draining and Action = Stop_Confirmed);
            U : constant Boolean := Unmapped or
              (Current = GPU_Stopped and Action = Mapping_Revoked);
         begin
            if Current = Quarantined then pragma Assert (After = Quarantined); end if;
            if Current = Reclaimable then pragma Assert (After = Reclaimable); end if;
            if Action = Owner_Lost and Current in GPU_Reachable | Draining | GPU_Stopped then
               pragma Assert (After = Quarantined);
            end if;
            Explore (After, Depth - 1, E, S, U);
         end;
      end loop;
   end Explore;
begin
   pragma Assert (Next (Private_Buffer, Retire) = Reclaimable);
   pragma Assert
     (Next (Next (Next (Next (Private_Buffer, Publish), Retire),
                  Stop_Confirmed), Mapping_Revoked) = Reclaimable);
   Explore (Private_Buffer, 8, False, False, False);
end DMA_Lifetime_Tests;
