package body Intel_GPU_Context_Table.Draining is
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   use type Life.Phase;
   use type Driver.Result;
   procedure Tick (Object : in out Table; Drain : in out Drain_State;
                   New_Fault : out Boolean) is
      Now : Unsigned_64;
      Status : Driver.Result;
      procedure Quarantine (I : Positive) is
      begin
         Driver.Fail (Object.Items (I));
         Drain.Items (I).Finished := True;
         New_Fault := True;
      end Quarantine;
   begin
      New_Fault := False;
      for I in 1 .. Object.Used loop
         if Object.Retired (I) and then not Drain.Items (I).Finished then
            if Object.Broken or else not Owner_Ready then
               Quarantine (I);
            elsif Driver.State (Object.Items (I)) in Life.Disabled | Life.Quarantined then
               Drain.Items (I).Finished := True;
            elsif Driver.State (Object.Items (I)) not in
              Life.Enable_Pending | Life.Enabled | Life.Disable_Pending
            then
               -- No runnable context yet. Retain registration/backing forever;
               -- do not enable a closing session just to disable it.
               Quarantine (I);
            else
               Now := Now_Us;
               if not Drain.Items (I).Started then
                  Drain.Items (I).Started := True;
                  Drain.Items (I).First := Now;
                  Drain.Items (I).Previous := Now;
               end if;
               if not Owner_Ready or else Now = Unsigned_64'Last or else
                 Now < Drain.Items (I).Previous or else
                 Now - Drain.Items (I).First >= 1_000_000 or else
                 Drain.Items (I).Polls = 100_000
               then
                  Quarantine (I);
               else
                  Drain.Items (I).Previous := Now;
                  Drain.Items (I).Polls := Drain.Items (I).Polls + 1;
                  if Driver.State (Object.Items (I)) = Life.Enabled then
                     Submit (Object, First_ID + Unsigned_32 (I - 1), Life.Disable, Status);
                     if Status not in Driver.Queued | Driver.Backpressure then
                        Quarantine (I);
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end loop;
   end Tick;
end Intel_GPU_Context_Table.Draining;
