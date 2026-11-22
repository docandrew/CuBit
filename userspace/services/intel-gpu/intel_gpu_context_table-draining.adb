package body Intel_GPU_Context_Table.Draining is
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   use type Life.Phase;
   use type Driver.Result;
   -- One retired context's whole drain (disable, then deregister): the
   -- deadline and the poll budget that also bounds a frozen clock.
   Drain_Deadline_Us : constant := 1_000_000;
   Drain_Poll_Limit : constant := 100_000;
   function Observe (Object : Table; ID : Unsigned_32; Session : Unsigned_64)
      return Retirement_State is
   begin
      if Session = 0 or else Object.Broken or else not Owner_Ready then
         return Uncertain;
      end if;
      if not Known (Object, ID) or else Object.Items (Index (ID)).Owner /= Session then
         return No_Context;
      end if;
      declare Item : Context_Record renames Object.Items (Index (ID)); begin
         if not Item.Retired then return Admission_Open; end if;
         case Driver.State (Item.Item) is
            when Life.Disabled => return Disabled;
            when Life.Deregistered => return Deregistered;
            when Life.Enable_Pending | Life.Enabled | Life.Disable_Pending |
                 Life.Deregister_Pending =>
               return Pending;
            when others => return Uncertain;
         end case;
      end;
   end Observe;
   procedure Tick (Object : in out Table; New_Fault : out Boolean) is
      Now : Unsigned_64;
      Status : Driver.Result;
      procedure Quarantine (I : Ordinal) is
      begin
         Driver.Fail (Object.Items (I).Item);
         Object.Items (I).Drain.Finished := True;
         New_Fault := True;
      end Quarantine;
   begin
      New_Fault := False;
      if Object.Items = null then return; end if;
      for I in 1 .. Object.Count loop
         if Object.Items (I).Opened and then Object.Items (I).Retired and then
           not Object.Items (I).Drain.Finished
         then
            declare
               Item : Context_Record renames Object.Items (I);
               ID : constant Unsigned_32 := First_ID + Unsigned_32 (I - 1);
            begin
               if Object.Broken or else not Owner_Ready then
                  Quarantine (I);
               elsif Driver.State (Item.Item) in Life.Deregistered | Life.Quarantined then
                  Item.Drain.Finished := True;
               elsif Driver.State (Item.Item) in Life.Ready | Life.Fresh then
                  -- Initialized, never sent to the GuC: nothing to drain.
                  Item.Drain.Finished := True;
               elsif Driver.State (Item.Item) not in
                 Life.Enable_Pending | Life.Enabled | Life.Disable_Pending |
                 Life.Disabled | Life.Deregister_Pending
               then
                  -- Registration sent but never runnable. Retain registration
                  -- and backing forever; do not enable a closing session just
                  -- to disable it.
                  Quarantine (I);
               else
                  Now := Now_Us;
                  if not Item.Drain.Started then
                     Item.Drain.Started := True;
                     Item.Drain.First := Now;
                     Item.Drain.Previous := Now;
                  end if;
                  if not Owner_Ready or else Now = Unsigned_64'Last or else
                    Now < Item.Drain.Previous or else
                    Now - Item.Drain.First >= Drain_Deadline_Us or else
                    Item.Drain.Polls = Drain_Poll_Limit
                  then
                     Quarantine (I);
                  else
                     Item.Drain.Previous := Now;
                     Item.Drain.Polls := Item.Drain.Polls + 1;
                     if Driver.State (Item.Item) = Life.Enabled then
                        Submit (Object, ID, Life.Disable, Status);
                        if Status not in Driver.Queued | Driver.Backpressure then
                           Quarantine (I);
                        end if;
                     elsif Driver.State (Item.Item) = Life.Disabled and then
                       not Item.Work_Held and then Work_Drained (Item.Owner)
                     then
                        Deregister_Retired (Object, ID, True, Status);
                        if Status not in Driver.Queued | Driver.Backpressure then
                           Quarantine (I);
                        end if;
                     end if;
                  end if;
               end if;
            end;
         end if;
      end loop;
   end Tick;
end Intel_GPU_Context_Table.Draining;
