package body Intel_GPU_VM_Update is
   function Current_Phase (Object : State) return Phase is (Object.Value);
   function Generation (Object : State) return Unsigned_64 is (Object.Epoch);
   function Can_Submit (Object : State) return Boolean is
     (Object.Value = Idle and then Owner_Ready and then Object.Value = Idle);
   procedure Fail (Object : in out State) is
   begin Object.Value := Quarantined; end Fail;
   procedure Execute (Object : in out State; Expected_Generation : Unsigned_64;
                      Status : out Result) is
      OK : Boolean;
   begin
      Status := Rejected;
      if Object.Value /= Idle or else Object.Epoch /= Expected_Generation or else
        Object.Epoch = Unsigned_64'Last then return; end if;
      -- Close admission before invoking a predicate that may pump events.
      -- A nested update must not observe Idle, and a retirement delivered by
      -- the predicate must not be overwritten by the next stage assignment.
      Object.Value := Draining;
      if not Owner_Ready or else Object.Value /= Draining then
         Fail (Object); Status := Ownership_Lost; return;
      end if;
      for Stage in Draining .. Resuming loop
         Object.Value := Stage;
         if not Owner_Ready or else Object.Value /= Stage then
            Fail (Object); Status := Ownership_Lost; return;
         end if;
         case Stage is
            when Draining => Drain (OK); Status := Drain_Failed;
            when Publishing => Publish (OK); Status := Publication_Failed;
            when Invalidating => Invalidate (OK); Status := Invalidation_Failed;
            when Resuming => Resume (OK); Status := Resume_Failed;
            when others => raise Program_Error;
         end case;
         -- A dispatched retirement may explicitly Fail this state while a
         -- callback pumps events. Never overwrite that terminal transition.
         if Object.Value /= Stage or else not Owner_Ready or else Object.Value /= Stage then
            Fail (Object); Status := Ownership_Lost; return;
         end if;
         if not OK then Fail (Object); return; end if;
      end loop;
      Object.Epoch := Object.Epoch + 1;
      Object.Value := Idle;
      Status := Complete;
   end Execute;
end Intel_GPU_VM_Update;
