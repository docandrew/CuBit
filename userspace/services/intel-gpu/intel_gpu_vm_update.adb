package body Intel_GPU_VM_Update is
   function Current_Phase (Object : State) return Phase is (Object.Value);
   function Generation (Object : State) return Unsigned_64 is (Object.Epoch);
   function Can_Submit (Object : State) return Boolean is
     (Object.Value = Idle and then Owner_Ready and then Object.Value = Idle);
   procedure Fail (Object : in out State) is
   begin Object.Value := Quarantined; end Fail;
   procedure Begin_Update
     (Object : in out State; Expected_Generation : Unsigned_64;
      Accepted : out Boolean; Status : out Result) is
   begin
      Accepted := False; Status := Rejected;
      if Object.Advancing or else Object.Value /= Idle or else Object.Epoch /= Expected_Generation or else
        Object.Epoch = Unsigned_64'Last then return; end if;
      -- Close admission before invoking a predicate that may pump events.
      -- A nested update must not observe Idle, and a retirement delivered by
      -- the predicate must not be overwritten by the next stage assignment.
      Object.Value := Draining;
      Object.Advancing := True;
      if not Owner_Ready or else Object.Value /= Draining then
         Object.Advancing := False;
         Fail (Object); Status := Ownership_Lost; return;
      end if;
      Object.Advancing := False; Accepted := True;
   end Begin_Update;
   procedure Advance
     (Object : in out State; Finished : out Boolean; Status : out Result) is
      Stage : constant Phase := Object.Value;
      OK : Boolean := False;
      Stage_Finished : Boolean := True;
   begin
      Finished := True; Status := Rejected;
      if Object.Advancing or else Stage not in Draining .. Resuming then return; end if;
      Object.Advancing := True;
      if not Owner_Ready or else Object.Value /= Stage then
         Object.Advancing := False; Fail (Object); Status := Ownership_Lost; return;
      end if;
      case Stage is
         when Draining => Drain (OK); Status := Drain_Failed;
         when Publishing => Advance_Publication (Stage_Finished, OK); Status := Publication_Failed;
         when Invalidating => Invalidate (OK); Status := Invalidation_Failed;
         when Resuming => Advance_Resume (Stage_Finished, OK); Status := Resume_Failed;
         when others => null; -- rejected before callback admission
      end case;
      -- A callback may pump retirement or try to advance this transaction.
      -- Neither nested progress nor a terminal Fail may be overwritten.
      if Object.Value /= Stage or else not Owner_Ready or else Object.Value /= Stage then
         Object.Advancing := False; Fail (Object); Status := Ownership_Lost; return;
      end if;
      Object.Advancing := False;
      if not OK then Fail (Object); return; end if;
      if not Stage_Finished then Finished := False; Status := Rejected; return; end if;
      if Stage = Resuming then
         Object.Epoch := Object.Epoch + 1; Object.Value := Idle; Status := Complete;
      else
         Object.Value := Phase'Succ (Stage); Finished := False; Status := Rejected;
      end if;
   end Advance;
   procedure Resume_Once (Finished, Success : out Boolean) is
   begin
      Resume (Success); Finished := True;
   end Resume_Once;
   procedure Publish_Once (Finished, Success : out Boolean) is
   begin
      Publish (Success); Finished := True;
   end Publish_Once;
   procedure Advance_Synchronous is new Advance (Publish_Once);
   procedure Advance_Once
     (Object : in out State; Finished : out Boolean; Status : out Result) is
   begin Advance_Synchronous (Object, Finished, Status); end Advance_Once;
   procedure Execute (Object : in out State; Expected_Generation : Unsigned_64;
                      Status : out Result) is
      Accepted, Finished : Boolean;
   begin
      Begin_Update (Object, Expected_Generation, Accepted, Status);
      if not Accepted then return; end if;
      loop
         Advance_Synchronous (Object, Finished, Status);
         exit when Finished;
      end loop;
   end Execute;
end Intel_GPU_VM_Update;
