with Interfaces; use Interfaces;
with Intel_GPU_GuC_Context_Event;
with Intel_GPU_GuC_Context_Lifecycle;
with Intel_GPU_GuC_Context_Session;
procedure GuC_Context_Session_Tests is
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   package Events renames Intel_GPU_GuC_Context_Event;
   use type Life.Phase;
   use type Events.Words;
   Owner, Retain_OK : Boolean := True;
   Lose_Owner_On_Queue : Boolean := False;
   Outcome : Life.Send_Result := Life.Queued;
   Calls, Retained_Count : Natural := 0;
   Last_Payload : Events.Words (0 .. 11) := [others => 0];
   Last_Length : Natural := 0;
   function Ready return Boolean is (Owner);
   procedure Queue (Payload : Events.Words;
                    Result : out Life.Send_Result) is
   begin
      Calls := Calls + 1; Last_Length := Payload'Length;
      Last_Payload := [others => 0];
      Last_Payload (0 .. Payload'Length - 1) := Payload;
      Result := Outcome;
      if Lose_Owner_On_Queue then Owner := False; end if;
   end Queue;
   procedure Retain (Payload : Events.Words; Fence : Unsigned_16;
                     Success : out Boolean) is
   begin
      pragma Assert (Payload'Length > 0 and Fence = 99);
      Retained_Count := Retained_Count + 1; Success := Retain_OK;
   end Retain;
   package Driver is new Intel_GPU_GuC_Context_Session (Ready, Queue, Retain);
   use type Driver.Result;
   Status : Driver.Result;
begin
   declare Object : Driver.Session; Saved : Natural; begin
      Driver.Initialize (Object, 7, 16#200000#, 4096, 1000, 500000, False);
      Saved := Retained_Count;
      Driver.Dispatch (Object, [16#90004600#, 8], 99, Status);
      pragma Assert (Status = Driver.Retained and Retained_Count = Saved + 1);
      pragma Assert (Driver.State (Object) = Life.Ready);
      Retain_OK := False;
      Driver.Dispatch (Object, [16#90004600#, 8], 99, Status);
      pragma Assert (Status = Driver.Faulted and Driver.State (Object) = Life.Quarantined);
      Retain_OK := True;
      Retained_Count := 0;
   end;
   for Scenario in 0 .. 5 loop
      declare Object : Driver.Session; Saved : Natural; begin
         Owner := True; Retain_OK := True; Outcome := Life.Queued;
         Driver.Initialize (Object, 7, 16#200000#, 4096, 1000, 500000, False);
         Saved := Calls;
         Driver.Submit (Object, Life.Enable, Status);
         pragma Assert (Status = Driver.Rejected and Calls = Saved);
         Driver.Submit (Object, Life.Register_Context, Status);
         pragma Assert (Status = Driver.Queued and Last_Length = 12);
         pragma Assert (Last_Payload = [16#20004502#, 1, 7, 0, 1, 0, 0, 0, 0, 0, 16#20031D#, 0]);
         Outcome := Life.Backpressure;
         Driver.Submit (Object, Life.Set_Policy, Status);
         pragma Assert (Status = Driver.Backpressure and Driver.State (Object) = Life.Registration_Queued);
         Outcome := Life.Queued;
         Driver.Submit (Object, Life.Set_Policy, Status);
         pragma Assert (Status = Driver.Queued and Last_Length = 10);
         pragma Assert (Last_Payload (0 .. 9) =
           [16#2000100B#, 7, 16#20030001#, 2, 16#20010001#, 1000,
            16#20020001#, 500000, 16#20050001#, 0]);
         if Scenario = 1 then Outcome := Life.Uncertain; end if;
         Driver.Submit (Object, Life.Enable, Status);
         pragma Assert (Last_Length = 3 and
                        Last_Payload (0 .. 2) = [16#20001001#, 7, 1]);
         if Scenario = 1 then
            pragma Assert (Status = Driver.Faulted);
         elsif Scenario = 2 then
            Driver.Dispatch (Object, [16#E0000001#], 16#8000#, Status);
            pragma Assert (Status = Driver.Faulted);
         elsif Scenario = 3 then
            Driver.Dispatch (Object, [16#90001002#, 7, 0], 0, Status);
            pragma Assert (Status = Driver.Faulted);
         elsif Scenario = 4 then
            Owner := False;
            Driver.Dispatch (Object, [16#90001002#, 7, 1], 0, Status);
            pragma Assert (Status = Driver.Faulted);
         elsif Scenario = 5 then
            Retain_OK := False;
            Driver.Dispatch (Object, [16#90008002#, 0], 99, Status);
            pragma Assert (Status = Driver.Faulted);
         else
            Driver.Dispatch (Object, [16#90008002#, 0], 99, Status);
            pragma Assert (Status = Driver.Retained);
            Driver.Dispatch (Object, [16#90001002#, 7, 1], 0, Status);
            pragma Assert (Status = Driver.Handled and Driver.State (Object) = Life.Enabled);
            Saved := Calls;
            Driver.Notify_Work (Object, False, Status);
            pragma Assert (Status = Driver.Rejected and Calls = Saved);
            Outcome := Life.Backpressure;
            Driver.Notify_Work (Object, True, Status);
            pragma Assert (Status = Driver.Backpressure);
            Outcome := Life.Queued;
            Driver.Notify_Work (Object, True, Status);
            pragma Assert (Status = Driver.Queued and
              Last_Length = 2 and Last_Payload (0 .. 1) = [16#20001000#, 7]);
            Driver.Notify_Work (Object, True, Status);
            pragma Assert (Status = Driver.Queued and
              Driver.State (Object) = Life.Enabled);
            Driver.Submit (Object, Life.Disable, Status);
            pragma Assert (Status = Driver.Queued);
            Driver.Dispatch (Object, [16#90001002#, 7, 0], 0, Status);
            pragma Assert (Status = Driver.Handled and Driver.State (Object) = Life.Disabled);
            Driver.Fail (Object);
         end if;
         pragma Assert (Driver.State (Object) = Life.Quarantined);
         Saved := Calls;
         Driver.Submit (Object, Life.Enable, Status);
         pragma Assert (Status = Driver.Rejected and Calls = Saved);
      end;
   end loop;
   pragma Assert (Retained_Count = 2);

   -- Notification failures must remain quarantined across retries. Test the
   -- session boundary as well as the pure lifecycle: ownership can disappear
   -- inside the transport callback, including a nonpublishing callback.
   for Scenario in 0 .. 5 loop
      declare
         Object : Driver.Session;
         Saved : Natural;
      begin
         Owner := True; Outcome := Life.Queued;
         Lose_Owner_On_Queue := False;
         Driver.Initialize (Object, 7, 16#200000#, 4096,
                            1000, 500000, False);
         Driver.Submit (Object, Life.Register_Context, Status);
         Driver.Submit (Object, Life.Set_Policy, Status);
         Driver.Submit (Object, Life.Enable, Status);
         Driver.Dispatch (Object, [16#90001002#, 7, 1], 0, Status);
         pragma Assert (Status = Driver.Handled and
                        Driver.State (Object) = Life.Enabled);
         Saved := Calls;
         case Scenario is
            when 0 => Owner := False;
            when 1 => Outcome := Life.Uncertain;
            when 2 => Lose_Owner_On_Queue := True;
            when 3 =>
               Lose_Owner_On_Queue := True;
               Outcome := Life.Backpressure;
            when others => null;
         end case;
         Driver.Notify_Work (Object, True, Status);
         pragma Assert (Calls = Saved + (if Scenario = 0 then 0 else 1));
         if Scenario >= 4 then
            pragma Assert (Status = Driver.Queued);
            if Scenario = 5 then
               Driver.Submit (Object, Life.Disable, Status);
               Driver.Dispatch (Object, [16#90001002#, 7, 0], 0, Status);
               pragma Assert (Driver.State (Object) = Life.Disabled);
            end if;
            -- Transport-classified failures quarantine even after disable.
            Driver.Dispatch (Object, [16#E0000001#], 16#8000#, Status);
         end if;
         pragma Assert (Status = Driver.Faulted and
                        Driver.State (Object) = Life.Quarantined);
         Owner := True; Lose_Owner_On_Queue := False; Outcome := Life.Queued;
         Saved := Calls;
         Driver.Notify_Work (Object, True, Status);
         pragma Assert (Status = Driver.Rejected and Calls = Saved);
         Driver.Submit (Object, Life.Disable, Status);
         pragma Assert (Status = Driver.Rejected and Calls = Saved);
      end;
   end loop;
   for Scenario in 0 .. 5 loop
      declare Object : Driver.Session; Saved : Natural; begin
         Owner := True; Retain_OK := True; Outcome := Life.Queued;
         Lose_Owner_On_Queue := False;
         Driver.Initialize (Object, 7, 16#200000#, 4096, 1000, 500000, False);
         Driver.Deregister (Object, True, True, Status);
         pragma Assert (Status = Driver.Rejected);
         Driver.Submit (Object, Life.Register_Context, Status);
         Driver.Submit (Object, Life.Set_Policy, Status);
         Driver.Submit (Object, Life.Enable, Status);
         Driver.Dispatch (Object, [16#90001002#, 7, 1], 0, Status);
         Driver.Submit (Object, Life.Disable, Status);
         Driver.Dispatch (Object, [16#90001002#, 7, 0], 0, Status);
         pragma Assert (Driver.State (Object) = Life.Disabled);
         Saved := Calls;
         Driver.Deregister (Object, False, True, Status);
         pragma Assert (Status = Driver.Rejected and Calls = Saved);
         Driver.Deregister (Object, True, False, Status);
         pragma Assert (Status = Driver.Rejected and Calls = Saved);
         case Scenario is
            when 1 => Owner := False;
            when 2 => Outcome := Life.Uncertain;
            when 3 => Lose_Owner_On_Queue := True;
            when 4 => Outcome := Life.Backpressure;
            when others => null;
         end case;
         Driver.Deregister (Object, True, True, Status);
         pragma Assert (Calls = Saved + (if Scenario = 1 then 0 else 1));
         if Scenario in 1 .. 3 then
            pragma Assert (Status = Driver.Faulted and Driver.State (Object) = Life.Quarantined);
         else
            pragma Assert (Last_Length = 2 and Last_Payload (0 .. 1) = [16#20004503#,7]);
            if Scenario = 4 then
               pragma Assert (Status = Driver.Backpressure and Driver.State (Object) = Life.Disabled);
               Outcome := Life.Queued;
               Driver.Deregister (Object, True, True, Status);
            end if;
            pragma Assert (Status = Driver.Queued and Driver.State (Object) = Life.Deregister_Pending);
            if Scenario = 5 then
               Driver.Dispatch (Object, [16#E0000001#], 16#8000#, Status);
               pragma Assert (Status = Driver.Faulted);
            else
               Driver.Dispatch (Object, [16#90004600#,7], 0, Status);
               pragma Assert (Status = Driver.Handled and Driver.State (Object) = Life.Deregistered);
               Driver.Dispatch (Object, [16#90004600#,7], 0, Status);
               pragma Assert (Status = Driver.Faulted);
            end if;
            pragma Assert (Driver.State (Object) = Life.Quarantined);
         end if;
      end;
   end loop;
end GuC_Context_Session_Tests;
