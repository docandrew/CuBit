with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Context_Table;
with Intel_GPU_GuC_Context_Event;
with Intel_GPU_GuC_Context_Lifecycle;
with Intel_GPU_GuC_Context_Session;
procedure Context_Deregister_Table_Tests is
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   package Events renames Intel_GPU_GuC_Context_Event;
   use type Life.Phase;
   use type Life.Operation;
   Owner, Lose_On_Queue : Boolean := False;
   Calls : Natural := 0;
   Last_Fence : Unsigned_16 := 0;
   Outcome : Life.Send_Result := Life.Queued;
   function Ready return Boolean is (Owner);
   procedure Queue (Payload : Events.Words; Fence : Unsigned_16;
                    Result : out Life.Send_Result) is
   begin
      Calls := Calls + 1;
      Last_Fence := Fence;
      if Payload (Payload'First) = 16#20004503# then
         pragma Assert (Payload'Length = 2 and Payload (Payload'First + 1) = 1);
      end if;
      Result := Outcome;
      if Lose_On_Queue then Owner := False; end if;
   end Queue;
   procedure Retain (Payload : Events.Words; Fence : Unsigned_16;
                     Success : out Boolean) is
      pragma Unreferenced (Payload, Fence);
   begin Success := True; end Retain;
   package Driver is new Intel_GPU_GuC_Context_Session (Ready, Queue, Retain);
   package Pool is new Intel_GPU_Context_Table (2, 100, 115, Driver, Ready, Retain);
   use type Driver.Result;
   use type Pool.Dispatch_Result;
begin
   for Scenario in 0 .. 6 loop
      declare
         T : Pool.Table;
         A, B, ID : Unsigned_32;
         OK : Boolean;
         Status : Driver.Result;
         Delivery : Pool.Dispatch_Result;
         Before : Natural;
         procedure Reject (Target : Unsigned_32; Drained : Boolean := True) is
            Count : constant Natural := Calls;
         begin
            Pool.Deregister_Retired (T, Target, Drained, Status);
            pragma Assert (Status = Driver.Rejected and Calls = Count);
         end Reject;
      begin
         Owner := True; Lose_On_Queue := False; Outcome := Life.Queued;
         Pool.Open (T, 16#200000#, 4096, 8, 1000, 500000, False, A, OK, 42);
         pragma Assert (OK and A = 1);
         Pool.Open (T, 16#210000#, 4096, 8, 1000, 500000, False, B, OK, 43);
         pragma Assert (OK);
         Reject (Pool.No_Context); Reject (A);
         for Action in Life.Register_Context .. Life.Enable loop
            Pool.Submit (T, A, Action, Status);
            pragma Assert (Status = Driver.Queued);
         end loop;
         Pool.Dispatch (T, [16#90001002#, A, 1], 102, ID, Delivery);
         pragma Assert (Delivery = Pool.Delivered);
         Reject (A);
         Pool.Submit (T, A, Life.Disable, Status);
         pragma Assert (Status = Driver.Queued);
         Reject (A);
         Pool.Dispatch (T, [16#90001002#, A, 0], 103, ID, Delivery);
         pragma Assert (Delivery = Pool.Delivered);
         Reject (A); -- Disabled alone does not close admission.
         if Scenario = 1 then
            Pool.Hold_Work (T, A, OK); pragma Assert (OK);
         end if;
         Pool.Retire_Session (T, 42, ID); pragma Assert (ID = A);
         Reject (A, False); Reject (B);
         Before := Calls;
         case Scenario is
            when 1 => Reject (A);
            when others =>
               if Scenario = 2 then Outcome := Life.Backpressure;
               elsif Scenario = 3 then Outcome := Life.Uncertain;
               elsif Scenario = 4 then Lose_On_Queue := True;
               elsif Scenario = 5 then Owner := False;
               end if;
               Pool.Deregister_Retired (T, A, True, Status);
               if Scenario = 5 then
                  pragma Assert (Status = Driver.Faulted and Calls = Before);
               else
                  pragma Assert (Calls = Before + 1 and Last_Fence = 104);
                  if Scenario in 3 .. 4 then
                     pragma Assert (Status = Driver.Faulted and Pool.State (T, A) = Life.Quarantined);
                  else
                     if Scenario = 2 then
                        pragma Assert (Status = Driver.Backpressure and Pool.State (T, A) = Life.Disabled);
                        Outcome := Life.Queued;
                        Pool.Deregister_Retired (T, A, True, Status);
                        pragma Assert (Last_Fence = 104 and Calls = Before + 2);
                     end if;
                     pragma Assert (Status = Driver.Queued and Pool.State (T, A) = Life.Deregister_Pending);
                     Reject (A); -- Never republish an outstanding request.
                     if Scenario = 6 then
                        Pool.Dispatch (T, [16#E0000000#], 104, ID, Delivery);
                        pragma Assert (Delivery = Pool.Context_Fault);
                     else
                        Pool.Dispatch (T, [16#90004600#, A], 0, ID, Delivery);
                        pragma Assert (Delivery = Pool.Delivered and ID = A);
                        pragma Assert (Pool.State (T, A) = Life.Deregistered);
                        -- Retained tombstone A must not block quiescence for
                        -- a different live session B. B still needs its own
                        -- scheduling acknowledgments; Fresh is not enough.
                        pragma Assert (Life.Scheduling_Stopped (Pool.State (T, A)));
                        pragma Assert (not Life.Scheduling_Stopped (Pool.State (T, B)));
                        for Action in Life.Operation loop
                           Pool.Submit (T, B, Action, Status);
                           pragma Assert (Status = Driver.Queued);
                           if Action in Life.Enable | Life.Disable then
                              Pool.Dispatch
                                (T, [16#90001002#, B,
                                     (if Action = Life.Enable then 1 else 0)],
                                 Last_Fence, ID, Delivery);
                              pragma Assert (Delivery = Pool.Delivered);
                           end if;
                        end loop;
                        pragma Assert
                          (Life.Scheduling_Stopped (Pool.State (T, A)) and
                           Life.Scheduling_Stopped (Pool.State (T, B)));
                        Reject (A);
                        pragma Assert (Pool.Owns_Fence (T, 104) and Pool.Count (T) = 2);
                        Pool.Dispatch (T, [16#90004600#, A], 0, ID, Delivery);
                        pragma Assert (Delivery = Pool.Context_Fault);
                     end if;
                  end if;
               end if;
         end case;
         pragma Assert (Pool.Session_Context (T, 42) = Pool.No_Context);
         if Scenario not in 4 .. 5 then
            pragma Assert (Pool.Session_Context (T, 43) = B and not Pool.Failed (T));
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Context table deregistration PASS: retirement/drain/hold gates, routing, retained identities, failures");
end Context_Deregister_Table_Tests;
