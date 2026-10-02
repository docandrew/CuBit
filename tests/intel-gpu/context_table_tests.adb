with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Context_Table;
with Intel_GPU_Context_Table.Waiting;
with Intel_GPU_Context_Table.Draining;
with Intel_GPU_GuC_CT_Receive;
with Intel_GPU_GuC_Context_Event;
with Intel_GPU_GuC_Context_Lifecycle;
with Intel_GPU_GuC_Context_Session;
procedure Context_Table_Tests is
   package Events renames Intel_GPU_GuC_Context_Event;
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   use type Life.Phase;
   Owner, Keep, Lose_On_Queue : Boolean := True;
   Sent_Fence : Unsigned_16 := 0;
   Kept : Natural := 0;
   Queue_Calls : Natural := 0;
   Queue_Result : Life.Send_Result := Life.Queued;
   function Ready return Boolean is (Owner);
   procedure Queue (Payload : Events.Words; Fence : Unsigned_16;
                    Result : out Life.Send_Result) is
   begin
      pragma Assert (Payload'Length > 0);
      Queue_Calls := Queue_Calls + 1;
      Sent_Fence := Fence; Result := Queue_Result;
      if Lose_On_Queue then Owner := False; end if;
   end Queue;
   procedure Retain (Payload : Events.Words; Fence : Unsigned_16;
                     Success : out Boolean) is
      pragma Unreferenced (Payload, Fence);
   begin Kept := Kept + 1; Success := Keep; end Retain;
   package Driver is new Intel_GPU_GuC_Context_Session (Ready, Queue, Retain);
   package Pool is new Intel_GPU_Context_Table (2, 100, 115, Driver, Ready, Retain);
   procedure Descriptor (Head, Tail, Status : out Unsigned_32; Success : out Boolean) is
   begin Head := 0; Tail := 0; Status := 0; Success := False; end Descriptor;
   procedure Read_Word (Index : Unsigned_32; Value : out Unsigned_32; Success : out Boolean) is
   begin Value := Index; Success := False; end Read_Word;
   procedure Finish (Success : out Boolean) is
   begin Success := False; end Finish;
   procedure Head (Value : Unsigned_32; Success : out Boolean) is
   begin Success := Value = 0; end Head;
   package Receiver is new Intel_GPU_GuC_CT_Receive (Descriptor, Read_Word, Finish, Head, Finish);
   Polls : Natural := 0;
   Only_Other, Fault_Other : Boolean := False;
   procedure Poll (Item : out Receiver.Message; Status : out Receiver.Result) is
   begin
      Polls := Polls + 1;
      Item := (others => <>); Status := Receiver.Empty;
      if Only_Other and Polls > 1 then return; end if;
      Status := Receiver.Received;
      if Fault_Other and Polls = 1 then
         Item.Length := 1; Item.Fence := 108; Item.Payload (1) := 16#E0000001#;
      else
         Item.Length := 3;
         Item.Payload (1 .. 3) := [16#90001002#, (if Polls = 1 then 2 else 1), 1];
      end if;
   end Poll;
   function Now_Us return Unsigned_64 is (0);
   procedure Pause is null;
   package Waiter is new Pool.Waiting (Receiver, Poll, Now_Us, Pause);
   Clock : Unsigned_64 := 0;
   function Drain_Now return Unsigned_64 is (Clock);
   Drained : Boolean := True;
   function Work_Drained (Session : Unsigned_64) return Boolean is
     (Drained and then Session = 42);
   package Draining is new Pool.Draining (Drain_Now, Work_Drained);
   use type Draining.Retirement_State;
   use type Waiter.Result;
   use type Driver.Result;
   use type Pool.Dispatch_Result;
   Object : Pool.Table;
   ID, A, B : Unsigned_32;
   OK : Boolean;
   Status : Driver.Result;
   Delivery : Pool.Dispatch_Result;
   procedure Open (T : in out Pool.Table; Result : out Unsigned_32) is
   begin
      Pool.Open (T, 16#200000# + Unsigned_64 (Pool.Count (T)) * 16#10000#,
                 4096, 8, 1000, 500000, False, Result, OK);
      pragma Assert (OK);
   end Open;
   procedure Enable (T : in out Pool.Table; Context : Unsigned_32) is
   begin
      Pool.Submit (T, Context, Life.Register_Context, Status);
      pragma Assert (Status = Driver.Queued);
      Pool.Submit (T, Context, Life.Set_Policy, Status);
      pragma Assert (Status = Driver.Queued);
      Pool.Submit (T, Context, Life.Enable, Status);
      pragma Assert (Status = Driver.Queued);
   end Enable;
begin
   -- Registration-only application transition: two queued controls are not
   -- runnable work. Losing its reply retires it without enabling to drain.
   declare
      T : Pool.Table;
      Drain : Draining.Drain_State;
      Registered, Retired : Unsigned_32;
      Fault : Boolean;
   begin
      Owner := True; Lose_On_Queue := False; Queue_Calls := 0;
      Pool.Open (T, 16#200000#, 4096, 8, 1000, 500000, True,
                 Registered, OK, Session => 42);
      pragma Assert (OK);
      for Action in Life.Register_Context .. Life.Set_Policy loop
         Pool.Submit (T, Registered, Action, Status);
         pragma Assert (Status = Driver.Queued);
      end loop;
      pragma Assert (Queue_Calls = 2 and Pool.State (T, Registered) = Life.Policy_Queued);
      pragma Assert (not Pool.Work_Allowed (T, Registered));
      Pool.Notify_Work (T, Registered, True, Status);
      pragma Assert (Status = Driver.Rejected and Queue_Calls = 2);
      Pool.Retire_Session (T, 42, Retired);
      pragma Assert (Retired = Registered and Pool.Session_Context (T, 42) = Pool.No_Context);
      Draining.Tick (T, Drain, Fault);
      pragma Assert (Fault and Pool.State (T, Registered) = Life.Quarantined);
      pragma Assert (Queue_Calls = 2);
      Pool.Submit (T, Registered, Life.Enable, Status);
      pragma Assert (Status = Driver.Rejected and Queue_Calls = 2);
   end;
   -- Exercise resume through real table/session/event routing, not only the
   -- scalar lifecycle. Retired contexts must never regain scheduling rights.
   for Scenario in 0 .. 2 loop
      declare
         T : Pool.Table;
         Before : Natural;
      begin
         Owner := True; Keep := True; Lose_On_Queue := False;
         Queue_Result := Life.Queued;
         Pool.Open (T, 16#200000#, 4096, 8, 1000, 500000, False, A, OK, Session => 42);
         pragma Assert (OK);
         Pool.Open (T, 16#210000#, 4096, 8, 1000, 500000, False, B, OK, Session => 43);
         pragma Assert (OK);
         Enable (T, A); Enable (T, B);
         Pool.Dispatch (T, [16#90001002#, A, 1], 0, ID, Delivery);
         Pool.Dispatch (T, [16#90001002#, B, 1], 0, ID, Delivery);
         Pool.Submit (T, A, Life.Disable, Status);
         pragma Assert (Status = Driver.Queued and Sent_Fence = 103);
         Pool.Dispatch (T, [16#90001002#, A, 0], 0, ID, Delivery);
         pragma Assert (Delivery = Pool.Delivered and Pool.State (T, A) = Life.Disabled);
         if Scenario = 2 then
            Pool.Retire_Session (T, 42, ID);
            Before := Queue_Calls;
            Pool.Submit (T, A, Life.Enable, Status);
            pragma Assert (Queue_Calls = Before and Status /= Driver.Queued);
         else
            Queue_Result := Life.Backpressure;
            Pool.Submit (T, A, Life.Enable, Status);
            pragma Assert (Sent_Fence = 104 and Status = Driver.Backpressure and
                           Pool.State (T, A) = Life.Disabled);
            Queue_Result := (if Scenario = 0 then Life.Queued else Life.Uncertain);
            Pool.Submit (T, A, Life.Enable, Status);
            pragma Assert (Sent_Fence = 104);
            if Scenario = 0 then
               pragma Assert (Status = Driver.Queued);
               Pool.Dispatch (T, [16#90001002#, A, 1], 0, ID, Delivery);
               pragma Assert (Delivery = Pool.Delivered and Pool.State (T, A) = Life.Enabled);
               Pool.Submit (T, A, Life.Disable, Status);
               pragma Assert (Status = Driver.Queued and Sent_Fence = 105);
               Pool.Dispatch (T, [16#90001002#, A, 0], 0, ID, Delivery);
               Pool.Retire_Session (T, 42, ID);
               Before := Queue_Calls;
               Pool.Submit (T, A, Life.Enable, Status);
               pragma Assert (Queue_Calls = Before and Status /= Driver.Queued);
            else
               pragma Assert (Pool.State (T, A) = Life.Quarantined);
            end if;
         end if;
         pragma Assert (Pool.State (T, B) = Life.Enabled and not Pool.Failed (T));
      end;
   end loop;
   for Scenario in 0 .. 4 loop
      declare
         T : Pool.Table;
         D : Draining.Drain_State;
         Fault : Boolean;
         Before : Natural;
      begin
         Owner := True; Keep := True; Lose_On_Queue := False; Clock := 0;
         Queue_Result := Life.Queued;
         Pool.Open (T, 16#200000#, 4096, 8, 1000, 500000, False, A, OK, Session => 42);
         pragma Assert (OK);
         Pool.Open (T, 16#210000#, 4096, 8, 1000, 500000, False, B, OK, Session => 43);
         pragma Assert (OK);
         Enable (T, A); Enable (T, B);
         Pool.Dispatch (T, [16#90001002#, A, 1], 102, ID, Delivery);
         Pool.Dispatch (T, [16#90001002#, B, 1], 110, ID, Delivery);
         Pool.Retire_Session (T, 42, ID);
         Before := Queue_Calls;
         case Scenario is
            when 0 | 3 => Queue_Result := Life.Backpressure;
            when 1 => Queue_Result := Life.Uncertain;
            when 2 => Lose_On_Queue := True;
            when others => null;
         end case;
         Draining.Tick (T, D, Fault);
         pragma Assert (Queue_Calls = Before + 1);
         if Scenario in 1 .. 2 then
            pragma Assert (Fault and Pool.State (T, A) = Life.Quarantined);
         elsif Scenario = 0 then
            pragma Assert (not Fault and Pool.State (T, A) = Life.Enabled);
            Queue_Result := Life.Queued;
            Draining.Tick (T, D, Fault);
            pragma Assert (not Fault and Queue_Calls = Before + 2);
            pragma Assert (Pool.State (T, A) = Life.Disable_Pending);
         else
            -- Clock stays frozen. Exhaust the finite tick budget in both
            -- no-publication backpressure and already-published cases.
            for Tick in 2 .. 100_000 loop
               Draining.Tick (T, D, Fault);
               pragma Assert (not Fault);
            end loop;
            Before := Queue_Calls;
            Draining.Tick (T, D, Fault);
            pragma Assert (Fault and Queue_Calls = Before);
            pragma Assert (Pool.State (T, A) = Life.Quarantined);
         end if;
         Before := Queue_Calls;
         Draining.Tick (T, D, Fault);
         pragma Assert (not Fault and Queue_Calls = Before);
         if Scenario /= 2 then
            pragma Assert (Pool.State (T, B) = Life.Enabled);
            pragma Assert (Pool.Session_Context (T, 43) = B);
         end if;
         Queue_Result := Life.Queued; Lose_On_Queue := False; Owner := True;
      end;
   end loop;
   for Scenario in 0 .. 3 loop
      declare
         T : Pool.Table;
         D : Draining.Drain_State;
         Fault : Boolean;
         Before : Unsigned_16;
      begin
         Owner := True; Keep := True; Lose_On_Queue := False; Clock := 10;
         pragma Assert (Draining.Observe (T, 0) = Draining.Uncertain);
         pragma Assert (Draining.Observe (T, 42) = Draining.No_Context);
         Pool.Open (T, 16#200000#, 4096, 8, 1000, 500000, False, A, OK, Session => 42);
         pragma Assert (OK);
         pragma Assert (Draining.Observe (T, 42) = Draining.Admission_Open);
         Enable (T, A);
         Pool.Retire_Session (T, 42, ID);
         pragma Assert (Draining.Observe (T, 42) = Draining.Pending);
         Before := Sent_Fence;
         Draining.Tick (T, D, Fault);
         pragma Assert (not Fault and Sent_Fence = Before);
         if Scenario = 0 then
            Pool.Dispatch (T, [16#90001002#, A, 1], 102, ID, Delivery);
            Draining.Tick (T, D, Fault);
            pragma Assert (not Fault and Pool.State (T, A) = Life.Disable_Pending);
            pragma Assert (Draining.Observe (T, 42) = Draining.Pending);
            Before := Sent_Fence;
            Draining.Tick (T, D, Fault);
            pragma Assert (not Fault and Sent_Fence = Before);
            Pool.Dispatch (T, [16#90001002#, A, 0], 103, ID, Delivery);
            pragma Assert (Draining.Observe (T, 42) = Draining.Disabled);
            Draining.Tick (T, D, Fault);
            pragma Assert (not Fault and Pool.State (T, A) = Life.Deregister_Pending);
            pragma Assert (Draining.Observe (T, 42) = Draining.Pending);
            Before := Sent_Fence;
            Draining.Tick (T, D, Fault);
            pragma Assert (not Fault and Sent_Fence = Before);
            Pool.Dispatch (T, [16#90004600#, A], 0, ID, Delivery);
            pragma Assert (Delivery = Pool.Delivered);
            Draining.Tick (T, D, Fault);
            pragma Assert (not Fault and Pool.State (T, A) = Life.Deregistered);
            pragma Assert (Draining.Observe (T, 42) = Draining.Deregistered);
            Owner := False;
            pragma Assert (Draining.Observe (T, 42) = Draining.Uncertain);
            Owner := True;
         else
            Clock := (case Scenario is when 1 => 1_000_010,
                      when 2 => 9, when others => Unsigned_64'Last);
            Draining.Tick (T, D, Fault);
            pragma Assert (Fault and Pool.State (T, A) = Life.Quarantined);
            pragma Assert (Draining.Observe (T, 42) = Draining.Uncertain);
            Draining.Tick (T, D, Fault);
            pragma Assert (not Fault);
         end if;
         pragma Assert (Pool.Session_Context (T, 42) = Pool.No_Context);
      end;
   end loop;
   -- Deregistration shares the bounded drain, but requires independent work
   -- evidence. Missing evidence/VM holds never become success on timeout.
   for Scenario in 0 .. 5 loop
      declare
         T : Pool.Table;
         D : Draining.Drain_State;
         Fault : Boolean;
         Before : Natural;
      begin
         Owner := True; Keep := True; Lose_On_Queue := False;
         Queue_Result := Life.Queued; Clock := 10; Drained := True;
         Pool.Open (T, 16#200000#, 4096, 8, 1000, 500000, False, A, OK, 42);
         pragma Assert (OK);
         Enable (T, A);
         Pool.Dispatch (T, [16#90001002#, A, 1], 102, ID, Delivery);
         Pool.Submit (T, A, Life.Disable, Status);
         Pool.Dispatch (T, [16#90001002#, A, 0], 103, ID, Delivery);
         if Scenario = 1 then
            Pool.Hold_Work (T, A, OK); pragma Assert (OK);
         end if;
         Pool.Retire_Session (T, 42, ID);
         Before := Queue_Calls;
         case Scenario is
            when 0 => Drained := False;
            when 2 => Queue_Result := Life.Backpressure;
            when 3 => Queue_Result := Life.Uncertain;
            when 4 => Lose_On_Queue := True;
            when others => null;
         end case;
         Draining.Tick (T, D, Fault);
         if Scenario in 0 .. 1 then
            pragma Assert (not Fault and Queue_Calls = Before);
            pragma Assert (Pool.State (T, A) = Life.Disabled);
            pragma Assert (Draining.Observe (T, 42) = Draining.Disabled);
         elsif Scenario in 3 .. 4 then
            pragma Assert (Fault and Pool.State (T, A) = Life.Quarantined);
         elsif Scenario = 2 then
            pragma Assert (not Fault and Pool.State (T, A) = Life.Disabled);
            pragma Assert (Sent_Fence = 104 and Queue_Calls = Before + 1);
            Queue_Result := Life.Queued;
            Draining.Tick (T, D, Fault);
            pragma Assert (not Fault and Queue_Calls = Before + 2 and Sent_Fence = 104);
            pragma Assert (Pool.State (T, A) = Life.Deregister_Pending);
         else
            pragma Assert (not Fault and Pool.State (T, A) = Life.Deregister_Pending);
         end if;
         Before := Queue_Calls;
         Clock := 1_000_010;
         Draining.Tick (T, D, Fault);
         pragma Assert (Fault = (Scenario not in 3 .. 4));
         pragma Assert (Queue_Calls = Before and Pool.State (T, A) = Life.Quarantined);
         Draining.Tick (T, D, Fault);
         pragma Assert (not Fault and Queue_Calls = Before);
      end;
   end loop;
   Owner := True; Lose_On_Queue := False; Drained := True; Queue_Result := Life.Queued;
   -- Closing a session is separate from hardware disable. A pending enable
   -- may still complete; route it, then allow only the trusted disable path.
   declare
      T : Pool.Table;
      Before : Unsigned_16;
   begin
      Owner := True; Keep := True; Lose_On_Queue := False;
      Pool.Open (T, 16#200000#, 4096, 8, 1000, 500000, False, A, OK, Session => 42);
      pragma Assert (OK);
      Pool.Open (T, 16#210000#, 4096, 8, 1000, 500000, False, B, OK, Session => 43);
      pragma Assert (OK);
      Enable (T, A); Enable (T, B);
      Pool.Retire_Session (T, 0, ID); pragma Assert (ID = Pool.No_Context);
      Pool.Retire_Session (T, 999, ID); pragma Assert (ID = Pool.No_Context);
      Pool.Retire_Session (T, 42, ID); pragma Assert (ID = A);
      pragma Assert (Pool.Session_Context (T, 42) = Pool.No_Context);
      pragma Assert (Pool.Session_Context (T, 43) = B);
      pragma Assert (Pool.State (T, A) = Life.Enable_Pending);
      Before := Sent_Fence;
      Pool.Notify_Work (T, A, True, Status);
      pragma Assert (Status = Driver.Rejected and Sent_Fence = Before);
      Pool.Submit (T, A, Life.Enable, Status);
      pragma Assert (Status = Driver.Rejected and Sent_Fence = Before);
      Pool.Dispatch (T, [16#90001002#, A, 1], 102, ID, Delivery);
      pragma Assert (Delivery = Pool.Delivered and Pool.State (T, A) = Life.Enabled);
      Pool.Notify_Work (T, A, True, Status);
      pragma Assert (Status = Driver.Rejected and Sent_Fence = Before);
      Pool.Submit (T, A, Life.Disable, Status);
      pragma Assert (Status = Driver.Queued);
      Pool.Dispatch (T, [16#90001002#, A, 0], 103, ID, Delivery);
      pragma Assert (Delivery = Pool.Delivered and Pool.State (T, A) = Life.Disabled);
      pragma Assert (Pool.Session_Context (T, 42) = Pool.No_Context);
      Owner := False;
      Pool.Retire_Session (T, 43, ID); pragma Assert (ID = B);
      Owner := True;
      pragma Assert (Pool.Session_Context (T, 43) = Pool.No_Context);
      Pool.Retire_Session (T, 42, ID); pragma Assert (ID = A);
      pragma Assert (Pool.Count (T) = 2 and Pool.Owns_Fence (T, 100));
   end;
   declare
      T : Pool.Table;
      Before : Natural;
   begin
      Owner := True; Keep := True; Lose_On_Queue := False;
      Pool.Open (T, 16#200000#, 4096, 8, 1000, 500000, False, A, OK, Session => 42);
      pragma Assert (OK and Pool.Session_Context (T, 42) = A);
      pragma Assert (Pool.Session_Context (T, 0) = Pool.No_Context);
      pragma Assert (Pool.Session_Context (T, 43) = Pool.No_Context);
      Before := Pool.Count (T);
      Pool.Open (T, 16#210000#, 4096, 8, 1000, 500000, False, ID, OK, Session => 42);
      pragma Assert (not OK and ID = Pool.No_Context and Pool.Count (T) = Before);
      Pool.Open (T, 16#210000#, 4096, 8, 1000, 500000, False, B, OK, Session => 43);
      pragma Assert (OK and A /= B and Pool.Session_Context (T, 43) = B);
      Enable (T, A); Enable (T, B);
      Pool.Dispatch (T, [16#E0000001#], 100, ID, Delivery);
      pragma Assert (Delivery = Pool.Context_Fault);
      pragma Assert (Pool.Session_Context (T, 42) = Pool.No_Context);
      pragma Assert (Pool.Session_Context (T, 43) = B);
      Owner := False;
      pragma Assert (Pool.Session_Context (T, 43) = Pool.No_Context);
      Owner := True;
      Pool.Fail (T);
      pragma Assert (Pool.Session_Context (T, 43) = Pool.No_Context);
   end;
   declare
      package Shifted is new Intel_GPU_Context_Table
        (2, 100, 115, Driver, Ready, Retain, First_ID => 7);
      T : Shifted.Table;
      Delivery : Shifted.Dispatch_Result;
      use type Shifted.Dispatch_Result;
   begin
      Owner := True; Keep := True; Lose_On_Queue := False;
      Shifted.Open (T, 16#200000#, 4096, 8, 1000, 500000, False, A, OK);
      pragma Assert (OK and A = 7);
      pragma Assert (Shifted.Session_Context (T, 0) = Shifted.No_Context);
      pragma Assert (Shifted.Owns_Fence (T, 100) and Shifted.Owns_Fence (T, 107));
      pragma Assert (not Shifted.Owns_Fence (T, 99) and not Shifted.Owns_Fence (T, 108));
      for Action in Life.Register_Context .. Life.Enable loop
         Shifted.Submit (T, A, Action, Status);
         pragma Assert (Status = Driver.Queued);
      end loop;
      Shifted.Dispatch (T, [16#90001002#, 7, 1], 102, ID, Delivery);
      pragma Assert (ID = 7 and Delivery = Shifted.Delivered);
      pragma Assert (Shifted.State (T, 7) = Life.Enabled);
      pragma Assert (Shifted.State (T, 1) = Life.Fresh);
      Shifted.Open (T, 16#210000#, 4096, 8, 1000, 500000, False, B, OK);
      pragma Assert (OK and B = 8 and Shifted.Owns_Fence (T, 108));
      Shifted.Fail (T);
      pragma Assert (not Shifted.Owns_Fence (T, 100));
   end;
   declare
      T : Pool.Table;
   begin
      Owner := True;
      Pool.Open (T, 0, 4096, 8, 1000, 500000, False, A, OK, Session => 42);
      pragma Assert (not OK and A /= Pool.No_Context and Pool.Count (T) = 1);
      pragma Assert (Pool.Session_Context (T, 42) = Pool.No_Context);
      Pool.Open (T, 16#200000#, 4096, 8, 1000, 500000, False, ID, OK, Session => 42);
      pragma Assert (not OK and ID = Pool.No_Context and Pool.Count (T) = 1);
      Pool.Open (T, 16#210000#, 4096, 8, 1000, 500000, False, B, OK, Session => 43);
      pragma Assert (OK and B /= A and Pool.Session_Context (T, 43) = B);
   end;
   for Scenario in 0 .. 2 loop
      declare T : Pool.Table; Wait_Status : Waiter.Result; begin
         Owner := True; Keep := True; Lose_On_Queue := False;
         Polls := 0; Only_Other := Scenario = 1; Fault_Other := Scenario = 2;
         Open (T, A); Open (T, B);
         Enable (T, B);
         Pool.Submit (T, A, Life.Register_Context, Status);
         Pool.Submit (T, A, Life.Set_Policy, Status);
         Waiter.Execute (T, A, Life.Enable, 3, Wait_Status);
         if Only_Other then
            pragma Assert (Wait_Status = Waiter.Timed_Out and Pool.Failed (T));
         else
            pragma Assert (Wait_Status = Waiter.Complete and Polls = 2);
            pragma Assert (Pool.State (T, A) = Life.Enabled);
            pragma Assert (Pool.State (T, B) =
              (if Fault_Other then Life.Quarantined else Life.Enabled));
         end if;
      end;
   end loop;
   Lose_On_Queue := False;
   Open (Object, A); Open (Object, B);
   pragma Assert (A = 1 and B = 2);
   Enable (Object, A); pragma Assert (Sent_Fence = 102);
   Enable (Object, B); pragma Assert (Sent_Fence = 110);
   Pool.Dispatch (Object, [16#90001002#, B, 1], 102, ID, Delivery);
   pragma Assert (ID = B and Delivery = Pool.Delivered);
   pragma Assert (Pool.State (Object, A) = Life.Enable_Pending);
   pragma Assert (Pool.State (Object, B) = Life.Enabled);
   Pool.Dispatch (Object, [16#90001002#, A, 1], 110, ID, Delivery);
   pragma Assert (ID = A and Delivery = Pool.Delivered);
   Pool.Notify_Work (Object, A, True, Status);
   pragma Assert (Status = Driver.Queued and Sent_Fence = 104);
   Pool.Notify_Work (Object, B, True, Status);
   pragma Assert (Status = Driver.Queued and Sent_Fence = 112);
   Pool.Dispatch (Object, [16#E0000001#], 104, ID, Delivery);
   pragma Assert (ID = A and Delivery = Pool.Context_Fault);
   pragma Assert (Pool.State (Object, A) = Life.Quarantined);
   pragma Assert (Pool.State (Object, B) = Life.Enabled and not Pool.Failed (Object));
   -- Late replies keep their old owner even after quarantine.
   Pool.Dispatch (Object, [16#E0000001#], 104, ID, Delivery);
   pragma Assert (ID = A and Delivery = Pool.Context_Fault);
   Pool.Open (Object, 16#300000#, 4096, 8, 1000, 500000, False, ID, OK);
   pragma Assert (not OK and Pool.Count (Object) = 2 and ID = Pool.No_Context);
   Pool.Dispatch (Object, [16#90001002#, 3, 1], 110, ID, Delivery);
   pragma Assert (Delivery = Pool.Retained and Kept = 1);
   Pool.Dispatch (Object, [16#E0000001#], 99, ID, Delivery);
   pragma Assert (Delivery = Pool.Retained and Kept = 2);
   Pool.Submit (Object, B, Life.Disable, Status);
   Pool.Dispatch (Object, [16#90001002#, B, 0], 0, ID, Delivery);
   pragma Assert (Delivery = Pool.Delivered and Pool.State (Object, B) = Life.Disabled);
   for Scenario in 0 .. 3 loop
      declare T : Pool.Table; begin
         Owner := True; Keep := True; Lose_On_Queue := False;
         Open (T, A); Open (T, B);
         case Scenario is
            when 0 => Pool.Dispatch (T, [0], 0, ID, Delivery);
            when 1 =>
               Keep := False;
               Pool.Dispatch (T, [16#F0000000#], 0, ID, Delivery);
            when 2 =>
               Owner := False;
               Pool.Dispatch (T, [16#F0000000#], 0, ID, Delivery);
            when 3 =>
               Lose_On_Queue := True;
               Pool.Submit (T, A, Life.Register_Context, Status);
               pragma Assert (Status = Driver.Faulted);
         end case;
         pragma Assert (Pool.Failed (T));
         pragma Assert (Pool.State (T, A) = Life.Quarantined and
                        Pool.State (T, B) = Life.Quarantined);
         Owner := True; -- ownership restoration cannot revive this lifetime
         Pool.Submit (T, B, Life.Register_Context, Status);
         pragma Assert (Status = Driver.Faulted);
      end;
   end loop;
   declare T : Pool.Table; begin
      Owner := True; Keep := True; Lose_On_Queue := False;
      Pool.Open (T, 16#200000#, 4096, 3, 1000, 500000, False, ID, OK);
      pragma Assert (not OK and ID = Pool.No_Context and Pool.Count (T) = 0);
      Pool.Open (T, 0, 4096, 8, 1000, 500000, False, A, OK);
      pragma Assert (not OK and A = 1 and Pool.Count (T) = 1);
      pragma Assert (Pool.State (T, A) = Life.Quarantined);
      Open (T, B);
      pragma Assert (B = 2);
      Enable (T, B);
      pragma Assert (Sent_Fence = 110); -- failed initialization's range retained
      Pool.Dispatch (T, [16#E0000001#], 100, ID, Delivery);
      pragma Assert (ID = A and Delivery = Pool.Context_Fault);
      pragma Assert (Pool.State (T, B) = Life.Enable_Pending);
      Pool.Submit (T, 0, Life.Enable, Status);
      pragma Assert (Status = Driver.Rejected);
      Pool.Notify_Work (T, Pool.No_Context, True, Status);
      pragma Assert (Status = Driver.Rejected);
   end;
   Ada.Text_IO.Put_Line ("Context table PASS (two hosted sessions, mock transport)");
end Context_Table_Tests;
