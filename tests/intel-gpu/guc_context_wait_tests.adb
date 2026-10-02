with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_GuC_Context_Event;
with Intel_GPU_GuC_Context_Lifecycle;
with Intel_GPU_GuC_Context_Session;
with Intel_GPU_GuC_Context_Wait;
with Intel_GPU_GuC_CT_Receive;
procedure GuC_Context_Wait_Tests is
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   package Events renames Intel_GPU_GuC_Context_Event;
   use type Life.Phase;
   Scenario, Polls, Sends, Retained, Clock_Reads : Natural := 0;
   Clock : Unsigned_64 := 0;
   Owner : Boolean := True;
   Runnable : Unsigned_32 := 1;
   Resuming : Boolean := False;
   function Ready return Boolean is (Owner);
   procedure Queue (Payload : Events.Words; Fence : Unsigned_16;
                    Result : out Life.Send_Result) is
   begin
      pragma Assert (Payload'Length > 0 and Fence in 100 .. 65535);
      Sends := Sends + 1;
      Result := (if Scenario = 1 and Sends = 3 then Life.Backpressure else Life.Queued);
      if Scenario = 10 and Sends = (if Resuming then 1 else 3)
      then Clock := 1_000_000; end if;
   end Queue;
   procedure Retain (Payload : Events.Words; Fence : Unsigned_16; Success : out Boolean) is
   begin
      pragma Assert (Payload'Length = 1 and Fence = 99);
      Retained := Retained + 1; Success := True;
   end Retain;
   package Driver is new Intel_GPU_GuC_Context_Session (Ready, Queue, Retain);
   procedure Descriptor (Head, Tail, Status : out Unsigned_32; Success : out Boolean) is
   begin Head := 0; Tail := 0; Status := 0; Success := False; end Descriptor;
   procedure Read_Word (Index : Unsigned_32; Value : out Unsigned_32; Success : out Boolean) is
   begin Value := Index; Success := False; end Read_Word;
   procedure Finish (Success : out Boolean) is
   begin Success := False; end Finish;
   procedure Head (Value : Unsigned_32; Success : out Boolean) is
   begin Success := Value = 0; end Head;
   package Receiver is new Intel_GPU_GuC_CT_Receive (Descriptor, Read_Word, Finish, Head, Finish);
   procedure Poll (Item : out Receiver.Message; Status : out Receiver.Result) is
   begin
      Polls := Polls + 1; Item := (others => <>); Status := Receiver.Empty;
      case Scenario is
         when 2 => return;
         when 3 => Clock := 1_000_000; return;
         when 4 => Clock := Unsigned_64'Last; return;
         when 5 => Owner := False; return;
         when 6 => Status := Receiver.Corrupt; return;
         when 7 =>
            Status := Receiver.Received; Item.Length := 1;
            Item.Fence := 100; Item.Payload (1) := 16#E0000001#; return;
         when 8 => Clock := 0; return;
         when others => null;
      end case;
      Status := Receiver.Received;
      if Polls = 1 then
         if Scenario = 12 then
            Item.Length := 3; Item.Payload (1 .. 3) := [16#90001002#, 9, 1];
         else
            Item.Length := 1; Item.Fence := 99; Item.Payload (1) := 16#90008002#;
         end if;
      else
         Item.Length := 3; Item.Payload (1 .. 3) := [16#90001002#, 7, Runnable];
      end if;
   end Poll;
   function Now_Us return Unsigned_64 is
   begin
      Clock_Reads := Clock_Reads + 1;
      if Scenario = 9 and Clock_Reads = 1 then Owner := False; end if;
      -- Lose ownership inside the final clock callback after successful
      -- dispatch; completion must still be rejected.
      if Scenario = 11 and Clock_Reads = 8 then Owner := False; end if;
      return Clock;
   end Now_Us;
   procedure Pause is null;
   Dispatches : Natural := 0;
   procedure Dispatch (Object : in out Driver.Session; Payload : Events.Words;
                       Fence : Unsigned_16; Status : out Driver.Result) is
   begin
      Dispatches := Dispatches + 1;
      if Scenario = 12 and then Payload'Length = 3 and then
        Payload (Payload'First + 1) = 9 then
         -- Model dispatch to another context without advancing this one.
         pragma Assert (Driver.State (Object) = Life.Enable_Pending);
         Status := Driver.Handled; return;
      end if;
      Driver.Dispatch (Object, Payload, Fence, Status);
   end Dispatch;
   package Waiter is new Intel_GPU_GuC_Context_Wait
     (Driver, Receiver, Ready, Poll, Now_Us, Pause, Dispatch);
   use type Waiter.Result;
   Status : Waiter.Result;
   Submitted : Driver.Result;
begin
   for Case_ID in 0 .. 12 loop
      declare Object : Driver.Session; begin
         Scenario := Case_ID; Polls := 0; Sends := 0; Retained := 0;
         Clock := 0; Owner := True; Runnable := 1;
         Clock_Reads := 0; Dispatches := 0;
         if Case_ID = 8 then Clock := 100; end if;
         Driver.Initialize (Object, 7, 16#200000#, 4096, 100, 65535, 1000, 500000, False);
         Waiter.Execute (Object, Life.Enable, 10, Status);
         pragma Assert (Status = Waiter.Rejected and Sends = 0 and Polls = 0);
         Driver.Submit (Object, Life.Register_Context, Submitted);
         Driver.Submit (Object, Life.Set_Policy, Submitted);
         Waiter.Execute (Object, Life.Enable, 10, Status);
         case Case_ID is
            when 0 | 1 =>
               pragma Assert (Status = Waiter.Complete and Driver.State (Object) = Life.Enabled);
               pragma Assert (Retained = 1 and Sends = (if Case_ID = 1 then 4 else 3));
               pragma Assert (Dispatches = 2);
               Runnable := 0; Polls := 0;
               Waiter.Execute (Object, Life.Disable, 10, Status);
               pragma Assert (Status = Waiter.Complete and Driver.State (Object) = Life.Disabled);
               -- VM updates disable an already registered context, then use
               -- the SAME wait helper to resume it. Test repeated transitions
               -- here, not only the underlying lifecycle in isolation.
               for Cycle in 1 .. 4 loop
                  Runnable := 1; Polls := 0;
                  Waiter.Execute (Object, Life.Enable, 10, Status);
                  pragma Assert (Status = Waiter.Complete and Driver.State (Object) = Life.Enabled);
                  Runnable := 0; Polls := 0;
                  Waiter.Execute (Object, Life.Disable, 10, Status);
                  pragma Assert (Status = Waiter.Complete and Driver.State (Object) = Life.Disabled);
               end loop;
            when 2 | 3 | 10 => pragma Assert (Status = Waiter.Timed_Out);
            when 4 | 8 => pragma Assert (Status = Waiter.Invalid_Clock);
            when 5 | 9 | 11 => pragma Assert (Status = Waiter.Ownership_Lost);
            when 6 => pragma Assert (Status = Waiter.Receive_Failed);
            when 7 => pragma Assert (Status = Waiter.Context_Failed);
            when 12 =>
               pragma Assert (Status = Waiter.Complete and Polls = 2 and
                              Dispatches = 2 and Retained = 0 and
                              Driver.State (Object) = Life.Enabled);
         end case;
         if Case_ID in 2 .. 11 then
            pragma Assert (Driver.State (Object) = Life.Quarantined);
         end if;
      end;
   end loop;
   -- Re-enabling must retain every failure fence, not merely accept the new
   -- starting state. Establish Disabled using real session transitions first.
   for Case_ID in 2 .. 11 loop
      declare
         Object : Driver.Session;
         Before_Sends : Natural;
      begin
         Scenario := 0; Owner := True; Clock := 0;
         Sends := 0; Polls := 0; Clock_Reads := 0; Runnable := 1;
         Driver.Initialize (Object, 7, 16#200000#, 4096, 100, 65535, 1000, 500000, False);
         Driver.Submit (Object, Life.Register_Context, Submitted);
         Driver.Submit (Object, Life.Set_Policy, Submitted);
         Waiter.Execute (Object, Life.Enable, 10, Status);
         pragma Assert (Status = Waiter.Complete);
         Runnable := 0; Polls := 0;
         Waiter.Execute (Object, Life.Disable, 10, Status);
         pragma Assert (Status = Waiter.Complete and Driver.State (Object) = Life.Disabled);
         Resuming := True; Scenario := Case_ID;
         Sends := 0; Polls := 0; Clock_Reads := 0; Runnable := 1;
         if Case_ID = 8 then Clock := 100; end if;
         Waiter.Execute (Object, Life.Enable, 10, Status);
         case Case_ID is
            when 2 | 3 | 10 => pragma Assert (Status = Waiter.Timed_Out);
            when 4 | 8 => pragma Assert (Status = Waiter.Invalid_Clock);
            when 5 | 9 | 11 => pragma Assert (Status = Waiter.Ownership_Lost);
            when 6 => pragma Assert (Status = Waiter.Receive_Failed);
            when 7 => pragma Assert (Status = Waiter.Context_Failed);
         end case;
         pragma Assert (Driver.State (Object) = Life.Quarantined);
         Before_Sends := Sends;
         Scenario := 0; Owner := True; Clock := 0;
         Waiter.Execute (Object, Life.Enable, 10, Status);
         pragma Assert (Status = Waiter.Rejected and Sends = Before_Sends and
                        Driver.State (Object) = Life.Quarantined);
         Resuming := False;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("GuC context scheduling wait PASS: initial/resumed enable, repeated cycles, fault quarantine (mock transport/clock)");
end GuC_Context_Wait_Tests;
