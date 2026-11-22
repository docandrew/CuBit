package body Intel_GPU_GuC_Context_Lifecycle with SPARK_Mode is
   function State (Object : Context) return Phase is (Object.Value);
   function Credits_Held (Object : Context) return Natural is (Object.Credits);
   function Can_Run_And_Retire (Object : Context) return Boolean is
     (Object.Value = Disabled and then not Object.Sending and then
      not Object.Notification_Sending and then not Object.Deregister_Sending);
   procedure Initialize (Object : in out Context; ID : Unsigned_32;
                         Ownership_Ready : Boolean) is
   begin
      if Object.Value /= Fresh then return; end if;
      Object.Value := Quarantined;
      if not Ownership_Ready or else ID >= 65535 then return; end if;
      Object.ID := ID; Object.Value := Ready;
   end Initialize;
   procedure Prepare (Object : in out Context; Action : Operation;
                      Accepted : out Boolean) is
      Expected : constant array (Operation) of Phase :=
        [Ready, Registration_Queued, Policy_Queued, Enabled];
      Pending : constant array (Operation) of Phase :=
        [Register_Pending, Policy_Pending, Enable_Pending, Disable_Pending];
   begin
      Accepted := False;
      if Object.Sending or Object.Notification_Sending then return; end if;
      if Object.Value /= Expected (Action) and then
        not (Action = Enable and Object.Value = Disabled) then return; end if;
      Object.Before_Send := Object.Value;
      Object.Value := Pending (Action);
      Object.Active := Action; Object.Sending := True;
      if Action in Enable | Disable then Object.Credits := 4; end if;
      Accepted := True;
   end Prepare;
   procedure Sent (Object : in out Context; Result : Send_Result) is
   begin
      if not Object.Sending or Object.Value = Quarantined then
         Object.Value := Quarantined; return;
      end if;
      Object.Sending := False;
      case Result is
         when Backpressure =>
            Object.Value := Object.Before_Send;
            Object.Credits := 0;
         when Uncertain => Object.Value := Quarantined;
         when Queued =>
            case Object.Active is
               when Register_Context => Object.Value := Registration_Queued;
               when Set_Policy => Object.Value := Policy_Queued;
               when Enable | Disable => null; -- wait for separate event
            end case;
      end case;
   end Sent;
   procedure Prepare_Notification
     (Object : in out Context; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Value /= Enabled or else Object.Sending or else
        Object.Notification_Sending
      then return; end if;
      Object.Notification_Sending := True;
      Accepted := True;
   end Prepare_Notification;
   procedure Notification_Sent (Object : in out Context; Result : Send_Result) is
   begin
      if Object.Value = Quarantined then return; end if;
      if not Object.Notification_Sending or else Object.Value /= Enabled
      then
         Object.Value := Quarantined; return;
      end if;
      Object.Notification_Sending := False;
      case Result is
         when Backpressure => null;
         when Uncertain => Object.Value := Quarantined;
         when Queued => null;
      end case;
   end Notification_Sent;
   procedure Scheduling_Done (Object : in out Context; ID, Runnable : Unsigned_32;
                              Accepted : out Boolean) is
   begin
      Accepted := False;
      if ID /= Object.ID then return; end if;
      if Object.Sending or else Object.Notification_Sending or else Object.Credits /= 4 or else
        Object.Value not in Enable_Pending | Disable_Pending or else
        Runnable /= (if Object.Value = Enable_Pending then 1 else 0)
      then Object.Value := Quarantined; return; end if;
      Object.Value := (if Object.Value = Enable_Pending then Enabled else Disabled);
      Object.Credits := 0; Accepted := True;
   end Scheduling_Done;
   procedure Fail (Object : in out Context) is
   begin
      Object.Value := Quarantined;
   end Fail;
   procedure Prepare_Deregister
     (Object : in out Context; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Value /= Disabled or else Object.Sending or else
        Object.Notification_Sending or else Object.Deregister_Sending or else
        Object.Credits /= 0
      then return; end if;
      Object.Value := Deregister_Pending;
      Object.Deregister_Sending := True;
      Object.Credits := 3;
      Accepted := True;
   end Prepare_Deregister;
   procedure Deregister_Sent (Object : in out Context; Result : Send_Result) is
   begin
      if Object.Value = Quarantined then return; end if;
      if not Object.Deregister_Sending or else Object.Value /= Deregister_Pending
      then Object.Value := Quarantined; return; end if;
      Object.Deregister_Sending := False;
      case Result is
         when Backpressure =>
            -- Only a transport guarantee of no publication allows rollback.
            Object.Value := Disabled; Object.Credits := 0;
         when Queued => null;
         when Uncertain => Object.Value := Quarantined;
      end case;
   end Deregister_Sent;
   procedure Deregistration_Done
     (Object : in out Context; ID : Unsigned_32; Accepted : out Boolean) is
   begin
      Accepted := False;
      if ID /= Object.ID or else Object.Value = Quarantined then return; end if;
      if Object.Value /= Deregister_Pending or else Object.Deregister_Sending
        or else Object.Sending or else Object.Notification_Sending or else
        Object.Credits /= 3
      then Object.Value := Quarantined; return; end if;
      Object.Value := Deregistered; Object.Credits := 0;
      Accepted := True;
   end Deregistration_Done;
end Intel_GPU_GuC_Context_Lifecycle;
