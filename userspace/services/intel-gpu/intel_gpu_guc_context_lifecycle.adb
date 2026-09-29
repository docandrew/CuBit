package body Intel_GPU_GuC_Context_Lifecycle with SPARK_Mode is
   function State (Object : Context) return Phase is (Object.Value);
   function Credits_Held (Object : Context) return Natural is (Object.Credits);
   procedure Initialize (Object : in out Context; ID : Unsigned_32;
                         Fence_Base : Unsigned_16; Ownership_Ready : Boolean) is
   begin
      if Object.Value /= Fresh then return; end if;
      Object.Value := Quarantined;
      if not Ownership_Ready or ID >= 65535 or Fence_Base = 0 or
        Fence_Base > 65532 then return; end if;
      Object.ID := ID; Object.Base := Fence_Base; Object.Value := Ready;
   end Initialize;
   procedure Prepare (Object : in out Context; Action : Operation;
                      Fence : out Unsigned_16; Accepted : out Boolean) is
      Expected : constant array (Operation) of Phase :=
        [Ready, Registration_Queued, Policy_Queued, Enabled];
      Pending : constant array (Operation) of Phase :=
        [Register_Pending, Policy_Pending, Enable_Pending, Disable_Pending];
   begin
      Fence := 0; Accepted := False;
      if Object.Value /= Expected (Action) or Object.Sending or
        Object.Used (Action) then return; end if;
      Object.Value := Pending (Action);
      Object.Active := Action; Object.Sending := True;
      Object.Used (Action) := True;
      if Action in Enable | Disable then Object.Credits := 4; end if;
      Fence := Object.Base + Unsigned_16 (Operation'Pos (Action));
      Accepted := True;
   end Prepare;
   procedure Sent (Object : in out Context; Result : Send_Result) is
      Before : constant array (Operation) of Phase :=
        [Ready, Registration_Queued, Policy_Queued, Enabled];
   begin
      if not Object.Sending or Object.Value = Quarantined then
         Object.Value := Quarantined; return;
      end if;
      Object.Sending := False;
      case Result is
         when Backpressure =>
            Object.Value := Before (Object.Active);
            Object.Used (Object.Active) := False;
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
   procedure Failed_Request (Object : in out Context; Fence : Unsigned_16;
                             Matched : out Boolean) is
   begin
      Matched := False;
      for Action in Operation loop
         if Object.Used (Action) and then
           Fence = Object.Base + Unsigned_16 (Operation'Pos (Action))
         then
            Object.Value := Quarantined; Matched := True; return;
         end if;
      end loop;
   end Failed_Request;
   procedure Scheduling_Done (Object : in out Context; ID, Runnable : Unsigned_32;
                              Accepted : out Boolean) is
   begin
      Accepted := False;
      if ID /= Object.ID then return; end if;
      if Object.Sending or else Object.Credits /= 4 or else
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
end Intel_GPU_GuC_Context_Lifecycle;
