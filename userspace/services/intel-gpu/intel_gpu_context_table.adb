with Intel_GPU_GuC_Fast_Fences;
package body Intel_GPU_Context_Table is
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   use type Life.Phase;
   use type Life.Operation;
   use type Driver.Result;
   function Count (Object : Table) return Natural is (Object.Used);
   function Failed (Object : Table) return Boolean is (Object.Broken);
   function Known (Object : Table; ID : Unsigned_32) return Boolean is
     (ID >= First_ID and then ID - First_ID < Unsigned_32 (Object.Used));
   function State (Object : Table; ID : Unsigned_32) return Life.Phase is
     (if Known (Object, ID) then Driver.State (Object.Items (Natural (ID - First_ID) + 1))
      else Life.Fresh);
   function Can_Run_And_Retire (Object : Table; ID : Unsigned_32) return Boolean is
     (not Object.Broken and then Owner_Ready and then Known (Object, ID) and then
      not Object.Retired (Natural (ID - First_ID) + 1) and then
      not Object.Work_Held (Natural (ID - First_ID) + 1) and then
      Driver.Can_Run_And_Retire (Object.Items (Natural (ID - First_ID) + 1)));
   function Session_Context (Object : Table; Session : Unsigned_64) return Unsigned_32 is
   begin
      if Session = 0 or else Object.Broken or else not Owner_Ready then return No_Context; end if;
      for I in 1 .. Object.Used loop
         if Object.Session_Owners (I) = Session then
            return (if Object.Retired (I) or else
                       Driver.State (Object.Items (I)) = Life.Quarantined then No_Context
                    else First_ID + Unsigned_32 (I - 1));
         end if;
      end loop;
      return No_Context;
   end Session_Context;
   procedure Retire_Session
     (Object : in out Table; Session : Unsigned_64; ID : out Unsigned_32) is
   begin
      ID := No_Context;
      if Session = 0 then return; end if;
      for I in 1 .. Object.Used loop
         if Object.Session_Owners (I) = Session then
            Object.Retired (I) := True;
            ID := First_ID + Unsigned_32 (I - 1);
            return;
         end if;
      end loop;
   end Retire_Session;
   procedure Fail (Object : in out Table) is
   begin
      Object.Broken := True;
      for I in 1 .. Object.Used loop Driver.Fail (Object.Items (I)); end loop;
   end Fail;
   function Ready (Object : in out Table) return Boolean is
   begin
      if not Owner_Ready then Fail (Object); end if;
      return not Object.Broken;
   end Ready;
   function Work_Allowed (Object : Table; ID : Unsigned_32) return Boolean is
     (not Object.Broken and then Owner_Ready and then Known (Object, ID)
      and then not Object.Retired (Natural (ID - First_ID) + 1)
      and then not Object.Work_Held (Natural (ID - First_ID) + 1)
      and then State (Object, ID) = Life.Enabled);
   procedure Hold_Work
     (Object : in out Table; ID : Unsigned_32; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Ready (Object) or else not Known (Object, ID) then return; end if;
      declare I : constant Positive := Natural (ID - First_ID) + 1; begin
         if Object.Retired (I) or else Object.Work_Held (I) or else
           State (Object, ID) not in Life.Enabled | Life.Disabled then return; end if;
         Object.Work_Held (I) := True;
         Accepted := True;
      end;
   end Hold_Work;
   procedure Release_Work
     (Object : in out Table; ID : Unsigned_32; Accepted : out Boolean;
      Keep_Disabled : Boolean := False) is
   begin
      Accepted := False;
      if not Ready (Object) or else not Known (Object, ID) then return; end if;
      declare I : constant Positive := Natural (ID - First_ID) + 1; begin
         if Object.Retired (I) or else not Object.Work_Held (I) or else
           State (Object, ID) /=
             (if Keep_Disabled then Life.Disabled else Life.Enabled)
         then return; end if;
         Object.Work_Held (I) := False;
         Accepted := True;
      end;
   end Release_Work;
   procedure Deregister_Retired
     (Object : in out Table; ID : Unsigned_32; Work_Drained : Boolean;
      Status : out Driver.Result) is
   begin
      Status := Driver.Faulted;
      if not Ready (Object) then return; end if;
      Status := Driver.Rejected;
      if not Known (Object, ID) then return; end if;
      declare I : constant Positive := Natural (ID - First_ID) + 1; begin
         if not Object.Retired (I) or else Object.Work_Held (I) or else
           not Work_Drained or else State (Object, ID) /= Life.Disabled
         then return; end if;
         Driver.Deregister (Object.Items (I), Admission_Closed => True,
                            Work_Drained => True, Status => Status);
         if not Ready (Object) then Status := Driver.Faulted; end if;
      end;
   end Deregister_Retired;
   procedure Open
     (Object : in out Table; GPU_Start, Pin_Bias : Unsigned_64;
      Quantum_Us, Preemption_Us : Unsigned_32;
      Preempt_To_Idle : Boolean; ID : out Unsigned_32; Accepted : out Boolean;
      Session : Unsigned_64 := 0) is
   begin
      ID := No_Context; Accepted := False;
      if not Ready (Object) or else Object.Used = Capacity then return; end if;
      if Session /= 0 then
         for I in 1 .. Object.Used loop
            if Object.Session_Owners (I) = Session then return; end if;
         end loop;
      end if;
      Object.Used := Object.Used + 1;
      Object.Session_Owners (Object.Used) := Session;
      ID := First_ID + Unsigned_32 (Object.Used - 1);
      Driver.Initialize (Object.Items (Object.Used), ID, GPU_Start, Pin_Bias,
                         Quantum_Us, Preemption_Us, Preempt_To_Idle);
      if not Ready (Object) then return; end if;
      Accepted := State (Object, ID) = Life.Ready;
   end Open;
   procedure Submit
     (Object : in out Table; ID : Unsigned_32; Action : Life.Operation;
      Status : out Driver.Result) is
   begin
      Status := Driver.Faulted;
      if not Ready (Object) then return; end if;
      Status := Driver.Rejected;
      if not Known (Object, ID) then return; end if;
      if Object.Retired (Natural (ID - First_ID) + 1) and then Action /= Life.Disable
      then return; end if;
      Driver.Submit (Object.Items (Natural (ID - First_ID) + 1), Action, Status);
      if not Ready (Object) then Status := Driver.Faulted; end if;
   end Submit;
   procedure Notify_Work
     (Object : in out Table; ID : Unsigned_32; Tail_Published : Boolean;
      Status : out Driver.Result) is
   begin
      Status := Driver.Faulted;
      if not Ready (Object) then return; end if;
      Status := Driver.Rejected;
      if not Known (Object, ID) then return; end if;
      if not Work_Allowed (Object, ID) then return; end if;
      Driver.Notify_Work (Object.Items (Natural (ID - First_ID) + 1), Tail_Published, Status);
      if not Ready (Object) then Status := Driver.Faulted; end if;
   end Notify_Work;
   procedure Dispatch
     (Object : in out Table; Payload : Intel_GPU_GuC_Context_Event.Words;
      Fence : Unsigned_16; ID : out Unsigned_32; Status : out Dispatch_Result) is
      Outcome : Driver.Result;
      Kept : Boolean;
      Item : constant Intel_GPU_GuC_Context_Event.Event :=
        Intel_GPU_GuC_Context_Event.Decode (Payload, Fence);
      use type Intel_GPU_GuC_Context_Event.Kind;
   begin
      ID := No_Context; Status := Transport_Fault;
      if not Ready (Object) then return; end if;
      -- Fast wire IDs may wrap and are never context-completion authority.
      -- A delayed failure stops the entire table, irrespective of ownership
      -- or whether its diagnostic ID has since been issued again.
      if Item.Tag = Intel_GPU_GuC_Context_Event.Request_Failure and then
        Intel_GPU_GuC_Fast_Fences.Is_Fast (Fence)
      then
         Fail (Object); return;
      end if;
      if Item.Tag = Intel_GPU_GuC_Context_Event.Malformed then
         Fail (Object);
      elsif Item.Tag in Intel_GPU_GuC_Context_Event.Scheduling_Done |
        Intel_GPU_GuC_Context_Event.Deregister_Done and then Known (Object, Item.ID)
      then
         ID := Item.ID;
         Driver.Dispatch (Object.Items (Natural (ID - First_ID) + 1), Payload, Fence, Outcome);
         Status := (if Outcome in Driver.Faulted | Driver.Rejected then
                      Context_Fault else Delivered);
      else
         Retain (Payload, Fence, Kept);
         if Kept then Status := Retained; else Fail (Object); end if;
      end if;
      if not Ready (Object) then Status := Transport_Fault; end if;
   end Dispatch;
end Intel_GPU_Context_Table;
