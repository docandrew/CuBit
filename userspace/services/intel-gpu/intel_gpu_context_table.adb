package body Intel_GPU_Context_Table is
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   use type Life.Phase;
   use type Life.Operation;
   use type Driver.Result;
   function Count (Object : Table) return Natural is (Object.Used);
   function Failed (Object : Table) return Boolean is (Object.Broken);
   function Owns_Fence (Object : Table; Fence : Unsigned_16) return Boolean is
     (not Object.Broken and then Routes.Owner (Object.Routing, Fence) /= Routes.No_Context);
   function Known (Object : Table; ID : Unsigned_32) return Boolean is
     (ID >= First_ID and then ID - First_ID < Unsigned_32 (Object.Used));
   function State (Object : Table; ID : Unsigned_32) return Life.Phase is
     (if Known (Object, ID) then Driver.State (Object.Items (Natural (ID - First_ID) + 1))
      else Life.Fresh);
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
      Fence_Count : Natural; Quantum_Us, Preemption_Us : Unsigned_32;
      Preempt_To_Idle : Boolean; ID : out Unsigned_32; Accepted : out Boolean;
      Session : Unsigned_64 := 0) is
      First, Last : Unsigned_16;
      Held : Boolean;
   begin
      ID := No_Context; Accepted := False;
      if not Ready (Object) or else Object.Used = Capacity then return; end if;
      if Session /= 0 then
         for I in 1 .. Object.Used loop
            if Object.Session_Owners (I) = Session then return; end if;
         end loop;
      end if;
      Fences.Reserve (Object.Ledger, Fence_Count, First, Last, Held);
      if not Held then return; end if;
      Object.Used := Object.Used + 1;
      Object.Session_Owners (Object.Used) := Session;
      ID := First_ID + Unsigned_32 (Object.Used - 1);
      Routes.Register (Object.Routing, ID, First, Last, Held);
      if not Held then Fail (Object); return; end if;
      Driver.Initialize (Object.Items (Object.Used), ID, GPU_Start, Pin_Bias,
                         First, Last, Quantum_Us, Preemption_Us, Preempt_To_Idle);
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
      Target : Routes.Destination;
      Outcome : Driver.Result;
      Kept : Boolean;
   begin
      ID := No_Context; Status := Transport_Fault;
      if not Ready (Object) then return; end if;
      Target := Routes.Select_Destination (Object.Routing, Payload, Fence);
      case Target.Kind is
         when Routes.Invalid_Message => Fail (Object);
         when Routes.Unclaimed =>
            Retain (Payload, Fence, Kept);
            if Kept then Status := Retained; else Fail (Object); end if;
         when Routes.Context_Message =>
            ID := Target.ID;
            if not Known (Object, ID) then Fail (Object); return; end if;
            Driver.Dispatch (Object.Items (Natural (ID - First_ID) + 1), Payload, Fence, Outcome);
            Status := (if Outcome in Driver.Faulted | Driver.Rejected then
                         Context_Fault else Delivered);
      end case;
      if not Ready (Object) then Status := Transport_Fault; end if;
   end Dispatch;
end Intel_GPU_Context_Table;
