with Ada.Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_GuC_Fast_Fences;
with Intel_GPU_GuC_Submission_Policy;
package body Intel_GPU_Context_Table is
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   package Credits renames Intel_GPU_G2H_Credits;
   use type Life.Phase;
   use type Life.Operation;
   use type Driver.Result;
   Record_Bytes : constant Unsigned_64 :=
     Unsigned_64 ((Record_Array'Component_Size + System.Storage_Unit - 1) /
                  System.Storage_Unit);
   -- Answer words a reply-bearing request reserves before it is sent.
   Answer_Words : constant := Intel_GPU_GuC_Submission_Policy.Reply_Credit_Words;
   function Storage_Bytes (Count : Context_Count) return Unsigned_64 is
     (Unsigned_64 (Count) * Record_Bytes);
   procedure Attach (Object : in out Table; Storage : System.Address) is
      function To_Records is new Ada.Unchecked_Conversion (System.Address, Record_Access);
   begin
      if Object.Items /= null or else Object.Count /= 0 or else
        System."=" (Storage, System.Null_Address) then return; end if;
      Object.Items := To_Records (Storage);
      if not Object.Configured then
         Credits.Configure (Object.Credits, Reply_Words);
         Object.Configured := True;
      end if;
   end Attach;
   function Grown (Object : Table) return Context_Count is (Object.Count);
   -- Zero the record and apply its default values in place: records hold
   -- limited driver state and are never copied.
   procedure Initialize_At (Where : System.Address) is
      Memory : Storage_Array (1 .. Storage_Offset (Record_Bytes)) with Import, Address => Where;
   begin
      Memory := [others => 0];
      declare
         Fresh : Context_Record with Address => Where;
         pragma Warnings (Off, Fresh);
      begin
         null;
      end;
   end Initialize_At;
   procedure Initialize (Object : in out Table; I : Ordinal) is
   begin
      Initialize_At (Object.Items (I)'Address);
   end Initialize;
   procedure Grow (Object : in out Table; To : Context_Count; Accepted : out Boolean) is
   begin
      Accepted := Object.Items /= null and then To >= Object.Count;
      if not Accepted then return; end if;
      for I in Object.Count + 1 .. To loop
         Initialize (Object, I);
      end loop;
      Object.Count := To;
   end Grow;
   function In_Use (Object : Table; ID : Unsigned_32) return Boolean is (Known (Object, ID));
   function Session_Of (Object : Table; ID : Unsigned_32) return Unsigned_64 is
     (if Known (Object, ID) then Object.Items (Index (ID)).Owner else 0);
   function Failed (Object : Table) return Boolean is (Object.Broken);
   function Reply_Words_In_Use (Object : Table) return Natural is
     (Credits.In_Use (Object.Credits));
   function State (Object : Table; ID : Unsigned_32) return Life.Phase is
     (if Known (Object, ID) then Driver.State (Object.Items (Index (ID)).Item)
      else Life.Fresh);
   function Can_Run_And_Retire (Object : Table; ID : Unsigned_32) return Boolean is
     (not Object.Broken and then Owner_Ready and then Known (Object, ID) and then
      not Object.Items (Index (ID)).Retired and then
      not Object.Items (Index (ID)).Work_Held and then
      Driver.Can_Run_And_Retire (Object.Items (Index (ID)).Item));
   function Can_Submit (Object : Table; ID : Unsigned_32) return Boolean is
     (not Object.Broken and then Owner_Ready and then Known (Object, ID) and then
      not Object.Items (Index (ID)).Retired and then
      not Object.Items (Index (ID)).Work_Held and then
      Driver.Can_Submit (Object.Items (Index (ID)).Item));
   function Session_Context
     (Object : Table; ID : Unsigned_32; Session : Unsigned_64) return Unsigned_32 is
     (if Session = 0 or else Object.Broken or else not Owner_Ready or else
         not Known (Object, ID) or else Object.Items (Index (ID)).Owner /= Session or else
         Object.Items (Index (ID)).Retired or else
         Driver.State (Object.Items (Index (ID)).Item) = Life.Quarantined
      then No_Context else ID);
   procedure Retire_Session
     (Object : in out Table; ID : Unsigned_32; Session : Unsigned_64) is
   begin
      if Session /= 0 and then Known (Object, ID) and then
        Object.Items (Index (ID)).Owner = Session
      then
         Object.Items (Index (ID)).Retired := True;
      end if;
   end Retire_Session;
   -- Bring the pool in line with what context I now holds. A quarantined
   -- context never gives credit back: its answer may still arrive.
   procedure Account (Object : in out Table; I : Ordinal) is
      Item : Context_Record renames Object.Items (I);
      Held : constant Natural := Driver.Credits_Held (Item.Item);
      Accepted : Boolean;
   begin
      if Held = Item.Charged or else
        (Driver.State (Item.Item) = Life.Quarantined and then Held < Item.Charged)
      then return; end if;
      Credits.Exchange (Object.Credits, Item.Charged, Held, Accepted);
      if Accepted then
         Item.Charged := Held;
      else
         -- Accounting lost track: no answer can be trusted to fit.
         Fail (Object);
      end if;
   end Account;
   procedure Fail (Object : in out Table) is
   begin
      Object.Broken := True;
      if Object.Items = null then return; end if;
      for I in 1 .. Object.Count loop
         if Object.Items (I).Opened then Driver.Fail (Object.Items (I).Item); end if;
      end loop;
   end Fail;
   function Ready (Object : in out Table) return Boolean is
   begin
      if not Owner_Ready then Fail (Object); end if;
      return not Object.Broken;
   end Ready;
   function Work_Allowed (Object : Table; ID : Unsigned_32) return Boolean is
     (not Object.Broken and then Owner_Ready and then Known (Object, ID)
      and then not Object.Items (Index (ID)).Retired
      and then not Object.Items (Index (ID)).Work_Held
      and then State (Object, ID) = Life.Enabled);
   function Publish_Allowed (Object : Table; ID : Unsigned_32) return Boolean is
     (not Object.Broken and then Owner_Ready and then Known (Object, ID)
      and then not Object.Items (Index (ID)).Retired
      and then not Object.Items (Index (ID)).Work_Held
      and then State (Object, ID) in Life.Enabled | Life.Disabled);
   procedure Hold_Work
     (Object : in out Table; ID : Unsigned_32; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Ready (Object) or else not Known (Object, ID) then return; end if;
      declare Item : Context_Record renames Object.Items (Index (ID)); begin
         if Item.Retired or else Item.Work_Held or else
           State (Object, ID) not in Life.Enabled | Life.Disabled then return; end if;
         Item.Work_Held := True;
         Accepted := True;
      end;
   end Hold_Work;
   procedure Release_Work
     (Object : in out Table; ID : Unsigned_32; Accepted : out Boolean;
      Keep_Disabled : Boolean := False) is
   begin
      Accepted := False;
      if not Ready (Object) or else not Known (Object, ID) then return; end if;
      declare Item : Context_Record renames Object.Items (Index (ID)); begin
         if Item.Retired or else not Item.Work_Held or else
           State (Object, ID) /=
             (if Keep_Disabled then Life.Disabled else Life.Enabled)
         then return; end if;
         Item.Work_Held := False;
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
      declare I : constant Ordinal := Index (ID); begin
         if not Object.Items (I).Retired or else Object.Items (I).Work_Held or else
           not Work_Drained or else State (Object, ID) /= Life.Disabled
         then return; end if;
         if Credits.Available (Object.Credits) < Answer_Words then
            Status := Driver.Backpressure; return;
         end if;
         Driver.Deregister (Object.Items (I).Item, Admission_Closed => True,
                            Work_Drained => True, Status => Status);
         Account (Object, I);
         if not Ready (Object) then Status := Driver.Faulted; end if;
      end;
   end Deregister_Retired;
   procedure Open
     (Object : in out Table; ID : Unsigned_32; GPU_Start, Pin_Bias : Unsigned_64;
      Quantum_Us, Preemption_Us : Unsigned_32;
      Preempt_To_Idle : Boolean; Accepted : out Boolean;
      Session : Unsigned_64 := 0) is
   begin
      Accepted := False;
      if not Ready (Object) or else Object.Items = null or else ID < First_ID or else
        ID - First_ID >= Unsigned_32 (Object.Count) or else Known (Object, ID)
      then return; end if;
      if Session /= 0 then
         for I in 1 .. Object.Count loop
            if Object.Items (I).Opened and then Object.Items (I).Owner = Session then
               return;
            end if;
         end loop;
      end if;
      declare I : constant Ordinal := Index (ID); begin
         Object.Items (I).Opened := True;
         Object.Items (I).Owner := Session;
         Driver.Initialize (Object.Items (I).Item, ID, GPU_Start, Pin_Bias,
                            Quantum_Us, Preemption_Us, Preempt_To_Idle);
      end;
      if not Ready (Object) then return; end if;
      Accepted := State (Object, ID) = Life.Ready;
   end Open;
   function Releasable (Object : Table; ID : Unsigned_32) return Boolean is
     (not Object.Broken and then Known (Object, ID) and then
      Object.Items (Index (ID)).Retired and then not Object.Items (Index (ID)).Work_Held and then
      Object.Items (Index (ID)).Charged = 0 and then
      Driver.State (Object.Items (Index (ID)).Item) in Life.Deregistered | Life.Ready | Life.Fresh);
   procedure Release (Object : in out Table; ID : Unsigned_32; Accepted : out Boolean) is
   begin
      Accepted := Releasable (Object, ID);
      if Accepted then Initialize (Object, Index (ID)); end if;
   end Release;
   procedure Submit
     (Object : in out Table; ID : Unsigned_32; Action : Life.Operation;
      Status : out Driver.Result) is
   begin
      Status := Driver.Faulted;
      if not Ready (Object) then return; end if;
      Status := Driver.Rejected;
      if not Known (Object, ID) then return; end if;
      declare I : constant Ordinal := Index (ID); begin
         if Object.Items (I).Retired and then Action /= Life.Disable then return; end if;
         if Action in Life.Enable | Life.Disable and then
           Credits.Available (Object.Credits) < Answer_Words
         then
            Status := Driver.Backpressure; return;
         end if;
         Driver.Submit (Object.Items (I).Item, Action, Status);
         Account (Object, I);
      end;
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
      Driver.Notify_Work (Object.Items (Index (ID)).Item, Tail_Published, Status);
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
         Driver.Dispatch (Object.Items (Index (ID)).Item, Payload, Fence, Outcome);
         Account (Object, Index (ID));
         Status := (if Outcome in Driver.Faulted | Driver.Rejected then
                      Context_Fault else Delivered);
      else
         Retain (Payload, Fence, Kept);
         if Kept then Status := Retained; else Fail (Object); end if;
      end if;
      if not Ready (Object) then Status := Transport_Fault; end if;
   end Dispatch;
end Intel_GPU_Context_Table;
