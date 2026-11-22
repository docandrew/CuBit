pragma Ada_2022;
package body ACPI_Field_Scoped_Provider is
   package D renames ACPI_Locked_NVS_Protocol;
   package F renames ACPI_Field_Diagnostics;
   function First_Failure (IO : Context) return F.Report is
     (if F.Present (IO.First) then IO.First else Scope.First_Failure (IO.Guard));
   use type A.Counter, A.Completion, Scope.Depth_Count, W.Time_Instant;
   function Status (IO : Context) return Startup_State is
     (if not IO.Consumed then Fresh elsif IO.Failed then Failed
      elsif Base.Ready (IO.Inner) then Ready else Failed);
   function Identity (IO : Context) return AML_Identity.Identity is (Base.Identity (IO.Inner));
   function Pending (IO : Context) return Boolean is
     (IO.Working or else IO.Failed or else IO.Captured.Present
      or else Scope.Pending (IO.Guard) or else Base.Pending (IO.Inner));
   function Cleanup_Required (IO : Context) return Boolean is
     (Scope.Observe (IO.Guard).Cleanup_Required);
   function Live (IO : Context) return Boolean is
     (Status (IO) = Ready and then not Pending (IO));
   function Current_Ticket (IO : Context) return ACPI_Global_Lock_Protocol.Ticket is
     (if Live (IO) then Scope.Current_Ticket (IO.Guard)
      else ACPI_Global_Lock_Protocol.No_Ticket);
   procedure Read_Field_Context (IO : Context; Requires_Lock, For_Write : Boolean;
      Deadline : out W.Time_Instant; Ticket : out ACPI_Global_Lock_Protocol.Ticket;
      Accepted : out Boolean) is
      Held : constant Boolean := Scope.Observe (IO.Guard).Lease_Usable;
   begin
      Deadline := W.No_Finite_Deadline;
      Ticket := ACPI_Global_Lock_Protocol.No_Ticket; Accepted := False;
      if not Live (IO) or else Scope.Observe (IO.Guard).Depth = 0
        or else IO.Startup_Deadline in 0 | W.No_Finite_Deadline
        or else (Held and then For_Write) then return; end if;
      if Held then
         Ticket := Current_Ticket (IO);
         if not ACPI_Global_Lock_Protocol.Valid (Ticket)
           or else IO.Data_Deadline in 0 | W.No_Finite_Deadline then
            Ticket := ACPI_Global_Lock_Protocol.No_Ticket; return;
         end if;
         Deadline := W.Time_Instant'Min (IO.Startup_Deadline, IO.Data_Deadline);
      elsif Requires_Lock then return;
      else Deadline := IO.Startup_Deadline;
      end if;
      Accepted := True;
   end Read_Field_Context;
   procedure Initialize
     (IO : in out Context; Destination : Peer; Symbol : A.Endpoint_ID;
      Deadline : W.Time_Instant; Accepted : out Boolean) is
   begin
      Accepted := False; if IO.Consumed then return; end if;
      IO.Consumed := True;
      if Destination = No_Peer then IO.Failed := True; return; end if;
      IO.Startup_Deadline := Deadline;
      Scope.Initialize (IO.Guard, Destination, Accepted);
      if Accepted then Initialize_Base (IO.Inner, Destination, Symbol, Deadline, Accepted); end if;
      if not Accepted then IO.Failed := True; end if;
   end Initialize;
   function Register_Count (IO : Context) return Natural is
     (if Live (IO) then Base.Register_Count (IO.Inner) else 0);
   procedure Export_Target
     (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean) is
   begin
      Target := A.No_Target; Accepted := False;
      if Live (IO) then Base.Export_Target (IO.Inner, Ordinal, Target, Accepted); end if;
   end Export_Target;
   procedure Select_Target
     (IO : in out Context; Geometry : AML_Field_Layout.Register_Geometry;
      For_Write : Boolean; Target : out A.Target; Accepted : out Boolean) is
   begin
      Target := A.No_Target; Accepted := False;
      if Live (IO) then Base.Select_Target (IO.Inner, Geometry, For_Write, Target, Accepted); end if;
   end Select_Target;
   procedure Prepare_Target
     (IO : in out Context; Geometry : AML_Field_Layout.Register_Geometry;
      For_Write : Boolean; Deadline : W.Time_Instant; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Live (IO) or else Scope.Observe (IO.Guard).Lease_Usable then return; end if;
      IO.Working := True;
      Prepare_Base (IO.Inner, Geometry, For_Write, Deadline, Accepted);
      IO.Working := False;
   end Prepare_Target;
   procedure Resolve_Module_Target
     (IO : in out Context; Geometry : AML_Field_Layout.Register_Geometry;
      Target : in out A.Target; Accepted : in out Boolean) is
   begin
      Target := A.No_Target; Accepted := False;
      if Live (IO) then Base.Resolve_Module_Target (IO.Inner, Geometry, Target, Accepted); end if;
   end Resolve_Module_Target;
   procedure Describe
     (IO : Context; Target : A.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (others => <>); Accepted := False;
      if Live (IO) then Base.Describe (IO.Inner, Target, Geometry, Accepted); end if;
   end Describe;
   procedure Authorize
     (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
      ID : D.Record_ID;
      Datum : D.Geometry;
   begin
      Accepted := False; if not Live (IO) then return; end if;
      if Scope.Observe (IO.Guard).Lease_Usable then
         if not For_Write then Describe_Scoped_Read (IO.Inner, Target, ID, Datum, Accepted); end if;
      else Base.Authorize (IO.Inner, Target, For_Write, Accepted);
      end if;
   end Authorize;
   procedure Begin_Field
     (IO : in out Context; Requires_Lock, For_Write : Boolean; Accepted : in out Boolean) is
      Lease_Until : Interfaces.Unsigned_64 := 0;
      IPC_Until : W.Time_Instant := W.No_Finite_Deadline;
      Available : Boolean := True;
   begin
      -- The default Core value is False for Lock Fields; this actual is the
      -- trusted implementation that can explicitly accept after acquisition.
      Accepted := False; if not Live (IO) then return; end if;
      IO.Working := True;
      if Requires_Lock and then not Scope.Observe (IO.Guard).Lease_Usable then
         Begin_Window (Lease_Until, IPC_Until, Available);
      end if;
      -- Unsupported locked writes refuse without acquiring any hardware lock.
      if not Available or else (Requires_Lock and then For_Write) then
         IO.Failed := True; IO.Working := False; return;
      end if;
      Scope.Begin_Field (IO.Guard, IO.Channel, Requires_Lock, For_Write,
        Lease_Until, IPC_Until, Acquisition_Budget, Accepted);
      if not Accepted then IO.Failed := True;
      elsif Requires_Lock and then IPC_Until /= W.No_Finite_Deadline then IO.Data_Deadline := IPC_Until;
      end if;
      IO.Working := False;
   end Begin_Field;
   procedure End_Field (IO : in out Context; Accepted : in out Boolean) is
      Deadline : W.Time_Instant := W.No_Finite_Deadline;
   begin
      Accepted := False; if IO.Working then return; end if;
      IO.Working := True; Cleanup_Deadline (Deadline);
      Scope.End_Field (IO.Guard, IO.Channel, Deadline, Accepted);
      if not Accepted then IO.Failed := True; end if;
      IO.Working := False; Accepted := Accepted and then not IO.Failed;
   end End_Field;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Accepted : Boolean;
      ID : D.Record_ID;
      Datum : D.Geometry;
      Value : Interfaces.Unsigned_64;
      Deadline : W.Time_Instant := W.No_Finite_Deadline;
   begin
      Reply := (others => <>);
      Authorize (IO, Item.Destination, Item.For_Write, Accepted);
      if not Accepted or else IO.Issued = A.Counter'Last then return; end if;
      IO.Issued := IO.Issued + 1; IO.Working := True;
      if Scope.Observe (IO.Guard).Lease_Usable then
         if Item.Value /= 0 then IO.Working := False; return; end if;
         Describe_Scoped_Read (IO.Inner, Item.Destination, ID, Datum, Accepted);
         if Accepted then
            Read_Deadline (Deadline);
            if Deadline /= W.No_Finite_Deadline then
               Deadline := W.Time_Instant'Min (Deadline, IO.Data_Deadline);
            end if;
            Scope.Read (IO.Guard, IO.Channel, ID, Datum, Deadline, Value, Accepted);
         end if;
         if Accepted then Reply := (A.Completed, Identity (IO), Value, A.No_Operation);
         else
            IO.Captured := (True, Identity (IO), Item, A.Operation_ID (IO.Issued));
            Reply := (A.Indeterminate, Identity (IO), 0, IO.Captured.Operation);
         end if;
      else
         Base.Dispatch (IO.Inner, Item, Reply);
         if Reply.Status = A.Indeterminate then
            IO.Captured := (True, Identity (IO), Item, A.Operation_ID (IO.Issued));
            Reply.Operation := IO.Captured.Operation;
         elsif Reply.Status = A.Failed_Completed then
            IO.Fault := (True, Identity (IO), Item, A.No_Operation);
         end if;
      end if;
      IO.Working := False;
      if Reply.Status /= A.Completed then
         if Reply.Status = A.Indeterminate and then not F.Present (Scope.First_Failure (IO.Guard)) then
            F.Remember (IO.First, (Reason => F.Provider_Uncertain, At_Point => F.Provider_Dispatch, others => <>));
         end if;
         IO.Failed := True;
      end if;
   end Dispatch;
   function Uncertain (IO : Context) return A.Observation is (IO.Captured);
   function Completed_Fault (IO : Context) return A.Observation is (IO.Fault);
   procedure Reconciled
     (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO, Expected);
   begin Accepted := False; end Reconciled;
   procedure Cleanup (IO : in out Context; Completed : out Boolean) is
      Deadline : W.Time_Instant := W.No_Finite_Deadline;
      Result : Scope.Cleanup_Outcome;
   begin
      Completed := False; if IO.Working then return; end if;
      -- A fresh owner has never initialized its lease scope or acquired any
      -- authority. Observer-only shutdown needs no native clock or IPC.
      if not IO.Consumed then Completed := True; return; end if;
      IO.Working := True; Cleanup_Deadline (Deadline);
      Scope.Cleanup (IO.Guard, IO.Channel, Deadline, Result);
      Completed := Scope.Observe (IO.Guard).Depth = 0 and then not Cleanup_Required (IO);
      IO.Working := False;
   end Cleanup;
end ACPI_Field_Scoped_Provider;
