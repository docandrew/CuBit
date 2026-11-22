pragma Ada_2022;
with ACPI_Capacity_Growth;
with System.Storage_Elements;
package body ACPI_Firmware_Data_Startup is
   package F renames RPC.F;
   package D renames ACPI_Capacity_Diagnostics;
   use type A.Endpoint_ID, A.Completion, IPC.Time_Instant, Client.Outcome;
   use type ACPI_Global_Lock_Protocol.Ticket;
   Cleanup_Milliseconds : constant IPC.Time_Instant := 25;
   function Ready return Boolean is
      (Initialized and then not Working and then not Stopped and then not Clock_Broken and then Provider.Ready (Owner));
   function Session_Pending (IO : Client.Context) return Boolean is
      R : constant Client.Statistics := Client.Observe (IO);
   begin return R.Working or else R.Failed or else (R.Depth = 0 and then R.Cleanup_Due); end Session_Pending;
   Maximum_Diagnostic_Records : constant Positive := 8;
   subtype Diagnostic_Index is Positive range 1 .. Maximum_Diagnostic_Records;
   type Diagnostic_Operation is (Publishing_Data, Opening_Data, Accessing_Data, Closing_Data, Cleaning_Data);
   type Diagnostic_Record is record
      Operation : Diagnostic_Operation := Publishing_Data;
      Result : Client.Outcome := Client.Not_Ready;
      Record_ID, Offset, Value : F.Word := 0;
      Depth : Natural := 0;
      Failed, Cleanup_Due : Boolean := False;
   end record;
   Trace : array (Diagnostic_Index) of Diagnostic_Record;
   Next_Record : Diagnostic_Index := Diagnostic_Index'First;
   Recorded : Natural range 0 .. Maximum_Diagnostic_Records := 0;
   First_Failure : Diagnostic_Record;
   Have_Failure : Boolean := False;
   function Bit (Value : Boolean) return String is (if Value then "1" else "0");
   function Name (Value : Diagnostic_Operation) return String is
     (case Value is when Publishing_Data => "PUBLISH",when Opening_Data => "BEGIN",
      when Accessing_Data => "DATUM",when Closing_Data => "END",when Cleaning_Data => "CLEANUP");
   function Name (Value : Client.Outcome) return String is
     (case Value is when Client.Ready => "READY",when Client.Completed => "COMPLETED",
      when Client.Denied => "DENIED",when Client.Invalid_Request => "INVALID_REQUEST",
      when Client.Not_Ready => "NOT_READY",when Client.Allocation_Refused => "ALLOCATION_REFUSED",
      when Client.Exhausted => "EXHAUSTED",when Client.Pending => "PENDING",when Client.Failed => "FAILED",
      when Client.Closed => "CLOSED",when Client.Reentered => "REENTERED");
   procedure Remember (Operation : Diagnostic_Operation; IO : Client.Context;
      Result : Client.Result; Record_ID : F.Identifier := F.No_ID; Offset : F.Word := 0) is
      Stats : constant Client.Statistics := Client.Observe (IO);
      Item : constant Diagnostic_Record := (Operation,Result.Status,F.Word (Record_ID),Offset,Result.Value,
         Stats.Depth,Stats.Failed,Stats.Cleanup_Due);
   begin
      Trace (Next_Record) := Item;
      if Next_Record = Diagnostic_Index'Last then Next_Record := Diagnostic_Index'First;
      else Next_Record := Next_Record+1; end if;
      if Recorded < Maximum_Diagnostic_Records then Recorded := Recorded+1; end if;
      if not Have_Failure and then Result.Status not in Client.Ready | Client.Completed | Client.Closed then
         First_Failure := Item; Have_Failure := True;
      end if;
   end Remember;
   Publication : Client.Publication_Diagnostic;
   function Publication_Summary return String is
      (" publish-attempt=" & Bit (Publication.Present) &
       " publish-transport=" & (if Publication.Present then Publication.Transport'Image else "NONE") &
       " publish-wire=" & (if Publication.Has_Remote then Publication.Remote'Image else "NONE") &
       " publish-until-ms=" & Publication.Deadline'Image);
   function Diagnostic_Summary return String is
     ("initialized=" & Bit (Initialized) & " stopped=" & Bit (Stopped) &
      " clock-broken=" & Bit (Clock_Broken) & " working=" & Bit (Working) &
      " provider-ready=" & Bit (Provider.Ready (Owner)) & " provider-pending=" & Bit (Provider.Pending (Owner)) &
      " startup-until-ms=" & Startup_Until'Image & " retained-clock-ms=" & Last_Clock'Image &
      " first-failure=" & (if Have_Failure then Name (First_Failure.Operation) & ":" & Name (First_Failure.Result) else "NONE") & Publication_Summary);
   function Diagnostic_Line (Ordinal : Positive) return String is
      Index : Diagnostic_Index;
   begin
      if Ordinal > Recorded then return ""; end if;
      Index := Diagnostic_Index (((Natural (Next_Record)-1+Maximum_Diagnostic_Records-Recorded+Ordinal-1)
         mod Maximum_Diagnostic_Records)+1);
      declare Item : constant Diagnostic_Record := Trace (Index); begin
         return "op=" & Name (Item.Operation) & " result=" & Name (Item.Result) &
           " record=" & Item.Record_ID'Image & " offset=" & Item.Offset'Image &
           " value=" & Item.Value'Image & " depth=" & Item.Depth'Image &
           " failed=" & Bit (Item.Failed) & " cleanup=" & Bit (Item.Cleanup_Due);
      end;
   end Diagnostic_Line;
   procedure Publish (IO : in out Client.Context; Region : F.L.Region; Deadline : IPC.Time_Instant;
      Record_ID : out F.Identifier; Status : out A.Completion) is
      R : Client.Result;
   begin
      Client.Publish (IO,Region,Deadline,R); Publication := Client.Observe_Publication (IO); Remember (Publishing_Data,IO,R,R.ID); Record_ID := R.ID;
      Status := (case R.Status is when Client.Ready => A.Completed,
         when Client.Pending => A.Indeterminate, when others => A.Denied);
   end Publish;
   procedure Begin_Data (IO : in out Client.Context; Requires_Lock : Boolean;
      Deadline : IPC.Time_Instant; Domain, Sequence : F.Word; Accepted : out Boolean) is
      R : Client.Result;
   begin Client.Begin_Field (IO,Requires_Lock,Deadline,Domain,Sequence,R); Remember (Opening_Data,IO,R); Accepted := R.Status = Client.Ready; end Begin_Data;
   procedure Datum (IO : in out Client.Context; Record_ID : F.Identifier; Offset : F.Word;
      Width : F.T.Access_Width; For_Write : Boolean; Input : F.Word;
      Value : out F.Word; Status : out A.Completion) is
      R : Client.Result;
   begin
      Client.Access_Datum (IO,Record_ID,Offset,Width,For_Write,Input,R); Remember (Accessing_Data,IO,R,Record_ID,Offset); Value := R.Value;
      Status := (case R.Status is when Client.Completed => A.Completed,
         when Client.Pending => A.Indeterminate, when others => A.Denied);
   end Datum;
   procedure End_Data (IO : in out Client.Context; Deadline : IPC.Time_Instant; Accepted : out Boolean) is
      R : Client.Result;
   begin Client.End_Field (IO,Deadline,R); Remember (Closing_Data,IO,R); Accepted := R.Status = Client.Closed; end End_Data;
   procedure Stop_Data (IO : in out Client.Context; Deadline : IPC.Time_Instant; Accepted : out Boolean) is
      R : Client.Result;
   begin Client.Cleanup (IO,Deadline,R); Remember (Cleaning_Data,IO,R); Accepted := R.Status = Client.Closed; end Stop_Data;
   procedure Publication_Deadline (Deadline : out IPC.Time_Instant) is
   begin
      Deadline := (if Initialized and then Working and then Phase = Resolving
                   then Startup_Until else IPC.No_Finite_Deadline);
   end Publication_Deadline;
   procedure Field_Context (Requires_Lock, For_Write : Boolean; Deadline : out IPC.Time_Instant;
      Domain, Sequence : out F.Word; Accepted : out Boolean) is
   begin
      Deadline := IPC.No_Finite_Deadline; Domain := 0; Sequence := 0;
      Accepted := Initialized and then Working and then Phase = Beginning
         and then not Stopped and then not Clock_Broken
         and then (not Requires_Lock or else ACPI_Global_Lock_Protocol.Valid (Field_Ticket))
         and then not (For_Write and then ACPI_Global_Lock_Protocol.Valid (Field_Ticket));
      if Accepted then
         Deadline := Field_Until; Domain := Field_Ticket.Domain; Sequence := Field_Ticket.Sequence;
      end if;
   end Field_Context;
   procedure Cleanup_Deadline (Deadline : out IPC.Time_Instant) is
      Now : IPC.Time_Instant := 0;
      Available : Boolean := False;
   begin
      Deadline := IPC.No_Finite_Deadline;
      Read_Clock (Now,Available);
      if not Available then return; end if;
      if Now < Last_Clock then Clock_Broken := True; Stopped := True; return; end if;
      if Clock_Broken or else Now >= IPC.No_Finite_Deadline - Cleanup_Milliseconds then return; end if;
      Last_Clock := Now; Deadline := Now + Cleanup_Milliseconds;
   end Cleanup_Deadline;
   procedure Session_Capacity (IO : Client.Context; Capacity : in out D.Report) is
      S : constant Client.Capacity_Report := Client.Observe_Capacity (IO);
   begin
      Capacity := (Cache => (case S.Cache is when Client.No_Cache => D.No_Cache,
         when Client.Region_Cache => D.Firmware_Regions, when Client.Frame_Cache => D.Firmware_Frames,
         when Client.Descriptor_Cache => D.Firmware_Descriptors),
         Failure => (case S.Problem is when Client.No_Capacity_Failure => D.No_Failure,
            when Client.Process_Heap_Limit => D.Process_Heap,
            when Client.Entry_Limit => D.Entry_Representation, when Client.Byte_Limit => D.Byte_Representation),
         Demand => D.Entry_Count (S.Demand),Capacity => D.Entry_Count (S.Capacity),
         Entry_Limit => D.Entry_Count (ACPI_Capacity_Growth.Count'Last),
         Requested_Bytes => D.Byte_Count (S.Requested_Bytes),
         Byte_Limit => D.Byte_Count (System.Storage_Elements.Storage_Count'Last));
   end Session_Capacity;
   function Capacity_Report return D.Report is
      P : constant Provider.Capacity_Report := Provider.Observe_Capacity (Owner);
      use type Provider.Capacity_Problem;
   begin
      if P.Problem = Provider.No_Capacity_Failure then return Provider.Session_Capacity_Report (Owner); end if;
      return (Cache => D.Firmware_Targets,
         Failure => (case P.Problem is when Provider.No_Capacity_Failure => D.No_Failure,
            when Provider.Process_Heap_Limit => D.Process_Heap,
            when Provider.Entry_Limit => D.Entry_Representation, when Provider.Byte_Limit => D.Byte_Representation),
         Demand => D.Entry_Count (P.Demand),Capacity => D.Entry_Count (P.Capacity),
         Entry_Limit => D.Entry_Count (ACPI_Capacity_Growth.Count'Last),
         Requested_Bytes => D.Byte_Count (P.Requested_Bytes),
         Byte_Limit => D.Byte_Count (System.Storage_Elements.Storage_Count'Last));
   end Capacity_Report;
   procedure Initialize (Destination : Endpoint; Symbol : A.Endpoint_ID;
      Deadline : IPC.Time_Instant; Accepted : in out Boolean) is
      Now : IPC.Time_Instant := 0;
      Available : Boolean := False;
   begin
      Accepted := False; if Consumed or else Working or else Stopped then return; end if;
      Consumed := True; Working := True; Phase := Initializing;
      if Destination = No_Endpoint or else Symbol = A.No_Endpoint or else Deadline in 0 | IPC.No_Finite_Deadline then
         Stopped := True; Working := False; Phase := Idle; return;
      end if;
      Read_Clock (Now,Available);
      if not Available or else Now >= Deadline then Stopped := True; Working := False; Phase := Idle; return; end if;
      Startup_Until := Deadline; Last_Clock := Now;
      Initialize_Connection (Connection,Destination,Accepted);
      if Accepted then Provider.Initialize (Owner,Symbol,Accepted); end if;
      Initialized := Accepted; Stopped := not Accepted; Working := False; Phase := Idle;
   end Initialize;
   procedure Inspect (Is_Ready, Is_Pending : in out Boolean;
      Observation : in out A.Observation; Capacity : in out D.Report) is
   begin
      Is_Ready := Ready;
      Is_Pending := Working or else Stopped or else Clock_Broken or else Provider.Pending (Owner);
      Observation := Provider.Uncertain (Owner); Capacity := Capacity_Report;
   end Inspect;
   procedure Describe (Target : A.Target; Geometry : in out L.Register_Geometry; Accepted : in out Boolean) is
   begin
      Geometry := (others => <>); Accepted := False;
      if Ready then Provider.Describe (Owner,Target,Geometry,Accepted); end if;
   end Describe;
   procedure Authorize (Target : A.Target; For_Write : Boolean; Accepted : in out Boolean) is
   begin Accepted := False; if Ready then Provider.Authorize (Owner,Target,For_Write,Accepted); end if; end Authorize;
   procedure Dispatch (Item : A.Request; Reply : in out A.Response) is
   begin
      Reply := (others => <>); if not Ready then return; end if;
      Working := True; Phase := Dispatching;
      Provider.Dispatch (Owner,Item,Reply);
      if Reply.Status /= A.Completed then Stopped := True; end if;
      Working := False; Phase := Idle;
   end Dispatch;
   procedure Resolve (Item : Declaration; Geometry : L.Register_Geometry;
      Target : in out A.Target; Accepted : in out Boolean) is
   begin
      Target := A.No_Target; Accepted := False; if not Ready then return; end if;
      Working := True; Phase := Resolving;
      Provider.Resolve_Startup_Firmware_Target (Owner,Item,Geometry,Target,Accepted);
      if Provider.Pending (Owner) then Stopped := True; end if;
      Working := False; Phase := Idle;
   end Resolve;
   procedure Begin_Field (Requires_Lock, For_Write : Boolean; Deadline : IPC.Time_Instant;
      Ticket : ACPI_Global_Lock_Protocol.Ticket; Accepted : in out Boolean) is
   begin
      Accepted := False; if not Ready then return; end if;
      if Deadline in 0 | IPC.No_Finite_Deadline
         or else (Ticket /= ACPI_Global_Lock_Protocol.No_Ticket and then not ACPI_Global_Lock_Protocol.Valid (Ticket))
         or else (Requires_Lock and then not ACPI_Global_Lock_Protocol.Valid (Ticket))
         or else (For_Write and then ACPI_Global_Lock_Protocol.Valid (Ticket)) then Stopped := True; return; end if;
      Field_Until := IPC.Time_Instant'Min (Startup_Until,Deadline); Field_Ticket := Ticket;
      Working := True; Phase := Beginning;
      Provider.Begin_Field (Owner,Requires_Lock,For_Write,Accepted);
      if not Accepted then Stopped := True; end if;
      Working := False; Phase := Idle;
   end Begin_Field;
   procedure End_Field (Accepted : in out Boolean) is
   begin
      Accepted := False; if Working or else not Initialized then return; end if;
      Working := True; Phase := Ending;
      Provider.End_Field (Owner,Accepted);
      if not Accepted then Stopped := True; end if;
      Working := False; Phase := Idle; Accepted := Accepted and then not Stopped;
   end End_Field;
   procedure Cleanup (Accepted : in out Boolean) is
   begin
      Accepted := False; if Working then return; end if;
      Stopped := True;
      if not Initialized then Accepted := True; return; end if;
      Working := True; Phase := Cleaning;
      Provider.Cleanup (Owner,Accepted);
      Working := False; Phase := Idle;
   end Cleanup;
   function Completed_Fault return A.Observation is (Provider.Completed_Fault (Owner));
end ACPI_Firmware_Data_Startup;
