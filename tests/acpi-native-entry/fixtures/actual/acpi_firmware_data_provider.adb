pragma Ada_2022;
with AML_Decode;
with AML_Identity.Issuer;
with ACPI_Firmware_Data_Identities;
package body ACPI_Firmware_Data_Provider is
   use type A.Endpoint_ID, A.Register_ID, A.Completion;
   use type AML_Identity.Identity, F.Identifier, F.Word, L.Register_Geometry;
   use type AML_Decode.Byte;
   use type Store.Outcome;
   function Identity (IO : Context) return AML_Identity.Identity is (IO.Token);
   function Pending (IO : Context) return Boolean is
      (IO.Working or else IO.Failed or else IO.Captured.Present or else Session_Pending (IO.Inner));
   function Ready (IO : Context) return Boolean is (IO.Initialized and then not Pending (IO));
   function Uncertain (IO : Context) return A.Observation is (IO.Captured);
   function Completed_Fault (IO : Context) return A.Observation is (IO.Fault);
   function Observe_Capacity (IO : Context) return Capacity_Report is
      R : constant Store.Report := Store.Observe (IO.Rows);
   begin
      return ((case R.Status is when Store.Stored => No_Capacity_Failure,
         when Store.Allocation_Refused => Process_Heap_Limit,
         when Store.Count_Representation_Exhausted => Entry_Limit,
         when Store.Byte_Representation_Exhausted => Byte_Limit),R.Demand,F.Word (R.Capacity),R.Requested_Bytes);
   end Observe_Capacity;
   function Session_Capacity_Report (IO : Context) return ACPI_Capacity_Diagnostics.Report is
      Result : ACPI_Capacity_Diagnostics.Report := ACPI_Capacity_Diagnostics.No_Report;
   begin Inspect_Session_Capacity (IO.Inner,Result); return Result; end Session_Capacity_Report;
   procedure Initialize (IO : in out Context; Symbol : A.Endpoint_ID; Accepted : out Boolean) is
   begin
      Accepted := False; if IO.Consumed then return; end if;
      IO.Consumed := True;
      if Symbol = A.No_Endpoint then IO.Failed := True; return; end if;
      AML_Identity.Issuer.Issue (IO.Token,Accepted);
      if Accepted then ACPI_Firmware_Data_Identities.Issue (IO.Epoch,Accepted); end if;
      if Accepted then IO.Symbol := Symbol; IO.Initialized := True; else IO.Failed := True; end if;
   end Initialize;
   function Register_Count (IO : Context) return Natural is
      (if Ready (IO) then Store.Length (IO.Rows) else 0);
   function At_Index (IO : Context; Index : Positive) return A.Target is
      ((IO.Token,IO.Symbol,A.Counter (IO.Epoch),A.Register_ID (Index)));
   function Index_Of (IO : Context; Target : A.Target) return Natural is
   begin
      if not Ready (IO) or else Target.Provider /= IO.Token or else Target.Endpoint /= IO.Symbol
        or else Target.Epoch /= A.Counter (IO.Epoch) or else Target.Register = A.No_Register
        or else A.Counter (Target.Register) > A.Counter (Store.Length (IO.Rows)) then return 0; end if;
      declare Index : constant Positive := Positive (Target.Register); begin
         return (if Store.Read (IO.Rows,Index).Ready then Index else 0);
      end;
   end Index_Of;
   procedure Export_Target (IO : Context; Ordinal : Positive; Target : out A.Target; Accepted : out Boolean) is
   begin
      Target := A.No_Target; Accepted := False;
      if not Ready (IO) or else Ordinal > Store.Length (IO.Rows) then return; end if;
      if Store.Read (IO.Rows,Ordinal).Ready then Target := At_Index (IO,Ordinal); Accepted := True; end if;
   end Export_Target;
   function Geometry_Valid (Item : L.Register_Geometry) return Boolean is
      (Item.Region.Space = L.Memory_Space and then not Item.Region.PCI.Known
       and then F.L.Valid ((Item.Region.Base,Item.Region.Length))
       and then Item.Byte_Offset < Item.Region.Length
       and then F.Word (F.T.Octets (Item.Width)) <= Item.Region.Length - Item.Byte_Offset
       and then F.T.Aligned (Item.Region.Base + Item.Byte_Offset,Item.Width));
   procedure Resolve_Startup_Firmware_Target (IO : in out Context; Item : Declaration;
      Geometry : L.Register_Geometry; Target : in out A.Target; Accepted : in out Boolean) is
      Entry_Data : Association;
      Index : Natural := 0;
      Stored : Store.Outcome;
      Deadline : F.IPC.Time_Instant;
      Status : A.Completion;
   begin
      Target := A.No_Target; Accepted := False;
      if not Ready (IO) or else not Geometry_Valid (Geometry) then return; end if;
      IO.Working := True;
      if not Original_Declaration (Item,Geometry) then IO.Working := False; return; end if;
      for I in 1 .. Store.Length (IO.Rows) loop
         Entry_Data := Store.Read (IO.Rows,I);
         if Entry_Data.Geometry = Geometry and then Entry_Data.Ready then
            Target := At_Index (IO,I); Accepted := True; IO.Working := False; return;
         end if;
      end loop;
      Entry_Data := (Geometry => Geometry,others => <>);
      Store.Append (IO.Rows,Entry_Data,Index,Stored);
      if Stored /= Store.Stored then IO.Working := False; return; end if;
      Publish_Deadline (Deadline);
      Publish (IO.Inner,(Geometry.Region.Base,Geometry.Region.Length),Deadline,Entry_Data.Record_ID,Status);
      if Status = A.Completed and then Entry_Data.Record_ID /= F.No_ID then
         Entry_Data.Ready := True; Store.Replace (IO.Rows,Index,Entry_Data);
         Target := At_Index (IO,Index); Accepted := True;
      else IO.Failed := True;
      end if;
      IO.Working := False;
   end Resolve_Startup_Firmware_Target;
   procedure Describe (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean) is
      Index : constant Natural := Index_Of (IO,Target);
   begin
      Geometry := (others => <>); Accepted := Index /= 0;
      if Accepted then Geometry := Store.Read (IO.Rows,Index).Geometry; end if;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
   begin Accepted := (not For_Write or else Allow_Writes) and then Index_Of (IO,Target) /= 0; end Authorize;
   procedure Begin_Field (IO : in out Context; Requires_Lock, For_Write : Boolean; Accepted : in out Boolean) is
      Deadline : F.IPC.Time_Instant;
      Domain, Sequence : F.Word;
   begin
      Accepted := False;
      if not Ready (IO) or else IO.Depth = Natural'Last
        or else (For_Write and then (not Allow_Writes or else Requires_Lock)) then return; end if;
      IO.Working := True;
      Read_Field_Context (Requires_Lock,For_Write,Deadline,Domain,Sequence,Accepted);
      if Accepted then Begin_Data_Field (IO.Inner,Requires_Lock,Deadline,Domain,Sequence,Accepted); end if;
      if Accepted then IO.Depth := IO.Depth + 1; else IO.Failed := True; end if;
      IO.Working := False;
   end Begin_Field;
   procedure End_Field (IO : in out Context; Accepted : in out Boolean) is
      Deadline : F.IPC.Time_Instant;
   begin
      Accepted := False; if IO.Working or else IO.Depth = 0 then return; end if;
      IO.Working := True; Cleanup_Deadline (Deadline);
      End_Data_Field (IO.Inner,Deadline,Accepted); IO.Depth := IO.Depth - 1;
      if not Accepted then IO.Failed := True; end if;
      IO.Working := False; Accepted := Accepted and then not Pending (IO);
   end End_Field;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Index : constant Natural := Index_Of (IO,Item.Destination);
      Entry_Data : Association;
      Value : F.Word;
      Outer_Operation : F.Identifier;
      Status : A.Completion;
      Accepted : Boolean;
   begin
      Reply := (others => <>);
      if Index = 0 or else IO.Depth = 0 or else (Item.For_Write and then not Allow_Writes) then return; end if;
      Entry_Data := Store.Read (IO.Rows,Index);
      if (if Item.For_Write then Item.Value > F.L.Mask (Entry_Data.Geometry.Width) else Item.Value /= 0) then return; end if;
      ACPI_Firmware_Data_Identities.Issue (Outer_Operation,Accepted); if not Accepted then return; end if;
      IO.Working := True;
      Access_Datum (IO.Inner,Entry_Data.Record_ID,Entry_Data.Geometry.Byte_Offset,
         Entry_Data.Geometry.Width,Item.For_Write,Item.Value,Value,Status);
      if Status = A.Completed and then
        (if Item.For_Write then Value /= 0 else Value > F.L.Mask (Entry_Data.Geometry.Width))
      then Status := A.Failed_Completed; end if;
      case Status is
         when A.Completed => Reply := (A.Completed,IO.Token,Value,A.No_Operation);
         when A.Indeterminate =>
            IO.Captured := (True,IO.Token,Item,A.Operation_ID (Outer_Operation));
            Reply := (A.Indeterminate,IO.Token,0,IO.Captured.Operation); IO.Failed := True;
         when A.Failed_Completed =>
            IO.Fault := (True,IO.Token,Item,A.No_Operation);
            Reply := (A.Failed_Completed,IO.Token,0,A.No_Operation); IO.Failed := True;
         when A.Denied => IO.Failed := True;
      end case;
      IO.Working := False;
   end Dispatch;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO,Expected);
   begin Accepted := False; end Reconciled;
   procedure Cleanup (IO : in out Context; Accepted : out Boolean) is
      Deadline : F.IPC.Time_Instant;
   begin
      Accepted := False; if IO.Working then return; end if;
      IO.Failed := True; if IO.Depth /= 0 then return; end if;
      IO.Working := True; Cleanup_Deadline (Deadline); Stop_And_Close (IO.Inner,Deadline,Accepted);
      IO.Working := False;
   end Cleanup;
end ACPI_Firmware_Data_Provider;
